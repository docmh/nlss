#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public legacy SEM diagnostic tests. No NLSS scientific functions.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
if ("--help" %in% args) { cat("Usage: run_sem_diagnostic_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Repeated option"); if (length(i)) args[i+1L] else fallback }
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
required <- c("yaml", "jsonlite", "digest", "arrow", "haven", "lavaan", "MVN")
for (p in required) if (!requireNamespace(p, quietly = TRUE)) stop("Missing test package: ", p)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo,path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (is.na(keep) || keep < 0L) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-sem-diagnostic")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
config <- file.path(work, "config.yml"); yaml::write_yaml(list(defaults = list(digits = 9L)), config)
Sys.setenv(NLSS_CONFIG_PATH = config)
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
numeric_checks <- 0L
near <- function(actual, expected, label, probability = FALSE, tolerance = 2e-7) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing mask differs"))
  use <- !is.na(expected); scale <- if (probability) pmax(1e-300, abs(expected[use])) else pmax(1, abs(expected[use]))
  check(all(is.finite(actual[use])) && all(abs(actual[use]-expected[use]) <= tolerance*scale), paste(label, "differs"))
  numeric_checks <<- numeric_checks + length(expected)
}
results <- list()
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  old <- getwd(); on.exit(setwd(old)); before <- numeric_checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results)+1L]] <<- list(test = name, passed = is.null(error), numeric_checks = numeric_checks-before,
    seconds = unname(proc.time()[["elapsed"]]-start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ",error), "\n",sep="")
}
sample <- lavaan::HolzingerSwineford1939
model <- "visual =~ x1+x2+x3; textual =~ x4+x5+x6; speed =~ x7+x8+x9"
new_case <- function(name, data = sample, sav = FALSE) {
  base <- file.path(work,"cases",name); project <- file.path(base,"project"); dir.create(project, recursive=TRUE)
  yaml::write_yaml(list(version=1L,datasets=list()),file.path(project,"nlss-workspace.yml"))
  file <- file.path(base, if (sav) "sample.sav" else "sample.rds")
  if (sav) haven::write_sav(data,file) else saveRDS(data,file)
  list(base=base,project=project,input=file,flag=if (sav) "--sav" else "--rds")
}
run <- function(context, options = list(), failure = FALSE) {
  resolved <- modifyList(list(analysis="cfa", model=model, normality="none", mardia=FALSE, mahalanobis=FALSE,
    collinearity=FALSE, convergence=TRUE, heywood=TRUE, estimator="ML", missing="listwise", se="standard", seed=401L), options)
  argv <- c(context$flag,context$input, unlist(Map(function(k,v) c(paste0("--",k),as.character(v)), names(resolved),resolved),use.names=FALSE))
  old <- getwd(); on.exit(setwd(old)); setwd(context$project)
  logfile <- tempfile("cli-",context$base,fileext=".log")
  status <- system2(file.path(R.home("bin"),"Rscript"),c(shQuote(file.path(repo,"scripts/R/assumptions.R")),shQuote(argv)),stdout=logfile,stderr=logfile)
  if (failure) { check(status != 0L,"Invalid request succeeded"); return(invisible(status)) }
  check(status==0L,paste("CLI failed",read_text(logfile)))
  log <- file.path(context$project,"sample/analysis_log.jsonl")
  entries <- lapply(readLines(log,warn=FALSE),jsonlite::fromJSON)
  entries <- Filter(function(x) identical(x$module,"assumptions"),entries)
  check(length(entries)>0L,"No diagnostic entry")
  entry <- tail(entries,1L)[[1L]]; rows <- entry$results$checks_df; meta <- entry$results$diagnostics
  check(is.data.frame(rows) && all(rows$status %in% c("available","unavailable","skipped")),"Missing diagnostic statuses")
  check(is.list(meta) && is.list(meta$requested),"Missing diagnostics metadata")
  list(rows=rows,meta=meta,entry=entry,markdown=read_text(file.path(context$project,"sample/report_canonical.md")))
}
row <- function(actual, name) { x <- actual$rows[actual$rows$test==name,,drop=FALSE]; check(nrow(x)==1L,paste("Expected single",name)); x }

test("sem_diagnostic_mardia_raw_probabilities_smoke", {
  context <- new_case("mardia"); actual <- run(context,list(mardia=TRUE,convergence=FALSE,heywood=FALSE))
  expected <- MVN::mardia(sample[paste0("x",1:9)],use_population=TRUE,bootstrap=FALSE)
  a <- actual$rows[actual$rows$assumption=="Multivariate normality",]
  near(a$statistic,expected$Statistic,"Mardia statistics"); near(a$p,expected$p.value,"Mardia raw p",TRUE)
  check(all(a$status=="available") && any(a$p<1e-10),"Mardia raw small p missing")
  check(grepl("Mardia",actual$markdown) && !grepl("Mardia test attempted but results were unavailable",actual$markdown),"Mardia Markdown contradicts values")
})
test("sem_diagnostic_independent_complete_screens_smoke", {
  data <- sample; data$x1[3] <- NA; data$x9[8] <- NA
  actual <- run(new_case("screens",data),list(normality="shapiro",mahalanobis=TRUE,collinearity=TRUE))
  x <- data[paste0("x",1:9)]; used <- which(complete.cases(x)); x <- x[used,]
  distances <- stats::mahalanobis(x,colMeans(x),stats::cov(x))
  near(actual$meta$screening$complete_source_rows,used,"Screen source rows",tolerance=0)
  near(actual$meta$mahalanobis$distances,distances,"Mahalanobis distances")
  near(actual$meta$mahalanobis$source_rows,used,"Distance source rows",tolerance=0)
  near(row(actual,"Mahalanobis distance")$value,max(distances),"Maximum distance")
  near(row(actual,"Max |r|")$value,max(abs(cor(x)[upper.tri(cor(x))])),"Max correlation")
  near(row(actual,"Condition number")$value,kappa(cor(x)),"Condition number")
  sw <- actual$rows[actual$rows$test=="Shapiro-Wilk",]
  for (i in seq_len(nrow(sw))) {
    values <- data[[sw$target[i]]]; expected <- shapiro.test(values)
    near(sw$statistic[i],unname(expected$statistic),"Shapiro statistic"); near(sw$p[i],expected$p.value,"Shapiro p",TRUE)
  }
})
test("sem_diagnostic_fiml_actual_case_group_identity_smoke", {
  data <- sample[c(201:301,1:200),]; data$x1[c(2,13)] <- NA; data[7,paste0("x",1:9)] <- NA; data$school[17] <- NA
  actual <- run(new_case("fiml-groups",data),list(group="school",missing="fiml",estimator="MLR",se="robust"))
  fit <- suppressWarnings(lavaan::cfa(model,data=data,group="school",missing="fiml",estimator="MLR",se="robust"))
  expected <- lavaan::lavInspect(fit,"case.idx"); labels <- lavaan::lavInspect(fit,"group.label")
  check(identical(actual$meta$fit$case_selection$group_order,labels),"Fit group labels differ")
  for (i in seq_along(expected)) near(actual$meta$fit$case_selection$groups$source_rows[[i]],expected[[i]],"Fit group cases",tolerance=0)
  check(any(!complete.cases(data[paste0("x",1:9)])[actual$meta$fit$case_selection$included_source_rows]),"FIML case selection silently listwise")
  check(actual$meta$fit$inference$effective$missing==lavaan::lavInspect(fit,"options")$missing,"Effective missing method differs")
})
test("sem_diagnostic_nearby_numeric_group_ids_stay_distinct", {
  data <- sample; data$school <- ifelse(data$school==levels(data$school)[1],1,1+1e-15)
  actual <- run(new_case("nearby-group-ids",data),list(group="school"))
  code <- ifelse(is.na(data$school),NA_character_,sprintf("%.17g",data$school))
  data$school <- factor(code,levels=unique(code[!is.na(code)]))
  fit <- lavaan::cfa(model,data=data,group="school",estimator="ML",missing="listwise",se="standard")
  check(length(actual$meta$group_levels)==2L && length(unique(actual$meta$group_levels))==2L,"Close numeric groups collapsed")
  check(identical(actual$meta$fit$case_selection$group_order,lavaan::lavInspect(fit,"group.label")),"Close group labels differ")
  expected <- lavaan::lavInspect(fit,"case.idx")
  for (i in seq_along(expected)) near(actual$meta$fit$case_selection$groups$source_rows[[i]],expected[[i]],"Close group source rows",tolerance=0)
})
test("sem_diagnostic_sav_ordinal_order_and_user_missing", {
  data <- sample[paste0("x",1:9)]
  for (name in names(data)) {
    cuts <- unique(quantile(data[[name]],probs=c(0,.34,.67,1)))
    code <- c(1,2,10)[as.integer(cut(data[[name]],cuts,include.lowest=TRUE))]
    code[3] <- 99
    data[[name]] <- haven::labelled_spss(code,labels=c(Zeta=1,Alpha=2,Middle=10,Missing=99),na_values=99,label=paste("Item",name))
  }
  actual <- run(new_case("ordinal-sav",data,TRUE),list(ordered=paste(names(data),collapse=","),estimator="WLSMV",missing="listwise",se="robust"))
  for (name in names(data)) { data[[name]][data[[name]]==99] <- NA; data[[name]] <- ordered(as.numeric(data[[name]])) }
  fit <- lavaan::cfa(model,data=data,ordered=names(data),estimator="WLSMV",missing="listwise",se="robust")
  near(actual$meta$fit$case_selection$included_source_rows,lavaan::lavInspect(fit,"case.idx"),"SAV ordinal cases",tolerance=0)
  check(all(vapply(actual$meta$ordered$levels,function(x) identical(x,c("1","2","10")),logical(1))),"Ordinal code order changed to labels")
  check(identical(actual$meta$labels$variables$x1,"Item x1"),"Variable label lost")
})
test("sem_diagnostic_unavailable_singular_screens", {
  data <- sample; data$x2 <- data$x1
  actual <- run(new_case("singular-screens",data),list(mardia=TRUE,mahalanobis=TRUE,convergence=FALSE,heywood=FALSE))
  check(all(actual$rows$status=="unavailable") && all(is.na(actual$rows$p)),"Singular screening passed or disappeared")
  check(grepl("unavailable",actual$markdown,ignore.case=TRUE),"Unavailable screening hidden in Markdown")
})
test("sem_diagnostic_intrinsic_ordered_factors_preserved", {
  data <- sample[paste0("x",1:9)]
  for (name in names(data)) data[[name]] <- ordered(cut(data[[name]],unique(quantile(data[[name]],c(0,.34,.67,1))),include.lowest=TRUE))
  actual <- run(new_case("intrinsic-ordered",data),list(estimator="WLSMV",se="robust"))
  fit <- lavaan::cfa(model,data=data,estimator="WLSMV",missing="listwise",se="robust")
  check(identical(actual$meta$fit$inference$effective$ordered,lavaan::lavNames(fit,"ov.ord")),"Intrinsic ordinal role lost")
  near(actual$meta$fit$case_selection$nobs,lavaan::lavInspect(fit,"nobs"),"Intrinsic ordered N",tolerance=0)
})
test("sem_diagnostic_normality_limit_and_constant", {
  actual <- run(new_case("shapiro-limit"),list(normality="shapiro",`max-shapiro-n`=10L,convergence=FALSE,heywood=FALSE))
  check(all(actual$rows$status=="skipped") && all(is.na(actual$rows$p)),"Shapiro limit not disclosed")
  data <- sample; data$x1 <- 1
  actual <- run(new_case("constant",data),list(normality="shapiro",convergence=FALSE,heywood=FALSE))
  check(actual$rows$status[actual$rows$target=="x1"]=="unavailable","Constant Shapiro claimed available")
})
test("sem_diagnostic_disabled_checks_absent", {
  actual <- run(new_case("disabled"),list(heywood=FALSE))
  check(nrow(actual$rows)==1L && actual$rows$test=="lavaan","Disabled checks executed")
  check(!actual$meta$requested$mardia && !actual$meta$requested$mahalanobis && is.null(actual$meta$mahalanobis),"Disabled request audit differs")
})
test("sem_diagnostic_negative_variance_is_not_success", {
  set.seed(1L); z <- rnorm(160)
  data <- data.frame(x1=z+.02*rnorm(160),x2=.7*z+.4*rnorm(160),x3=.8*z+.4*rnorm(160))
  syntax <- "f =~ x1+x2+x3"
  actual <- run(new_case("negative-variance",data),list(model=syntax))
  fit <- suppressWarnings(lavaan::cfa(syntax,data=data,estimator="ML",missing="listwise",se="standard"))
  pe <- lavaan::parameterEstimates(fit,standardized=TRUE)
  expected <- sum(pe$est[pe$op=="~~" & pe$lhs==pe$rhs]<0)
  check(expected>0 && isTRUE(lavaan::lavInspect(fit,"converged")),"Fixture did not produce converged negative variance")
  near(row(actual,"Negative variances")$value,expected,"Negative variance count",tolerance=0)
  check(row(actual,"Negative variances")$decision!="ok" && !actual$meta$fit$fit_status$admissible,"Inadmissible solution called admissible")
})
test("sem_diagnostic_seeded_bootstrap_matches_lavaan", {
  data <- sample[paste0("x",1:3)]; syntax <- "x2 ~ a*x1; x3 ~ b*x2 + cp*x1; indirect := a*b"
  actual <- run(new_case("bootstrap",data),list(analysis="path",model=syntax,se="bootstrap",`bootstrap-samples`=31L,seed=912L))
  set.seed(912L); fit <- lavaan::sem(syntax,data=data,estimator="ML",missing="listwise",se="bootstrap",bootstrap=31L)
  draws <- lavaan::lavInspect(fit,"boot")
  near(actual$meta$fit$bootstrap$attempted,nrow(draws),"Bootstrap attempted",tolerance=0)
  near(actual$meta$fit$bootstrap$failed,length(attr(draws,"error.idx")),"Bootstrap failures",tolerance=0)
  # lavaan stores a runif double, whereas set.seed uses its integer truncation.
  # Legacy JSON retains about 15 significant digits, not every binary64 bit.
  near(actual$meta$fit$bootstrap$lavaan_seed,attr(draws,"seed"),"Lavaan bootstrap internal seed",tolerance=5e-15)
  near(trunc(actual$meta$fit$bootstrap$lavaan_seed),trunc(attr(draws,"seed")),"Effective integer bootstrap seed",tolerance=0)
  check(actual$meta$fit$seed==912L,"Diagnostic seed missing")
})
test("sem_diagnostic_invariance_explicit_single_fit_scope", {
  actual <- run(new_case("invariance"),list(analysis="invariance",group="school",`group-equal`="loadings"))
  fit <- lavaan::cfa(model,data=sample,group="school",group.equal="loadings",estimator="ML",missing="listwise",se="standard")
  near(actual$meta$fit$fit_status$n_parameters,lavaan::lavInspect(fit,"npar"),"Constrained fit parameters",tolerance=0)
  check(grepl("no automatic invariance sequence",actual$meta$fit$scope,fixed=TRUE),"Standalone diagnostic scope overstated")
})
test("sem_diagnostic_exact_option_names_do_not_partially_match", {
  context <- new_case("exact-options")
  actual <- run(context,list(se=NULL,estimator="MLR",`bootstrap-samples`=17L))
  check(!actual$meta$fit$bootstrap$enabled,"bootstrap-samples incorrectly enabled bootstrap")
  fit <- lavaan::cfa(model,data=sample,estimator="MLR",missing="listwise",se="robust")
  check(identical(actual$meta$fit$inference$effective$se,lavaan::lavInspect(fit,"options")$se),"seed incorrectly matched se")
})
test("sem_diagnostic_path_loading_screen_inapplicable", {
  actual <- run(new_case("path-no-loadings"),list(analysis="path",model="x2 ~ x1; x3 ~ x2+x1"))
  value <- row(actual,"Std. loading > 1")
  check(value$status=="unavailable" && is.na(value$value) && value$decision=="", "Absent loading screen claimed available zero")
  check(grepl("not applicable",actual$markdown,fixed=TRUE),"Inapplicable path-model loading screen hidden")
})
test("sem_diagnostic_invalid_requests_preserve_prior_reports", {
  context <- new_case("invalid-protection"); run(context)
  files <- file.path(context$project,"sample",c("report_canonical.md","analysis_log.jsonl"))
  before <- vapply(files,function(f) digest::digest(file=f,algo="sha256"),character(1))
  for (options in list(list(estimator="invented"),list(se="invented"),list(ci="invented"),list(seed=-1),list(normality="typo"),
    list(`bootstrap-samples`=0),list(ordered="does_not_exist"),list(group="x1"),list(`group-equal`="loadings"),list(`mahalanobis-alpha`=1))) run(context,options,TRUE)
  check(identical(before,vapply(files,function(f) digest::digest(file=f,algo="sha256"),character(1))),"Invalid request changed prior publication")
})
test("sem_diagnostic_selected_infinity_rejected", {
  data <- sample; data$x1[1] <- Inf; run(new_case("infinity",data),failure=TRUE)
})
test("sem_diagnostic_unused_infinity_ignored", {
  data <- sample; data$unused <- Inf; run(new_case("unused-infinity",data))
})

if (!length(results)) stop("No tests selected")
summary <- file.path(work,"results.json")
jsonlite::write_json(list(schema_version=1L,suite="phase2-sem-diagnostic",execution_contract="legacy-assumptions",test_pattern=pattern,
  source_sha256=list(assumptions=digest::digest(file=file.path(repo,"scripts/R/assumptions.R"),algo="sha256"),
    helper=digest::digest(file=file.path(repo,"scripts/R/lib/sem_helpers.R"),algo="sha256"),runner=digest::digest(file=script,algo="sha256")),
  numeric_checks=numeric_checks,tests=results,environment=list(r=R.version.string,packages=as.list(vapply(required,function(p) as.character(packageVersion(p)),character(1))))),summary,pretty=TRUE,auto_unbox=TRUE,digits=NA)
passed <- sum(vapply(results,function(x)x$passed,logical(1)))
cat(sprintf("Phase 2 SEM diagnostics: %d/%d cases passed; %d numeric comparisons. Results: %s\n",passed,length(results),numeric_checks,summary))
# Only prune this runner's own generated directories when retention is explicitly positive.
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection,recursive=FALSE,full.names=TRUE),decreasing=TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$",basename(dirs))]
  if (length(dirs)>keep) for (path in setdiff(tail(dirs,-keep),work)) unlink(path,recursive=TRUE)
}
if (passed!=length(results)) quit(status=1)
