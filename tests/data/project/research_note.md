# Research note: change in questionnaire scores

This is an illustrative study description for NLSS's synthetic
`golden_dataset.csv` test fixture, not a record of real participants or findings.

The example follows participants at three occasions (`pre_score`, `mid_score`,
`post_score`). `id` identifies a participant and `site` identifies a site.
`group2` distinguishes the labels treatment and control. The sampling and group
assignment mechanisms are not documented by this fixture, so those labels alone
do not establish randomization or permit a causal treatment claim.

The questionnaire items `f1_1` through `f1_4` are illustrative measures; `f1_3_rev`
is named as a reverse-keyed item. The file does not establish whether reversal
has already been applied, the instrument's validity, or a justified scoring rule.
Clarify these points before deriving a composite.

The theoretical motivation is that questionnaire scores may change over repeated
measurement, and that the pattern may differ between groups. Practice effects,
selection and measurement properties offer alternative explanations.

H1: The mean post-measurement score differs from the mean pre-measurement score.
H2: Pre-to-post change differs between the two groups.

These are example hypotheses stated for this synthetic demonstration, not a
preregistration. No direction or treatment benefit is asserted. Any report needs
actual statistical results, appropriate uncertainty, and a distinction between
the study description, observed evidence and interpretation.
