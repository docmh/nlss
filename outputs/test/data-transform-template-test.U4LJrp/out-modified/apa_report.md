# Data Transformations TEMPLATE TEST

Analysis:

- Calc: bmi=weight/(height^2)
- Transform: score=log
- Standardize: age
- Percentile Bins: weight=4
- Bins: age=30,20,40
- Recode: gender='M':1,'F':0
- Rename: gender_rec:gender_binary
- Drop: height
- Coerce non-numeric: No
- Overwrite Vars: No

**Table 1**

Transformation log TEST.

| Step | Action | Variable | New Variable | Details | Note |
| --- | --- | --- | --- | --- | --- |
| 1 | Calculated | bmi | bmi | weight/(height^2) |  |
| 2 | Transformed | score | log_score | log |  |
| 3 | Standardized | age | age_z | z-score (suffix _z) |  |
| 4 | Recoded | gender | gender_rec | pairs=2 |  |
| 5 | Percentile-binned | weight | weight_pct | bins=4 |  |
| 6 | Binned | age | age_bin | breaks=20,30,40 | sorted breaks |
| 7 | Renamed | gender_rec | gender_binary | rename |  |
| 8 | Dropped | height |  | dropped |  |


*Note-TEST.* Actions: Calculated, Transformed, Standardized, Recoded, Percentile-binned, Binned, Renamed, Dropped. Notes indicate coercion or bin adjustments.

**Narrative TEST**

Data transformations were applied in 8 steps. Derived variables created: bmi. Transformed variables added: log_score. Standardized variables added: age_z. Recoded variables added: gender_rec. Percentile bins created: weight_pct. Custom bins created: age_bin. Variables renamed: gender_rec->gender_binary. Variables dropped: height.

Step 1: Calculated bmi (weight/(height^2)).
Step 2: Transformed score -> log_score (log).
Step 3: Standardized age -> age_z (z-score (suffix _z)).
Step 4: Recoded gender -> gender_rec (pairs=2).
Step 5: Percentile-binned weight -> weight_pct (bins=4).
Step 6: Binned age -> age_bin (breaks=20,30,40). Note: sorted breaks.
Step 7: Renamed gender_rec -> gender_binary.
Step 8: Dropped height.

---