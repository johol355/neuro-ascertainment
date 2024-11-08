# neuro-ascertainment

**Version 0.2.0 (2024/11/08)**
This version includes updated features in hospital-admission-concept that are not yet implemented in all queries:
* Calculation of "Days alive and out of hospital" at 90 and 180 days
* Hospital length of stay
* An ID for "Coherent hospital stays", i.e. in the case of multiple PAR admissions immediately following each other, as to reflect hospital stays rather than only stays in a certain clinic as per "HADM_ID". *CAUTION! The variable hospital_los_from_ICU_admit needs more testing, it could be prone to errors when PAR admissions are overlapping.*

The concepts of DAOH, hospital LOS and from now on also days alive and death at 7/30/90/180/365 days are all calculated relative to an ICU admission. In other queries the PAR admission was used. Counting from the SIR admission is more relevant the evaluation of ICU care.

Buyer beware! All new functions require more testing/sanity checks.