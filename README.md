# neuro-ascertainment

**Version 0.2.1 (2024/11/09)**
Version 0.2 includes updated features in hospital-admission-concept that are not yet implemented in all queries:
* Calculation of "Days alive and out of hospital" at 90 and 180 days
* Hospital length of stay: two flavors are included to account for the fact that some patients are still in the hospital after being declared dead. HOSPITAL_LOS and HOSPITAL_LOS_ALIVE (only LOS while alive is counted).
* An ID for "Coherent hospital stays", i.e. in the case of multiple PAR admissions immediately following each other, as to reflect hospital stays rather than only stays in a certain clinic as per "HADM_ID". *CAUTION! The variable Hospital LOS (from ICU admission) needs more testing, it could be prone to errors when PAR admissions are overlapping.*

The concepts of DAOH, hospital LOS and from now on also days alive and death at 7/30/90/180/365 days are all calculated relative to an ICU admission. In other queries the PAR admission was used. Counting from the SIR admission is more relevant the evaluation of ICU care.

Issues to adress:
- How does one count LOS? Only full days?
- How does one count days alive?

Currently if the patient is in the hospital any part of the day, it will count as a full day in hospital. The day of death is not counted as a day alive.

Buyer beware! All new functions require more testing/sanity checks.