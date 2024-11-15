# neuro-ascertainment

**Version 0.3.1 (2024/11/15)** Version 0.3 begins to split queries which previously were contained in a single large document into more relevant smaller files.

-   A directory 'sql-queries' is created to contain all sql-code ready for beta-testing.

-   The directory 'candidate-queries' is retained for legacy-purposes and for any future alpha-level development.

-   Within 'sql-queries' there are currently several files:

    -   general_cte.sql can be considered an 'init'-file as this needs to be run first

        -   Contains the initial "WHILE" statement for constructing CTE's
        -   Creates all hospital- and ICU-admission flags and ID's to enable future analysis
        -   Creates relevant diagnosis-groups and Dx-hierarchy

    -   demographics_icu_data.sql -

        -   Combines ICU-data from several icuregswe-registers
        -   Calculates derived values
        -   Adds basic demographics such as age and sex from PAR

    -   hospital_los_and_daoh.sql

        -   Calculates outcomes such as DAOH and hospital LOS

    -   summary_tables.sql

        -   Contains queries for creating various summary tables for use in analysis

    -   build_query.R

        -   An R-script for initializing the local function build_query() which is a wrapper around an apply-function to iterate paste() and read_file() over the .sql-files to build a query that can be sent to an sql-database.

**Version 0.2.1 (2024/11/09)** Version 0.2 includes updated features in hospital-admission-concept that are not yet implemented in all queries: \* Calculation of "Days alive and out of hospital" at 90 and 180 days \* Hospital length of stay: two flavors are included to account for the fact that some patients are still in the hospital after being declared dead. HOSPITAL_LOS and HOSPITAL_LOS_ALIVE (only LOS while alive is counted). \* An ID for "Coherent hospital stays", i.e. in the case of multiple PAR admissions immediately following each other, as to reflect hospital stays rather than only stays in a certain clinic as per "HADM_ID". *CAUTION! The variable Hospital LOS (from ICU admission) needs more testing, it could be prone to errors when PAR admissions are overlapping.*

The concepts of DAOH, hospital LOS and from now on also days alive and death at 7/30/90/180/365 days are all calculated relative to an ICU admission. In other queries the PAR admission was used. Counting from the SIR admission is more relevant the evaluation of ICU care.

Issues to adress: - How does one count LOS? Only full days? - How does one count days alive?

Currently if the patient is in the hospital any part of the day, it will count as a full day in hospital. The day of death is not counted as a day alive.

Buyer beware! All new functions require more testing/sanity checks.
