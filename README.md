# Case Ascertainment

## Tables which are suitable for user-end extraction

1.  ICU_ADM_CONT: CTE for verifying LopNr-CONT_ICU_ID-VtfId_LopNr indexing

2.  ICU_ADM_DX: Diagnosis information for continuous ICU-admission

3.  TERT_ICU_ADM_DX: Diagnosis information only for continuous tertiary ICU-admissions

4.  DESCRIPTIVE_SIR: Demographic and physiological variables in all of SIR

5.  CONT_DESCRIPTIVE_SIR: Demographic and physiological variables in SIR aggregated on CONT_ICU_ID (i.e. continuous ICU-admission - both in primary and tertiary centres)

6.  T_CONT_DESCRIPTIVE_SIR: As above but only for continuous admissions to tertiary centres

7.  etc. for beta-testing (see SQL-query scripts).

## User instructions for querying database

**R**

In the directory *sql-queries*, an R-script *build_query.R* exists. This contains the function *build_query()* which can be used to combine the individual *.sql*-files into a single correct query to send to the database.

*build_query()* takes two arguments:

1.  A vector of strings containing the path to each *.sql*-file in the order it should be passed to the database
2.  A single string for the final SELECT-query

*An example of how to use*:

```         
source(".../build_query.R")

build_query(c(".../general_cte.sql", ".../diagnosis.sql"), 
            "SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME;")
```

Which is equivalent to:

```         
source(".../build_query.R")

cte_files <- c(".../general_cte.sql",
               ".../diagnosis.sql")

build_query(cte_files, 
            "SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME;")
```

This could then be passed to a function to query the database:

```         
source(".../build_query.R")

cte_files <- c(".../general_cte.sql",
               ".../diagnosis.sql")
               
query <- build_query(cte_files, 
                     "SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME;")
                     
DBI::dbGetQuery(conn, query)
```

A full example to query the database could be the following:

```         
source(".../build_query.R")

db <- DBI::dbConnect(RSQLite::SQLite(), ".../db.sqlite", extended_types = TRUE)

cte_files <- c(".../general_cte.sql",
               ".../diagnosis.sql")
               
query <- build_query(cte_files, 
                     "SELECT * FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME;")
                     
DBI::dbGetQuery(conn, query)
```

And could be passed as a single command without initializing dependencies (although slightly messy):

```         
source(".../build_query.R")

DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), ".../db.sqlite", extended_types = TRUE), 
                build_query(c(".../general_cte.sql",
                              ".../diagnosis.sql"),
                            "SELECT * 
                             FROM ICU_ADMISSIONS_MATCHED_WITH_PAR_WITH_DX_HIERARCHY_TIME;")
               )
```

**Python**

Ask Johan :)

## Dev-notes

**Version 0.4.2 (2024/11/22)** Update *demographics_icu_data.sql* which now works.

**Version 0.4.2 (2024/11/22)** Remove länssjukhus from diagnosis hierarchy.

**Version 0.4.1 (2024/11/22)** Version 0.4 retains the split of queries previously contained in a single large document into separate files. Note: The other CTE's aside from *general_cte.sql* and *diagnosis.sql* have not been updated yet.

-   general_cte.sql has been updated and now contains CTE's for patients both if only tertiary hospitals are of interest and if all hospitals are included

-   The variable CONT_ICU_ID has been introduced to tag a single continuous ICU-admission spanning across multiple administrative ICU-admission-ID's and even across multiple hospitals

-   The variable T_CONT_ICU_ID is identical to CONT_ICU_ID but only denotes continuous ICU-admissions to a tertiary hospital

-   diagnosis.sql is updated for adding on diagnosis information and has been redesigned to also handle non-tertiary ICU-admissions

    -   Each diagnosis-group has been separated into a t_diagnosis and a diagnosis CTE.

    -   The hierarchical structure of choosing diagnosis to match each admission has been expanded and now consists of:

        -   Level 1: Order by hospital type (i.e. regional \> läns \> länsdel)
        -   Level 2: Within regional, tertiary nsicu-centres are given priority
        -   Level 3: Hierarchy of diagnosis
        -   Level 4: Admission date

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
