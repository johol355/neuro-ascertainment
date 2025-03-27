------------------------------------------------------------------------------
------------------------------------------------------------------------------
---------------------- PAR ÖV CONCEPTS ---------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- This file helps with setting up basic concepts for out-patient clinic
-- visits based on the PAR_OV table.


PAR_OUTPATIENT AS (
    SELECT *,
           ROW_NUMBER() OVER ( 
               ORDER BY LopNr,
                        INDATUM,
                        CASE WHEN SJUKHUS NOT IN ('11001', '11003', '51001', '12001', '21001', '64001', '41001', '41002', '55010') THEN 0 ELSE 1 END
           ) AS OV_ID
    FROM PAR_OV
    WHERE 
        ALDER >= 18
        AND INDATUM >= 14610 -- This is 2010/01/01
),

-- This CTE filters out all planned visits, leaving only emergent visits 
PAR_OUTPATIENT_UNPLANNED AS (
    SELECT *
    FROM PAR_OUTPATIENT
    WHERE PVARD IS NOT 1
)