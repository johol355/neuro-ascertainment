-- Processes the cause of death register and produces a CTE with three columns:
--  - LopNr
--  - DODSDAT_CLEAN is a column of strings with the date of death, some of the 
--      dates are simply years (YYYY) or months (YYYYMM) but all ambiguous formats
--      such as YYYYMM00 or YYYY0000 have been removed.
--  - DODSDAT_DATE is a column of dates for all patients in whom there is a date of 
--  -   death.

PROCESSED_DORS AS(
  SELECT 
    D.LopNr, 
    
    -- Keep the original date as a string but remove trailing zeroes to enable
    -- future parsing as YMD YM and Y
    CASE
      WHEN SUBSTR(D.DODSDAT, -4, 2) = '00' THEN SUBSTR(D.DODSDAT, 1, 4) 
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00' THEN SUBSTR(D.DODSDAT, 1, 6) 
      ELSE D.DODSDAT END AS DODSDAT_CLEAN,
    
    -- Keep correct dates only
    CASE
      WHEN SUBSTR(D.DODSDAT, -2) != '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
      ELSE NULL
    END AS DODSDAT_DATE,
    
    -- Keep all dates but for incorrect dates, round to the 1st day of the observation period
    CASE 
      WHEN SUBSTR(D.DODSDAT, -4, 2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-01-01')
      WHEN SUBSTR(D.DODSDAT, -4, 2) != '00' AND SUBSTR(D.DODSDAT, -2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 6) || '-01')
      ELSE DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
    END AS DODSDAT_ROUND_DOWN,
    
    -- Keep all date but for incorrect dates, round up to the last day of observation period
    CASE 
      WHEN SUBSTR(D.DODSDAT, -4, 4) = '0000'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 4) || '-12-31')
      WHEN SUBSTR(D.DODSDAT, -2) = '00'
      THEN DATE(SUBSTR(D.DODSDAT, 1, 6) || '01', 'start of month', '+1 month', '-1 day')
      ELSE DATE(SUBSTR(D.DODSDAT, 1, 4) || '-' || SUBSTR(D.DODSDAT, 5, 2) || '-' || SUBSTR(D.DODSDAT, 7, 2))
    END AS DODSDAT_ROUND_UP,
    
    -- Add a flag for date with an error in formatting
    CASE WHEN SUBSTR(D.DODSDAT, -2) = '00' THEN 1 ELSE 0 END AS ERROR_DATE
    
  FROM DORS D
  LEFT JOIN DORS_MAN DM ON D.LopNr = DM.LopNr
  LEFT JOIN DORS_AVI DA ON D.LopNr = DA.LopNr
)