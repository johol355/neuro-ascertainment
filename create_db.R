library(tidyverse)
library(RSQLite)
library(DBI)
library(haven)
library(readr)

# Create or connect to SQLite DB
db <- dbConnect(SQLite(), "db_2025.sqlite", extended_types = TRUE)

# Converts all character columns to UTF-8 encoding
convert_to_utf8 <- function(df) {
  df %>%
    mutate(across(where(is.character), ~iconv(.x, from = "", to = "UTF-8")))
}

#Collapse op and dia colums
collapse_diagnoses_and_ops <- function(df) {
  # Auto-detect DIA and OP columns
  dia_cols <- names(df)[str_detect(names(df), regex("^DIA\\d+$", ignore_case = TRUE))]
  op_cols  <- names(df)[str_detect(names(df), regex("^OP\\d+$", ignore_case = TRUE))]
  
  # Collapse DIA columns
  if (length(dia_cols) > 0) {
    df <- df %>%
      mutate(Diagnos = pmap_chr(select(., all_of(dia_cols)), ~paste(na.omit(c(...)), collapse = " "))) %>%
      select(-all_of(dia_cols))
  }
  
  # Collapse OP columns
  if (length(op_cols) > 0) {
    df <- df %>%
      mutate(Op = pmap_chr(select(., all_of(op_cols)), ~paste(na.omit(c(...)), collapse = " "))) %>%
      select(-all_of(op_cols))
  }
  
  return(df)
}


# Coerce columns in df2 to match types of df1
coerce_column_types <- function(df1, df2) {
  common_cols <- intersect(names(df1), names(df2))
  for (col in common_cols) {
    target_type <- typeof(df1[[col]])
    current_type <- typeof(df2[[col]])
    if (target_type != current_type) {
      df2[[col]] <- switch(
        target_type,
        "double" = as.double(df2[[col]]),
        "integer" = as.integer(df2[[col]]),
        "character" = as.character(df2[[col]]),
        "logical" = as.logical(df2[[col]]),
        df2[[col]]  # fallback: no coercion
      )
    }
  }
  return(df2)
}

# --- Load LISA datasets ---
wd <- getwd()
setwd("/Users/JO/PhD/datauttag/2022/SCB_Stata")
lisa <- list.files(pattern = 'LH_Lev_LISA*') %>%
  map(~read_dta(.)) %>%
  set_names(list.files(pattern = 'LH_Lev_LISA*') %>%
              map_chr(~str_sub(., -8, -5)))
lisa$grunduppgifter <- read_dta('LH_Lev_Grunduppgifter.dta')
setwd(wd)

# Write LISA tables
walk2(names(lisa), lisa, ~{
  dbWriteTable(db, paste0("LISA_", .x), .y, overwrite = TRUE)
})

# --- Load Socialstyrelsen datasets ---
setwd("/Users/JO/PhD/datauttag/2025")

# Helper function to rename specific columns
standardize_par_columns <- function(df) {
  names(df) <- case_when(
    tolower(names(df)) == "lopnr"     ~ "LopNr",
    tolower(names(df)) == "utdatuma"  ~ "UTDATUMA",
    tolower(names(df)) == "diagnos"   ~ "DIAGNOS",
    TRUE                              ~ names(df)
  )
  df
}

# Read, clean, encode and write PAR
par <- read_sas('ut_r_par_sv_31705_2024.sas7bdat') %>%
  collapse_diagnoses_and_ops() %>%
  convert_to_utf8() %>%
  standardize_par_columns()

dbWriteTable(db, 'PAR', par, overwrite = TRUE)


# Read, clean, encode, uppercase column names and write PAR_OV
par_ov <- read_sas('ut_r_par_ov_31705_2024.sas7bdat') %>%
  collapse_diagnoses_and_ops() %>%
  convert_to_utf8()

dbWriteTable(db, 'PAR_OV', par_ov, overwrite = TRUE)

# Read the other tables
dbWriteTable(db, 'LMED', read_sas('ut_r_lmed_31705_2024.sas7bdat'))
dbWriteTable(db, 'DORS', read_sas('ut_r_dors_31705_2024.sas7bdat'))
dbWriteTable(db, 'DORS_AVI', read_sas('ut_r_dors_avi_31705_2024.sas7bdat'))

# --- Function to read a full SIR folder ---
read_file_with_fallback <- function(file_path) {
  tryCatch({
    read_delim(file_path, delim = "\t", escape_double = FALSE, trim_ws = TRUE,
               locale = locale(decimal_mark = ",", encoding = 'UTF-8'))
  }, error = function(e) {
    message("Fallback to ISO-8859-1 for: ", basename(file_path))
    read_delim(file_path, delim = "\t", escape_double = FALSE, trim_ws = TRUE,
               locale = locale(decimal_mark = ",", encoding = 'ISO-8859-1'))
  })
}

read_sir_folder <- function(path, id_offset = 0, encoding_overrides = list()) {
  files <- list.files(path, pattern = '\\.txt$', full.names = TRUE)
  names <- basename(files) %>% str_remove("\\.txt$")
  
  read_sir_file <- function(file, name) {
    encoding <- encoding_overrides[[name]] %||% "UTF-8"
    read_delim(file,
               delim = "\t",
               escape_double = FALSE,
               trim_ws = TRUE,
               locale = locale(decimal_mark = ",", encoding = encoding))
  }
  
  sir <- map2(files, names, read_sir_file) %>%
    set_names(names)
  
  if (id_offset > 0) {
    sir <- sir %>%
      map(~mutate(., VtfId_LopNr = VtfId_LopNr + id_offset))
  }
  
  return(sir)
}


# --- Load both old and new SIR datasets ---
sir_old <- read_sir_folder(
  "~/PhD/datauttag/2022/SIR_Textfiler",
  encoding_overrides = list(
    Basdata = "ISO-8859-1",
    Saps3 = "UTF-8"  # optional, as it's the default
  )
)

sir_new <- read_sir_folder(
  "~/PhD/datauttag/2025/SIR_Textfiler",
  id_offset = 2000000,
  encoding_overrides = list(
    Basdata = "ISO-8859-1",
    Saps3 = "Windows-1252"
  )
)

# Deduplicate new Basdata
sir_new$Basdata <- anti_join(sir_new$Basdata,
                             sir_old$Basdata,
                             by = c("LopNr", "AnkomstTidpunkt", "AvdNamn"))

# Keep only referenced VtfId_LopNr in new data
valid_ids_new <- sir_new$Basdata$VtfId_LopNr
sir_new <- sir_new %>%
  map(~filter(., VtfId_LopNr %in% valid_ids_new))

# Merge old and cleaned new
sir_combined <- map(names(sir_old), function(name) {
  df1 <- sir_old[[name]]
  df2 <- sir_new[[name]]
  df2 <- coerce_column_types(df1, df2)
  bind_rows(df1, df2)
}) %>% 
  set_names(names(sir_old))

# Write all merged SIR tables to DB with UTF-8 encoding

walk2(names(sir_combined), sir_combined, ~{
  df_clean <- convert_to_utf8(.y)
  table_name <- case_when(
    .x == "AvstaAvbrytaBegransningar" ~ "SIR_BEGRANSNINGAR",
    TRUE ~ paste0("SIR_", toupper(.x))
  )
  dbWriteTable(db, table_name, df_clean, overwrite = TRUE)
})

# SIR SAPS3 is f***ed and reads some vals with incorrect decimal placements.
# Code below fixes this by overwriting errors. 
dbGetQuery(db, "SELECT * FROM SIR_SAPS3") %>%
  as_tibble() %>%
  mutate(SAPS3_pHMin = case_when(between(SAPS3_pHMin, 10, 100) ~ SAPS3_pHMin/10,
                                 between(SAPS3_pHMin, 100, 1000) ~ SAPS3_pHMin/100,
                                 TRUE ~ SAPS3_pHMin/10),
         SAPS3_KroppstempMax = if_else(SAPS3_KroppstempMax>100, SAPS3_KroppstempMax/10, SAPS3_KroppstempMax),
         SAPS3_PaO2 = case_when(between(SAPS3_PaO2, 10, 100) ~ SAPS3_PaO2/10,
                                between(SAPS3_PaO2, 100, 1000) ~ SAPS3_PaO2/100,
                                TRUE ~ SAPS3_PaO2/10)) %>%
  dbWriteTable(db, "SIR_SAPS3", ., overwrite = TRUE)

message("✅ All tables (LISA, Socialstyrelsen, SIR) written to db.sqlite.")

