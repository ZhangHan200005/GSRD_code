# 01_generate_qc_flags.R
# Purpose:
#   1) check REFID-REF consistency
#   2) create a REFID recoding map
#   3) generate global and site-level 3×IQR QC flags for RS_MASS25
#   4) export an updated dataset and QC reports
#
# Expected input:
#   data/GSRD_main.csv
#
# Outputs:
#   outputs/qc/REFID_REF_inconsistencies.csv
#   outputs/qc/REFID_recode_mapping.csv
#   outputs/qc/GSRD_main_with_qc.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

input_file <- "data/GSRD_main.csv"
output_dir <- "outputs/qc"

digits_site <- 3
min_n_site  <- 10

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

gsrd <- readr::read_csv(input_file, show_col_types = FALSE)

required_cols <- c("DATAID", "REFID", "REF", "LAT", "LON", "RS_MASS25")
missing_cols <- setdiff(required_cols, names(gsrd))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

gsrd <- gsrd %>%
  mutate(
    REFID = as.character(REFID),
    REF   = as.character(REF),
    LAT   = as.numeric(LAT),
    LON   = as.numeric(LON),
    RS_MASS25 = as.numeric(RS_MASS25)
  )

# -------------------------
# 1) REFID -> REF diagnostic
# -------------------------
gsrd <- gsrd %>%
  mutate(
    REF_clean = ifelse(is.na(REF), "", REF),
    REF_clean = trimws(REF_clean),
    REF_clean = gsub("\\s+", " ", REF_clean)
  )

refid_issue <- gsrd %>%
  filter(!is.na(REFID), REFID != "") %>%
  group_by(REFID) %>%
  summarise(
    n_ref_strings = n_distinct(REF_clean),
    ref_examples = paste(unique(REF_clean), collapse = " || "),
    .groups = "drop"
  ) %>%
  filter(n_ref_strings > 1) %>%
  arrange(desc(n_ref_strings), REFID)

readr::write_csv(refid_issue, file.path(output_dir, "REFID_REF_inconsistencies.csv"))

# -------------------------
# 2) REFID recoding map
# -------------------------
gsrd <- gsrd %>%
  mutate(
    REFID_KEY = case_when(
      !is.na(REFID) & REFID != "" ~ REFID,
      REF_clean != "" ~ paste0("MISSINGREFID_", sprintf("%04d", dense_rank(REF_clean))),
      TRUE ~ paste0("MISSINGREFID_DATAID_", DATAID)
    )
  )

refid_canonical <- gsrd %>%
  group_by(REFID_KEY, REF_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(REFID_KEY, desc(n), REF_clean) %>%
  group_by(REFID_KEY) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    REF_clean_for_order = if_else(
      REF_clean == "",
      paste0("REFIDKEY_", REFID_KEY),
      REF_clean
    )
  )

ref_map <- refid_canonical %>%
  mutate(
    is_try = grepl("TRY", REFID_KEY, ignore.case = TRUE),
    is_text_nontry = !is_try & grepl("[A-Za-z]", REFID_KEY)
  ) %>%
  arrange(REF_clean_for_order) %>%
  mutate(
    REFID_NEW = row_number(),
    TRY_SEQ = if_else(is_try, cumsum(is_try), NA_integer_),
    REFID_NEW_LABEL = case_when(
      is_try ~ paste0("TRY", TRY_SEQ),
      is_text_nontry ~ REFID_KEY,
      TRUE ~ as.character(REFID_NEW)
    )
  ) %>%
  select(REFID_KEY, REFID_NEW, REFID_NEW_LABEL)

gsrd <- gsrd %>%
  left_join(ref_map, by = "REFID_KEY")

if (any(is.na(gsrd$REFID_NEW))) {
  warning("Some records have NA REFID_NEW. Please check REFID/REF columns.")
}

readr::write_csv(ref_map, file.path(output_dir, "REFID_recode_mapping.csv"))

# -------------------------
# 3) QC flags for RS_MASS25
# -------------------------
gsrd <- gsrd %>%
  mutate(
    LAT_r = round(LAT, digits_site),
    LON_r = round(LON, digits_site),
    SITE_ID = if_else(
      is.na(LAT_r) | is.na(LON_r),
      NA_character_,
      paste0(
        "lat", formatC(LAT_r, format = "f", digits = digits_site),
        "_lon", formatC(LON_r, format = "f", digits = digits_site)
      )
    )
  )

x <- gsrd$RS_MASS25
iqr_g  <- IQR(x, na.rm = TRUE)
q1_g   <- quantile(x, 0.25, na.rm = TRUE)
q3_g   <- quantile(x, 0.75, na.rm = TRUE)
low_g  <- as.numeric(q1_g - 3 * iqr_g)
high_g <- as.numeric(q3_g + 3 * iqr_g)

flag_global <- ifelse(
  is.na(x),
  NA_integer_,
  as.integer(x < low_g | x > high_g)
)

site_thr <- gsrd %>%
  filter(!is.na(SITE_ID)) %>%
  group_by(SITE_ID) %>%
  summarise(
    n_site = sum(!is.na(RS_MASS25)),
    q1_s   = if (n_site >= min_n_site) quantile(RS_MASS25, 0.25, na.rm = TRUE) else NA_real_,
    q3_s   = if (n_site >= min_n_site) quantile(RS_MASS25, 0.75, na.rm = TRUE) else NA_real_,
    iqr_s  = if (n_site >= min_n_site) IQR(RS_MASS25, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    low_s  = q1_s - 3 * iqr_s,
    high_s = q3_s + 3 * iqr_s
  )

gsrd <- gsrd %>%
  left_join(site_thr %>% select(SITE_ID, n_site, low_s, high_s), by = "SITE_ID") %>%
  mutate(
    thr_low_used = case_when(
      is.na(RS_MASS25) ~ NA_real_,
      !is.na(n_site) & n_site >= min_n_site ~ low_s,
      TRUE ~ low_g
    ),
    thr_high_used = case_when(
      is.na(RS_MASS25) ~ NA_real_,
      !is.na(n_site) & n_site >= min_n_site ~ high_s,
      TRUE ~ high_g
    ),
    QC_FLAG_RS_MASS25_GLOBAL_IQR3 = flag_global,
    QC_FLAG_RS_MASS25_SITE_IQR3 = case_when(
      is.na(RS_MASS25) ~ NA_integer_,
      TRUE ~ as.integer(RS_MASS25 < thr_low_used | RS_MASS25 > thr_high_used)
    )
  )

gsrd_out <- gsrd %>%
  select(
    -REF_clean, -REFID_KEY,
    -LAT_r, -LON_r, -SITE_ID,
    -n_site, -low_s, -high_s, -thr_low_used, -thr_high_used
  )

readr::write_csv(gsrd_out, file.path(output_dir, "GSRD_main_with_qc.csv"))

message("QC outputs written to: ", normalizePath(output_dir))
