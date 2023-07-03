# ---------------------------------------------------------------------------- #
# Clean Demographic Data and Create Table
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

pkgs <- c("flextable", "officer", "ftExtra")
groundhog.library(pkgs, groundhog_day)

# Set "flextable" package defaults and load "officer" package properties

source("./code/1b_set_flextable_defaults.R")
source("./code/1c_set_officer_properties.R")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat.RData")

# ---------------------------------------------------------------------------- #
# Clean demographic data ----
# ---------------------------------------------------------------------------- #

# Restrict to demographic columns of interest at baseline

index_cols <- c("record_id", "redcap_event_name")

dem_cols <- c("age", paste0("race___", c(1:5, 9, 99)), "race_other", "ethnicity", 
              "country", "relationship_status", "live_alone", "gender", "sex", 
              "employment_status", "education", "cag_repeats",
              paste0("study_awareness___", c(0:8)), "survey_help")

dem_dat <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == "baseline_arm_1", 
                       c(index_cols, dem_cols)]

# Clean age

  # Note: No participants entered "99"

sum(dem_dat$age == 99, na.rm = TRUE) == 0

  # TODO (Was this how pna was coded for age? Note that data dictionary says
  # age was computed from "date_of_birth". Did they choose pna for that?): 
  # Participant 814 has value of 0

dem_dat$record_id[!is.na(dem_dat$age) & dem_dat$age == 0] == 814
# View(mdib_hd_dat[mdib_hd_dat$record_id == 814, ])




# Clean race

  # Note: No values of "other" or "prefer not to answer"

all(dem_dat[, c("race___9", "race___99")] == 0)
sum(!is.na(dem_dat$race_other)) == 0

  # Create a single column "race_coll", collapsing cases of more than once race

dem_dat$race_coll <- NA

race_cols_not_pna <- paste0("race___", c(1:5, 9))

for (i in 1:nrow(dem_dat)) {
  if (rowSums(dem_dat[i, race_cols_not_pna]) <= 1) {
    dem_dat$race_coll[[i]][dem_dat$race___1[[i]]  == 1] <- "American Indian or Alaska Native"
    dem_dat$race_coll[[i]][dem_dat$race___2[[i]]  == 1] <- "Asian"
    dem_dat$race_coll[[i]][dem_dat$race___3[[i]]  == 1] <- "Black or African American"
    dem_dat$race_coll[[i]][dem_dat$race___4[[i]]  == 1] <- "Native Hawaiian or Other Pacific Islander"
    dem_dat$race_coll[[i]][dem_dat$race___5[[i]]  == 1] <- "White"
    dem_dat$race_coll[[i]][dem_dat$race___9[[i]]  == 1] <- "Other"
    dem_dat$race_coll[[i]][dem_dat$race___99[[i]] == 1] <- "Prefer not to answer"
  } else if (rowSums(dem_dat[i, race_cols_not_pna]) > 1) {
    dem_dat$race_coll[[i]] <- "More than one race"
  }
}

  # Reorder levels

dem_dat$race_coll <-
  factor(dem_dat$race_coll,
         levels = c("American Indian or Alaska Native", "Asian", "Black or African American", 
                    "Native Hawaiian or Other Pacific Islander", "White",
                    "More than one race", "Other", "Prefer not to answer"))

# Clean ethnicity

dem_dat$ethnicity <- 
  factor(dem_dat$ethnicity, levels = 1:4,
         labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown", 
                    "Prefer not to answer"))

# Clean relationship status

dem_dat$relationship_status <- 
  factor(dem_dat$relationship_status, levels = c(0:9, 99),
         labels = c("Single", "Single, but casually dating",
                    "Single, but currently engaged to be married",
                    "Single, but currently living with someone in a marriage-like relationship",
                    "Married", "In a domestic or civil union", "Separated", "Divorced", 
                    "Widow/widower", "Other", "Prefer not to answer"))

# Clean living alone

dem_dat$live_alone <-
  factor(dem_dat$live_alone, levels = c(0:1, 99),
         labels = c("No", "Yes", "Prefer not to answer"))

# Clean gender identity

dem_dat$gender <-
  factor(dem_dat$gender, levels = c(0:4, 99),
         labels = c("Female", "Male", "Transgender Female", "Transgender Male",
                    "Other", "Prefer not to answer"))

# Clean sex assigned at birth

dem_dat$sex <-
  factor(dem_dat$sex, levels = c(0:3, 99),
         labels = c("Female", "Male", "Intersex", "Unknown or other", 
                    "Prefer not to answer"))

# Clean employment status

dem_dat$employment_status <-
  factor(dem_dat$employment_status, levels = c(0:8, 99),
         labels = c("Working full-time", "Working part-time", "Unemployed or laid off",
                    "Looking for work", "Homemaker/keeping house or raising children full-time",
                    "Retired", "Student", "Other", "Unknown", "Prefer not to answer"))

# Clean education

dem_dat$education <-
  factor(dem_dat$education, levels = c(1:8, 99),
         labels = c("Elementary School", "Junior High", "High School", "Some College",
                    "Associate's Degree", "Bachelor's Degree", "Master's Degree",
                    "Doctorate/ PhD", "Prefer not to answer"))

# Clean CAG repeats

  # Recode variations of unknown

unknown <- c("Don't know", "I don't know it offhand", "Unknown ")

dem_dat$cag_repeats[dem_dat$cag_repeats %in% unknown] <- "Unknown"

  # Recode approximate responses or responses with extraneous text

dem_dat$cag_repeats[dem_dat$cag_repeats == '"about 40"']   <- 40
dem_dat$cag_repeats[dem_dat$cag_repeats == "47 CAG"]       <- 47

  # TODO (why would it be not applicable to them?): Recode not applicable

dem_dat$cag_repeats[dem_dat$cag_repeats == "N/A"] <- "Not applicable"





  # TODO (ask Jessie about these): Identify values not in [35, 60]

outlying_ids <- dem_dat$record_id[dem_dat$cag_repeats < 35 | dem_dat$cag_repeats > 60]
# View(dem_dat[dem_dat$record_id %in% outlying_ids, c("record_id", "cag_repeats")])




  # Create variable containing only numeric responses (with all others NA)

dem_dat$cag_repeats_numeric <- dem_dat$cag_repeats

dem_dat$cag_repeats_numeric[dem_dat$cag_repeats_numeric %in% 
                              c("Not applicable", "Unknown")] <- NA
dem_dat$cag_repeats_numeric <- as.numeric(dem_dat$cag_repeats_numeric)

# Clean country (though we will not include it in formatted table)

  # Recode variations of United States

united_states <- c("America", "U.S.", "united states", "United States",
                   "United states ", "United States ", "United States of America",
                   "US", "usa", "Usa", "USA", "USA ")

dem_dat$country[dem_dat$country %in% united_states] <- "United States"

  # Note: Participant 114 had responded with a county in the United States. On
  # 7/3/23, Jeremy Eberle manually replaced the value with "United States" and
  # deleted the original file "final HD Aim 1 data_deid_2023-01-09_1525.csv";
  # the new file is "final HD Aim 1 data_deid_2023-01-09_1525_v2.csv".

# Clean study awareness (though we will not include it in formatted table)

  # Create column "study_awareness"

dem_dat$study_awareness <- NA

dem_dat$study_awareness[dem_dat$study_awareness___0 == 1] <- "Huntington's Disease Society of America (HDSA) website"
dem_dat$study_awareness[dem_dat$study_awareness___1 == 1] <- "HDSA Convention"
dem_dat$study_awareness[dem_dat$study_awareness___2 == 1] <- "Huntington's Action Awareness (HAA) email"
dem_dat$study_awareness[dem_dat$study_awareness___3 == 1] <- "HAA Conference"
dem_dat$study_awareness[dem_dat$study_awareness___4 == 1] <- "Huntington Study Group (HSG) email"
dem_dat$study_awareness[dem_dat$study_awareness___5 == 1] <- "HSG family day"
dem_dat$study_awareness[dem_dat$study_awareness___6 == 1] <- "HDSA Support Group"
dem_dat$study_awareness[dem_dat$study_awareness___7 == 1] <- "My HD healthcare provider"
dem_dat$study_awareness[dem_dat$study_awareness___8 == 1] <- "Other"

  # Note: No participants chose more than one method

sum(table(dem_dat$study_awareness)) == length(unique(dem_dat$record_id))

  # Reorder levels

dem_dat$study_awareness <-
  factor(dem_dat$study_awareness,
         levels = c("Huntington's Disease Society of America (HDSA) website", 
                    "HDSA Convention", "Huntington's Action Awareness (HAA) email", 
                    "HAA Conference", "Huntington Study Group (HSG) email",
                    "HSG family day", "HDSA Support Group", "My HD healthcare provider",
                    "Other"))

# Clean survey help (though we will not include it in formatted table)

dem_dat$survey_help <-
  factor(dem_dat$survey_help, levels = c(0:1, 99),
         labels = c("No", "Yes", "Prefer not to answer"))

  # Note: There are 26 values of NA for participants with "record_ids" < 92
  # (maybe item wasn't administered before that)

all(dem_dat$record_id[!is.na(dem_dat$survey_help)]) < 92

# ---------------------------------------------------------------------------- #
# Save cleaned data ----
# ---------------------------------------------------------------------------- #

save(dem_dat, file = "./data/further_clean/dem_dat.RData")

# ---------------------------------------------------------------------------- #
# Create demographics table ----
# ---------------------------------------------------------------------------- #

# Define function to compute descriptives

compute_desc <- function(df) {
  # Compute sample size
  
  n <- data.frame(label = "n",
                  value = length(df$record_id))
  
  # Compute mean and standard deviation for numeric variables
  
  vars <- c("age", "cag_repeats_numeric")
  var_labels <- c("Age", "CAG Repeats")
  unit_labels <- c("Years: M (SD)", "M (SD)")
  
  num_res <- data.frame()
  
  for (i in 1:length(vars)) {
    tmp_res <- rbind(data.frame(label = var_labels[i],
                                value = NA),
                     data.frame(label = unit_labels[i],
                                value = paste0(format(round(mean(df[, vars[i]], na.rm = TRUE), 2),
                                                      nsmall = 2, trim = TRUE), 
                                               " (",
                                               format(round(sd(df[, vars[i]], na.rm = TRUE), 2),
                                                      nsmall = 2, trim = TRUE),
                                               ")")))
    num_res <- rbind(num_res, tmp_res)
  }
  
  # TODO (ask Jessie about such values first): Compute count and percentage "Not 
  # applicable" or "Unknown" for "cag_repeats"
  
  num_res_pna <- data.frame(label = "Prefer not to answer: n (%)",
                            value = paste0(sum(is.na(df$age)),
                                           " (",
                                           format(round(sum(is.na(df$age))/length(df$age), 1),
                                                  nsmall = 1, trim = TRUE),
                                           ")"))
  
  
  
  
  
  # Compute count and percentage for factor variables
  
  vars <- c("gender", "sex", "race_coll", "ethnicity", "education", "employment_status", 
            "relationship_status", "live_alone", "cag_repeats",
            "country", "study_awareness", "survey_help")
  var_labels <- paste0(c("Gender", "Sex Assigned at Birth", "Race", "Ethnicity", 
                         "Education", "Employment Status", "Relationship Status", 
                         "Living Alone", "CAG Repeats", "Country",
                         "Where did you hear about this survey study?",
                         "Did anyone help you complete these surveys?"),
                       ": n (%)")
  
  fct_res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <- table(df[, vars[i]])
    prop_tbl <- prop.table(tbl)*100
    
    tbl_res <- rbind(data.frame(label = var_labels[i],
                                value = NA),
                     data.frame(label = names(tbl),
                                value = paste0(as.numeric(tbl),
                                               " (", 
                                               format(round(as.numeric(prop_tbl), 1),
                                                      nsmall = 1, trim = TRUE),
                                               ")")))
    fct_res <- rbind(fct_res, tbl_res)
  }
  
  # Combine results
  
  res <- rbind(n, num_res, num_res_pna, fct_res)
  
  return(res)
}

# Run function

dem_tbl <- compute_desc(dem_dat)

# Save table to CSV

dem_path <- "./results/demographics/"

dir.create(dem_path, recursive = TRUE)

write.csv(dem_tbl, paste0(dem_path, "dem_tbl.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Format demographics table ----
# ---------------------------------------------------------------------------- #

# "flextable" defaults are set in "set_flextable_defaults.R" above

# Section and text properties are sourced from "set_officer_properties.R" above

# Define function to format demographics table

format_dem_tbl <- function(dem_tbl, gen_note, title) {
  # Format "label" column using Markdown
  
  dem_tbl$label_md <- dem_tbl$label
  
  rows_no_indent <- dem_tbl$label_md == "n" | 
    grepl("\\b(Age|Gender|Sex Assigned at Birth|Race|Ethnicity|Education|Employment Status|Relationship Status|Living Alone|CAG Repeats)\\b",
          dem_tbl$label_md)
  rows_indent <- !rows_no_indent
  
  indent_spaces <- "\\ \\ \\ \\ \\ "
  
  dem_tbl$label_md[rows_indent] <- paste0(indent_spaces, dem_tbl$label_md[rows_indent])
  
  dem_tbl$label_md[dem_tbl$label_md == "n"] <- "*n*"
  dem_tbl$label_md <- gsub("n \\(%\\)", "*n* \\(%\\)", dem_tbl$label_md)
  
  dem_tbl$label_md <- gsub("M \\(SD\\)", "*M* \\(*SD*\\)", dem_tbl$label_md)
  
  dem_tbl <- dem_tbl[c("label_md", names(dem_tbl)[names(dem_tbl) != "label_md"])]
  
  # Define columns
  
  left_align_body_cols <- "label_md"
  
  exclude_cols <- c("label", "country")
  target_cols <- names(dem_tbl)[!(names(dem_tbl) %in% exclude_cols)]
  
  # Create flextable
  
  dem_tbl_ft <- flextable(dem_tbl[, target_cols]) |>
    set_table_properties(align = "left") |>
    
    set_caption(as_paragraph(as_i(title)), word_stylename = "heading 1",
                fp_p = fp_par(padding.left = 0, padding.right = 0),
                align_with_table = FALSE) |>
    
    align(align = "center", part = "header") |>
    align(align = "center", part = "body") |>
    align(j = left_align_body_cols, align = "left", part = "body") |>
    align(align = "left", part = "footer") |>
    
    valign(valign = "bottom", part = "header") |>
    
    set_header_labels(label_md = "Characteristic",
                      value    = "Value") |>

    colformat_md(j = "label_md", part = "body") |>
    
    labelizor(part = "body",
              labels = c("Doctorate/ PhD"    = "Doctorate/PhD",
                         "Working full-time" = "Working full time",
                         "Working part-time" = "Working part time")) |>
    
    add_footer_lines(gen_note) |>
    
    autofit()
}

# TODO: Define general notes

gen_note <- as_paragraph_md("*Note.* INSERT")





# Run function

dem_tbl_ft <- format_dem_tbl(dem_tbl, gen_note, "Sample Characteristics at Baseline")

# ---------------------------------------------------------------------------- #
# Write table to MS Word ----
# ---------------------------------------------------------------------------- #

# Write demographics table (note: "flextable" seems to have a bug in which blank 
# page is at end of doc)

dem_tbl_orientation <- "p"
dem_tbl_number      <- 1

doc <- read_docx()
doc <- body_set_default_section(doc, psect_prop)

doc <- body_add_fpar(doc, fpar(ftext(paste0("Table ", dem_tbl_number),
                                     prop = text_prop_bold)))
doc <- body_add_par(doc, "")

doc <- body_add_flextable(doc, dem_tbl_ft, align = "left")
  
if (dem_tbl_orientation == "p") {
  doc <- body_end_block_section(doc, block_section(psect_prop))
} else if (dem_tbl_orientation == "l") {
  doc <- body_end_block_section(doc, block_section(lsect_prop))
}

print(doc, target = paste0(dem_path, "dem_tbl.docx"))