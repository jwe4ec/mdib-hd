# ---------------------------------------------------------------------------- #
# Compute Test-Retest Reliability and External Validity
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

pkgs <- c("cocor", "rockchalk")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define functions used in script ----
# ---------------------------------------------------------------------------- #

# Define function to compute Pearson product-moment correlation between one
# variable ("x_var") and one or more others ("y_vars") from wide data frame

cor_test_one_to_many <- function(dat, x_var, y_vars, metric) {
  r     <- vector("double", length(y_vars))
  t     <- vector("double", length(y_vars))
  df    <- vector("double", length(y_vars))
  p     <- vector("double", length(y_vars))
  ci_ll <- vector("double", length(y_vars))
  ci_ul <- vector("double", length(y_vars))
  n     <- vector("double", length(y_vars))
  
  for (i in 1:length(y_vars)) {
    cor_test <- cor.test(dat[, x_var],
                         dat[, y_vars[i]],
                         type = "pearson")
    
    r[i]     <- round(cor_test$estimate, 2)
    t[i]     <- round(cor_test$statistic, 2)
    df[i]    <- cor_test$parameter
    p[i]     <- round(cor_test$p.value, 3)
    ci_ll[i] <- round(cor_test$conf.int[1], 2)
    ci_ul[i] <- round(cor_test$conf.int[2], 2)
    
    n[i]     <- sum(complete.cases(dat[c(x_var, y_vars[i])]))
  }
  
  res <- data.frame(metric, x_var, y_var = y_vars, 
                    r, t, df, p, ci_ll, ci_ul, n)
  
  return(res)
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat2.RData")

# ---------------------------------------------------------------------------- #
# Create time point variable, restrict columns, convert to wide format ----
# ---------------------------------------------------------------------------- #

# Create time point variable

mdib_hd_dat2$wave <- NA
mdib_hd_dat2$wave[mdib_hd_dat2$redcap_event_name == "baseline_arm_1"] <- "baseline"
mdib_hd_dat2$wave[mdib_hd_dat2$redcap_event_name == "followup_arm_1"] <- "followup"

# Restrict to columns of interest

anlys_cols <- c("mdib_neg_9_int_m", "mdib_neg_9_ext_m", "bbsiq_neg_int_m", "bbsiq_neg_ext_m", 
                "asi_m", "asi_red_phy_m", "asi_red_cog_m", "asi_red_soc_m", 
                "bfne2_8_m", "neuroqol_anx_m", "sads_m", "sads_red_m", "auditc_m")

target_cols <- c("record_id", "wave", anlys_cols)

mdib_hd_dat2_short <- mdib_hd_dat2[, target_cols]

# Convert to wide format

mdib_hd_dat2_short_wide <- reshape(mdib_hd_dat2_short, direction = "wide",
                                   idvar = "record_id", timevar = "wave",
                                   v.names = anlys_cols)

# Remove columns not assessed at certain time points

rm_cols <- c("sads_red_m.baseline", 
             "asi_m.followup", 
             "asi_red_phy_m.followup", "asi_red_cog_m.followup", "asi_red_soc_m.followup", 
             "bfne2_8_m.followup", "sads_m.followup", "auditc_m.followup")

mdib_hd_dat2_short_wide <- mdib_hd_dat2_short_wide[, !(names(mdib_hd_dat2_short_wide) %in% rm_cols)]

# ---------------------------------------------------------------------------- #
# Analyze test-retest reliability ----
# ---------------------------------------------------------------------------- #

# Compute 2-week test-retest reliability correlations for MDIB and BBSIQ scales
# using average item scores and pairwise deletion

metric <- "test_retest"

x_var  <- "mdib_neg_9_int_m.baseline"
y_vars <- "mdib_neg_9_int_m.followup"

mdib_neg_9_int_m_retest <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_9_ext_m.baseline"
y_vars <- "mdib_neg_9_ext_m.followup"

mdib_neg_9_ext_m_retest <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "bbsiq_neg_int_m.baseline"
y_vars <- "bbsiq_neg_int_m.followup"

bbsiq_neg_int_m_retest <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "bbsiq_neg_ext_m.baseline"
y_vars <- "bbsiq_neg_ext_m.followup"

bbsiq_neg_ext_m_retest <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

# Compare dependent, non-overlapping correlations for MDIB versus BBSIQ scales.
# In neither comparison do the correlations significantly differ.

cocor(~mdib_neg_9_int_m.baseline + mdib_neg_9_int_m.followup | 
        bbsiq_neg_int_m.baseline + bbsiq_neg_int_m.followup, 
      mdib_hd_dat2_short_wide, test = "zou2007")

cocor(~mdib_neg_9_ext_m.baseline + mdib_neg_9_ext_m.followup | 
        bbsiq_neg_ext_m.baseline + bbsiq_neg_ext_m.followup, 
      mdib_hd_dat2_short_wide, test = "zou2007")

# ---------------------------------------------------------------------------- #
# Analyze convergent validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations at baseline using average item scores and pairwise deletion

metric <- "convergent"

x_var  <- "mdib_neg_9_int_m.baseline"
y_vars <- c("bbsiq_neg_int_m.baseline",
            "asi_m.baseline", "asi_red_phy_m.baseline", "asi_red_cog_m.baseline")

mdib_neg_9_int_m_conv <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_9_ext_m.baseline"
y_vars <- c("bbsiq_neg_ext_m.baseline", 
            "asi_m.baseline", "asi_red_soc_m.baseline",
            "bfne2_8_m.baseline")

mdib_neg_9_ext_m_conv <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze discriminant validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations at baseline using average item scores and pairwise deletion

metric <- "discriminant"
y_vars <- "auditc_m.baseline"

x_var  <- "mdib_neg_9_int_m.baseline"

mdib_neg_9_int_m_disc <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_9_ext_m.baseline"

mdib_neg_9_ext_m_disc <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze concurrent validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations at baseline using average item scores and pairwise deletion

metric <- "concurrent"

x_var  <- "mdib_neg_9_int_m.baseline"
y_vars <- "neuroqol_anx_m.baseline"

mdib_neg_9_int_m_conc <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_9_ext_m.baseline"
y_vars <- c("neuroqol_anx_m.baseline", "sads_m.baseline")

mdib_neg_9_ext_m_conc <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze predictive validity ----
# ---------------------------------------------------------------------------- #

# Analyze correlations from baseline to follow-up using average item scores
# and pairwise deletion

metric <- "predictive"

x_var  <- "mdib_neg_9_int_m.baseline"
y_vars <- "neuroqol_anx_m.followup"

mdib_neg_9_int_m_pred <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

x_var  <- "mdib_neg_9_ext_m.baseline"
y_vars <- c("neuroqol_anx_m.followup", "sads_red_m.followup")

mdib_neg_9_ext_m_pred <- cor_test_one_to_many(mdib_hd_dat2_short_wide, x_var, y_vars, metric)

# ---------------------------------------------------------------------------- #
# Analyze incremental concurrent validity ----
# ---------------------------------------------------------------------------- #

# TODO (restrict analysis to those with scores for all 3 variables--outcome, BBSIQ, MDIB--in a given analysis)
# TODO (compute zero-order Pearson product-moment correlation between the BBSIQ score and the outcome) = Already done for concurrent validity above
# TODO (semipartial correlation between the MDIB score and the outcome (i.e., after controlling for the BBSIQ score)

lm_conc_neuroqol_mdib_neg_9_int_step1 <- lm(neuroqol_anx_m.baseline ~ bbsiq_neg_int_m.baseline, 
                                            mdib_hd_dat2_short_wide)
lm_conc_neuroqol_mdib_neg_9_int_step2 <- lm(neuroqol_anx_m.baseline ~ bbsiq_neg_int_m.baseline + mdib_neg_9_int_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_conc_neuroqol_mdib_neg_9_ext_step1 <- lm(neuroqol_anx_m.baseline ~ bbsiq_neg_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)
lm_conc_neuroqol_mdib_neg_9_ext_step2 <- lm(neuroqol_anx_m.baseline ~ bbsiq_neg_ext_m.baseline + mdib_neg_9_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_conc_sads_mdib_neg_9_ext_step1     <- lm(sads_m.baseline ~ bbsiq_neg_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)
lm_conc_sads_mdib_neg_9_ext_step2     <- lm(sads_m.baseline ~ bbsiq_neg_ext_m.baseline + mdib_neg_9_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_conc_neuroqol_mdib_neg_9_int_step1_mc <- meanCenter(lm_conc_neuroqol_mdib_neg_9_int_step1, centerOnlyInteractors = FALSE)
lm_conc_neuroqol_mdib_neg_9_int_step2_mc <- meanCenter(lm_conc_neuroqol_mdib_neg_9_int_step2, centerOnlyInteractors = FALSE)
lm_conc_neuroqol_mdib_neg_9_ext_step1_mc <- meanCenter(lm_conc_neuroqol_mdib_neg_9_ext_step1, centerOnlyInteractors = FALSE)
lm_conc_neuroqol_mdib_neg_9_ext_step2_mc <- meanCenter(lm_conc_neuroqol_mdib_neg_9_ext_step2, centerOnlyInteractors = FALSE)
lm_conc_sads_mdib_neg_9_ext_step1_mc     <- meanCenter(lm_conc_sads_mdib_neg_9_ext_step1,     centerOnlyInteractors = FALSE)
lm_conc_sads_mdib_neg_9_ext_step2_mc     <- meanCenter(lm_conc_sads_mdib_neg_9_ext_step2,     centerOnlyInteractors = FALSE)

anova(lm_conc_neuroqol_mdib_neg_9_int_step1_mc, lm_conc_neuroqol_mdib_neg_9_int_step2_mc)
anova(lm_conc_neuroqol_mdib_neg_9_ext_step1_mc, lm_conc_neuroqol_mdib_neg_9_ext_step2_mc)
anova(lm_conc_sads_mdib_neg_9_ext_step1_mc,     lm_conc_sads_mdib_neg_9_ext_step2_mc)





# ---------------------------------------------------------------------------- #
# Analyze incremental predictive validity ----
# ---------------------------------------------------------------------------- #

# TODO (restrict analysis to those with scores for all 3 variables--outcome, BBSIQ, MDIB--in a given analysis)
# TODO (compute zero-order Pearson product-moment correlation between the BBSIQ score and the outcome) = Already done for predictive validity above
# TODO (semipartial correlation between the MDIB score and the outcome (i.e., after controlling for the BBSIQ score)

lm_pred_neuroqol_mdib_neg_9_int_step1 <- lm(neuroqol_anx_m.followup ~ bbsiq_neg_int_m.baseline,
                                            mdib_hd_dat2_short_wide)
lm_pred_neuroqol_mdib_neg_9_int_step2 <- lm(neuroqol_anx_m.followup ~ bbsiq_neg_int_m.baseline + mdib_neg_9_int_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_pred_neuroqol_mdib_neg_9_ext_step1 <- lm(neuroqol_anx_m.followup ~ bbsiq_neg_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)
lm_pred_neuroqol_mdib_neg_9_ext_step2 <- lm(neuroqol_anx_m.followup ~ bbsiq_neg_ext_m.baseline + mdib_neg_9_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_pred_sads_mdib_neg_9_ext_step1     <- lm(sads_red_m.followup ~ bbsiq_neg_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)
lm_pred_sads_mdib_neg_9_ext_step2     <- lm(sads_red_m.followup ~ bbsiq_neg_ext_m.baseline + mdib_neg_9_ext_m.baseline,
                                            mdib_hd_dat2_short_wide)

lm_pred_neuroqol_mdib_neg_9_int_step1_mc <- meanCenter(lm_pred_neuroqol_mdib_neg_9_int_step1, centerOnlyInteractors = FALSE)
lm_pred_neuroqol_mdib_neg_9_int_step2_mc <- meanCenter(lm_pred_neuroqol_mdib_neg_9_int_step2, centerOnlyInteractors = FALSE)
lm_pred_neuroqol_mdib_neg_9_ext_step1_mc <- meanCenter(lm_pred_neuroqol_mdib_neg_9_ext_step1, centerOnlyInteractors = FALSE)
lm_pred_neuroqol_mdib_neg_9_ext_step2_mc <- meanCenter(lm_pred_neuroqol_mdib_neg_9_ext_step2, centerOnlyInteractors = FALSE)
lm_pred_sads_mdib_neg_9_ext_step1_mc     <- meanCenter(lm_pred_sads_mdib_neg_9_ext_step1,     centerOnlyInteractors = FALSE)
lm_pred_sads_mdib_neg_9_ext_step2_mc     <- meanCenter(lm_pred_sads_mdib_neg_9_ext_step2,     centerOnlyInteractors = FALSE)

anova(lm_pred_neuroqol_mdib_neg_9_int_step1_mc, lm_pred_neuroqol_mdib_neg_9_int_step2_mc)
anova(lm_pred_neuroqol_mdib_neg_9_ext_step1_mc, lm_pred_neuroqol_mdib_neg_9_ext_step2_mc)
anova(lm_pred_sads_mdib_neg_9_ext_step1_mc,     lm_pred_sads_mdib_neg_9_ext_step2_mc)




# ---------------------------------------------------------------------------- #
# Write results ----
# ---------------------------------------------------------------------------- #

# TODO




