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

pkgs <- c("cocor", "rockchalk", "r2glmm")
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

# TODO (remove function if not ultimately used): Define function to compute 
# semipartial R-squared estimates and convert to semipartial r

compute_sp_r <- function(model) {
  sp_rsq <- r2beta(model, partial = TRUE, method = "lm")
  sp_rsq <- sp_rsq[sp_rsq$Effect != "Model", ]
  
  sp_r_df <- data.frame(effect          = sp_rsq$Effect,
                        sp_rsq          = sp_rsq$Rsq,
                        sp_rsq_lower.CL = sp_rsq$lower.CL,
                        sp_rsq_upper.CL = sp_rsq$upper.CL,
                        sp_r            = sqrt(sp_rsq$Rsq),
                        sp_r_lower.CL   = sqrt(sp_rsq$lower.CL),
                        sp_r_upper.CL   = sqrt(sp_rsq$upper.CL))
  
  # Maintain predictor order from model (don't reorder in descending R-squared)
  
  sp_r_df <- sp_r_df[match(sp_r_df$effect, attr(model$terms, "term.labels")), ]
  
  sp_r_df <- rbind(c("(Intercept)", rep(NA, length(sp_r_df) - 1)),
                   sp_r_df)
  
  return(sp_r_df)
}





# Define function to run hierarchical multiple regression

run_hmr <- function(dat, outcome, bbsiq_scale, mdib_scale) {
  fml_step0 <- as.formula(paste(outcome, "~", 1))
  fml_step1 <- as.formula(paste(outcome, "~", bbsiq_scale))
  fml_step2 <- as.formula(paste(outcome, "~", bbsiq_scale, "+", mdib_scale))
  
  # Need to use "do.call" so that "fml" is evaluated before being sent to "lm"
  # but quote "dat" so it is not. Otherwise, "meanCenter" lacks the evaluated
  # "fml" call to "lm" and returns an error.
  
  lm_step0 <- do.call("lm", list(fml_step0, data = quote(dat)))
  lm_step1 <- do.call("lm", list(fml_step1, data = quote(dat)))
  lm_step2 <- do.call("lm", list(fml_step2, data = quote(dat)))
  
  lm_step1_mc <- meanCenter(lm_step1, centerOnlyInteractors = FALSE)
  lm_step2_mc <- meanCenter(lm_step2, centerOnlyInteractors = FALSE)
  
  anova_diff <- anova(lm_step1_mc, lm_step2_mc)
  
  summary_lm_step1_mc <- summary(lm_step1_mc)
  summary_lm_step2_mc <- summary(lm_step2_mc)
  
  confint_lm_step1_mc <- as.data.frame(confint(lm_step1_mc))
  confint_lm_step2_mc <- as.data.frame(confint(lm_step2_mc))
  
  # Compute zero-order r between BBSIQ score and outcome (at Step 1)
  
  bbsiq_scale_r_lm_step1 <- sqrt(summary_lm_step1_mc$r.squared)
  
  # Compute semipartial r between MDIB score and outcome (at Step 2)
  
  ss_total <- anova(lm_step0)$`Sum Sq`
  ss_diff  <- anova_diff$`Sum of Sq`[2]
  
  mdib_scale_sp_rsq_lm_step2 <- ss_diff/ss_total
  mdib_scale_sp_r_lm_step2   <- sqrt(mdib_scale_sp_rsq_lm_step2)
  
  # TODO: Consider computing CIs for semipartial r for MDIB score. However, in
  # simple linear regression, CIs from "r2beta()" (even when "method = 'lm'") differ 
  # from those obtained from "cor.test()", which may use a different method to 
  # compute CIs (even though point estimates are the same). Emailed Byron Jaeger 
  # (developer of "r2glmm" package) about this discrepancy on 11/12/23. Also, in
  # multiple linear regression, point estimates from "r2beta()" (even when "method
  # = 'lm'") differ from those computed manually (not yet sure why).
  
  compute_sp_r(lm_step2_mc)
  
  
  
  
  
  # Collect results in list
  
  res <- list(lm_step0                   = lm_step0,
              lm_step1                   = lm_step1,
              lm_step2                   = lm_step2,
              lm_step1_mc                = lm_step1_mc,
              lm_step2_mc                = lm_step2_mc,
              anova_diff                 = anova_diff,
              summary_lm_step1_mc        = summary_lm_step1_mc,
              summary_lm_step2_mc        = summary_lm_step2_mc,
              confint_lm_step1_mc        = confint_lm_step1_mc,
              confint_lm_step2_mc        = confint_lm_step2_mc,
              bbsiq_scale_r_lm_step1     = bbsiq_scale_r_lm_step1,
              ss_total                   = ss_total,
              ss_diff                    = ss_diff,
              mdib_scale_sp_rsq_lm_step2 = mdib_scale_sp_rsq_lm_step2,
              mdib_scale_sp_r_lm_step2   = mdib_scale_sp_r_lm_step2)
  
  return(res)
}

# Define function to create hierarchical multiple regression table

create_hmr_tbl <- function(res, outcome) {
  df_step1 <- cbind(as.data.frame(res$summary_lm_step1_mc$coefficients),
                    res$confint_lm_step1_mc)
  df_step2 <- cbind(as.data.frame(res$summary_lm_step2_mc$coefficients),
                    res$confint_lm_step2_mc)
  
  df_step1$Outcome <- outcome
  df_step2$Outcome <- outcome
  
  df_step1$Model <- "Step 1"
  df_step2$Model <- "Step 2"
  
  df_step1$Effect <- row.names(df_step1)
  df_step2$Effect <- row.names(df_step2)
  
  target_cols <- c("Outcome", "Model", "Effect")
  df_step1 <- df_step1[, c(target_cols, names(df_step1)[!(names(df_step1) %in% target_cols)])]
  df_step2 <- df_step2[, c(target_cols, names(df_step2)[!(names(df_step2) %in% target_cols)])]
  
  df_step1 <- df_step1[row.names(df_step1) != "(Intercept)", ]
  df_step2 <- df_step2[row.names(df_step2) != "(Intercept)", ]
  
  row.names(df_step1) <- 1:nrow(df_step1)
  row.names(df_step2) <- 1:nrow(df_step2)
  
  df_step1$r <- res$bbsiq_scale_r_lm_step1
  df_step2$r <- NA
  
  df_step1$sp_r <- NA
  
  df_step2$sp_r <- NA
  df_step2$sp_r[grepl("mdib", df_step2$Effect)] <- res$mdib_scale_sp_r_lm_step2
  
  df <- rbind(df_step1, df_step2)
  
  # Rename columns
  
  names(df)[names(df) == "Estimate"]   <- "b"
  names(df)[names(df) == "Std. Error"] <- "SE"
  names(df)[names(df) == "t value"]    <- "t"
  names(df)[names(df) == "Pr(>|t|)"]   <- "p"
  
  # Identify significant parameters
  
  df$sig <- NA
  df$sig <- ifelse(df$`2.5 %` > 0 | df$`97.5 %` < 0, 1, 0)
  
  # Round selected columns and ensure two decimal digits are printed
  
  two_digit_vars   <- c("b", "SE", "t", "2.5 %", "97.5 %", "r", "sp_r")
  three_digit_vars <- "p"
  
  df[, two_digit_vars]   <- format(round(df[, two_digit_vars],   2),
                                   nsmall = 2, trim = TRUE)
  df[, three_digit_vars] <- format(round(df[, three_digit_vars], 3),
                                   nsmall = 3, trim = TRUE)
  
  # Format combined b and SE column
  
  df$`b (SE)` <- paste0(df$b, " (", df$SE, ")")
  
  # Format CIs
  
  df$`95% CI` <- paste0("[", df$`2.5 %`, ", ", df$`97.5 %`, "]")
  
  # Remove redundant columns
  
  rm_cols <- c("b", "SE", "2.5 %", "97.5 %")
  
  df <- df[, !(names(df) %in% rm_cols)]
  
  # Rearrange columns
  
  df <- df[, c("Outcome", "Model", "Effect", "b (SE)", "t", "p", "95% CI", "r", "sp_r", "sig")]
  
  return(df)
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

# Export data

save(mdib_hd_dat2_short_wide, file = "./data/further_clean/mdib_hd_dat2_short_wide.RData")

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

# Given no missingness in predictors, each analysis is restricted to participants with
# outcome (i.e., same sample size as concurrent validity analyses above)

predictors <- c("bbsiq_neg_int_m.baseline", "bbsiq_neg_ext_m.baseline",
                "mdib_neg_9_int_m.baseline", "mdib_neg_9_ext_m.baseline")

sum(is.na(mdib_hd_dat2_short_wide[predictors])) == 0

# Run function to analyze incremental concurrent validity

res_conc_neuroqol_mdib_neg_9_int <- run_hmr(mdib_hd_dat2_short_wide,
                                            "neuroqol_anx_m.baseline",
                                            "bbsiq_neg_int_m.baseline", "mdib_neg_9_int_m.baseline")

res_conc_neuroqol_mdib_neg_9_ext <- run_hmr(mdib_hd_dat2_short_wide,
                                            "neuroqol_anx_m.baseline",
                                            "bbsiq_neg_ext_m.baseline", "mdib_neg_9_ext_m.baseline")
res_conc_sads_mdib_neg_9_ext     <- run_hmr(mdib_hd_dat2_short_wide,
                                            "sads_m.baseline",
                                            "bbsiq_neg_ext_m.baseline", "mdib_neg_9_ext_m.baseline")

# Create tables

tbl_conc_neuroqol_mdib_neg_9_int <- create_hmr_tbl(res_conc_neuroqol_mdib_neg_9_int, "neuroqol_anx_m.baseline")

tbl_conc_neuroqol_mdib_neg_9_ext <- create_hmr_tbl(res_conc_neuroqol_mdib_neg_9_ext, "neuroqol_anx_m.baseline")
tbl_conc_sads_mdib_neg_9_ext     <- create_hmr_tbl(res_conc_sads_mdib_neg_9_ext,     "sads_m.baseline")

tbl_conc <- rbind(tbl_conc_neuroqol_mdib_neg_9_int, 
                  tbl_conc_neuroqol_mdib_neg_9_ext, tbl_conc_sads_mdib_neg_9_ext)

# ---------------------------------------------------------------------------- #
# Analyze incremental predictive validity ----
# ---------------------------------------------------------------------------- #

# Given no missingness in predictors (see above), each analysis is restricted to 
# participants with outcome (same sample size as predictive validity analyses above)

# Run function to analyze incremental concurrent validity

res_pred_neuroqol_mdib_neg_9_int <- run_hmr(mdib_hd_dat2_short_wide,
                                            "neuroqol_anx_m.followup",
                                            "bbsiq_neg_int_m.baseline", "mdib_neg_9_int_m.baseline")

res_pred_neuroqol_mdib_neg_9_ext <- run_hmr(mdib_hd_dat2_short_wide,
                                            "neuroqol_anx_m.followup",
                                            "bbsiq_neg_ext_m.baseline", "mdib_neg_9_ext_m.baseline")
res_pred_sads_mdib_neg_9_ext     <- run_hmr(mdib_hd_dat2_short_wide,
                                            "sads_red_m.followup",
                                            "bbsiq_neg_ext_m.baseline", "mdib_neg_9_ext_m.baseline")

# Create tables

tbl_pred_neuroqol_mdib_neg_9_int <- create_hmr_tbl(res_pred_neuroqol_mdib_neg_9_int, "neuroqol_anx_m.followup")

tbl_pred_neuroqol_mdib_neg_9_ext <- create_hmr_tbl(res_pred_neuroqol_mdib_neg_9_ext, "neuroqol_anx_m.followup")
tbl_pred_sads_mdib_neg_9_ext     <- create_hmr_tbl(res_pred_sads_mdib_neg_9_ext,     "sads_red_m.followup")

tbl_pred <- rbind(tbl_pred_neuroqol_mdib_neg_9_int, 
                  tbl_pred_neuroqol_mdib_neg_9_ext, tbl_pred_sads_mdib_neg_9_ext)

# ---------------------------------------------------------------------------- #
# Write results ----
# ---------------------------------------------------------------------------- #

# TODO




