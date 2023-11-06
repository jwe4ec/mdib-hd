# ---------------------------------------------------------------------------- #
# Run Exploratory Factor Analysis
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# TODO: For reference on EFA, see "twincogFA.R" and "PHysCompFA.R" from 11/4/2019
# multivariate class with Steve Boker





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

pkgs <- c("psych", "lavaan")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define functions used in script ----
# ---------------------------------------------------------------------------- #

# Define function to export basic EFA results and details to TXT and loadings to CSV

export_efa_res <- function(fit, path, filename_stem) {
  sink(paste0(path, paste0(filename_stem, ".txt")))
  print(summary(fit))
  sink()
  
  sink(paste0(path, paste0(filename_stem, "_detail.txt")))
  print(summary(fit, se = TRUE, zstat = TRUE, pvalue = TRUE))
  sink()
  
  sink(paste0(path, paste0(filename_stem, ".csv")))
  print(fit$loadings)
  sink()
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat2.RData")
load("./data/helper/mdib_dat_items.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Restrict to baseline and MDIB columns

mdib_items <- c(mdib_dat_items$mdib_ben, mdib_dat_items$mdib_neg)

mdib_bl <- mdib_hd_dat2[mdib_hd_dat2$redcap_event_name == "baseline_arm_1", mdib_items]

# Rename columns to reflect benign/negative, internal/external threat, scenario, and item number

all(mdib_items ==
  c("md_bbsiq_1a_benign",  "md_bbsiq_1c_benign",  "md_bbsiq_2b_benign",  "md_bbsiq_2c_benign",
    "md_bbsiq_3a_benign",  "md_bbsiq_3b_benign",  "md_bbsiq_4a_benign",  "md_bbsiq_4b_benign",
    "md_bbsiq_5b_benign",  "md_bbsiq_5c_benign",  "md_bbsiq_6a_benign",  "md_bbsiq_6c_benign",
    "md_bbsiq_7b_benign",  "md_bbsiq_7c_benign",  "md_bbsiq_8a_benign",  "md_bbsiq_8c_benign",
    "md_bbsiq_9a_benign",  "md_bbsiq_9b_benign",  "md_bbsiq_10b_benign", "md_bbsiq_10c_benign",
    "md_bbsiq_11a_benign", "md_bbsiq_11b_benign", "md_bbsiq_12a_benign", "md_bbsiq_12c_benign",
    "md_bbsiq_1b_neg",     "md_bbsiq_2a_neg",     "md_bbsiq_3c_neg",     "md_bbsiq_4c_neg",
    "md_bbsiq_5a_neg",     "md_bbsiq_6b_neg",     "md_bbsiq_7a_neg",     "md_bbsiq_8b_neg",
    "md_bbsiq_9c_neg",     "md_bbsiq_10a_neg",    "md_bbsiq_11c_neg",    "md_bbsiq_12b_neg"))

mdib_items_rename <-
  c("mdib_ben_int_remember_1a", "mdib_ben_int_remember_1c", "mdib_ben_ext_server_2b",     "mdib_ben_ext_server_2c",
    "mdib_ben_ext_reminder_3a", "mdib_ben_ext_reminder_3b", "mdib_ben_int_cleaning_4a",   "mdib_ben_int_cleaning_4b",
    "mdib_ben_ext_neighbor_5b", "mdib_ben_ext_neighbor_5c", "mdib_ben_int_email_6a",      "mdib_ben_int_email_6c",
    "mdib_ben_ext_exercise_7b", "mdib_ben_ext_exercise_7c", "mdib_ben_int_medication_8a", "mdib_ben_int_medication_8c",
    "mdib_ben_ext_walk_9a",     "mdib_ben_ext_walk_9b",     "mdib_ben_ext_job_10b",       "mdib_ben_ext_job_10c",
    "mdib_ben_ext_stumble_11a", "mdib_ben_ext_stumble_11b", "mdib_ben_int_cough_12a",     "mdib_ben_int_cough_12c",
    "mdib_neg_int_remember_1b", "mdib_neg_ext_server_2a",   "mdib_neg_ext_reminder_3c",   "mdib_neg_int_cleaning_4c",
    "mdib_neg_ext_neighbor_5a", "mdib_neg_int_email_6b",    "mdib_neg_ext_exercise_7a",   "mdib_neg_int_medication_8b",
    "mdib_neg_ext_walk_9c",     "mdib_neg_ext_job_10a",     "mdib_neg_ext_stumble_11c",   "mdib_neg_int_cough_12b")

meaning <-
  c("ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg")

domain <-
  c("int", "int", "ext", "ext",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "ext", "ext",
    "ext", "ext", "int", "int",
    "int", "ext", "ext", "int",
    "ext", "int", "ext", "int",
    "ext", "ext", "ext", "int")

mdib <- data.frame(items        = mdib_items,
                   items_rename = mdib_items_rename,
                   meaning      = meaning,
                   domain       = domain)

names(mdib_bl) <- mdib_items_rename[match(names(mdib_bl), mdib_items)]

# Order columns by meaning and then domain

mdib <- mdib[order(mdib$meaning, mdib$domain), ]

mdib_bl <- mdib_bl[match(mdib$items_rename, names(mdib_bl))]

# ---------------------------------------------------------------------------- #
# Inspect item distributions ----
# ---------------------------------------------------------------------------- #

# Define function to plot histograms for six items at a time

plot_item_hists <- function(df, cols) {
  par(mfrow = c(3, 2))
  
  for (i in cols) {
    col_name <- names(df[i])
    hist(df[, i], main = col_name, xlab = "")
  }
}

# Run function and export plots to PDF

efa_path   <- "./results/efa/"

hists_path <- paste0(efa_path, "hists/")

dir.create(hists_path, recursive = TRUE)

pdf(paste0(hists_path, "mdib_bl_hists.pdf"), height = 6, width = 6)
plot_item_hists(mdib_bl, 1:6)
plot_item_hists(mdib_bl, 7:12)
plot_item_hists(mdib_bl, 13:18)
plot_item_hists(mdib_bl, 19:24)
plot_item_hists(mdib_bl, 25:30)
plot_item_hists(mdib_bl, 31:36)
dev.off()

# Note: Some item distributions are heavily skewed, especially for negative items.
# Thus, use WLSMV estimation in addition to ML estimation with Sattora-Bentler
# scaling (see Finney & DiStefano, 2013, p. 476; Rosellini & Brown, 2021, p. 64).

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on all items ----
# ---------------------------------------------------------------------------- #

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

all_items_path <- paste0(efa_path,       "all_items/")
scree_path     <- paste0(all_items_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound (see 
# Montoya & Edwards, 2021, p. 416) of 4 factors (based on principal axis factoring;
# "PA-PAF-m" in Lim & Jahng, 2019) or 4 components (based on principal component 
# analysis; "PA-PCA-m"). Lim and Jahng (2019) found that PA-PCA-m is better across 
# a wide variety of situations (inc. ordinal data) and recommend that. (Note that 
# "ncomp" suggests 2 components, whereas the eigenvalues of 4 components actually 
# exceed the mean. William Revelle confirmed via email on 7/13/2023 that "fa.parallel"
# defaults to a "quant" argument of .95, meaning the 95% CI is used as the threshold 
# rather than the mean, in contrast to the documentation, which needs to be fixed.)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 2

sum(result_ml$pc.values > result_ml$pc.sim)  == 4
sum(result_ml$pc.values > result_ml$pc.simr) == 4

# Also do parallel analysis of polychoric correlations (given some heavily skewed 
# items--see above). Per personal correspondence on 9/17/2023, William Revelle 
# suggested setting "correct = 0" to avoid error/warnings when setting 'cor = "poly"'
# (though some warnings remain). Unclear what estimation method to use for polychoric 
# correlations, so try both "ML" (as above, but with only 10 iterations as it takes
# long to run) and "minres" (default, incl. for "fa.parallel.poly()"). Results same.

# Two kinds of warnings for "ML":
# 1: In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
# 2: In polychoric(sampledata, correct = correct) :
#    The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 4

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 4
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 3

# Note for "minres":
# In smc, smcs < 0 were set to .0

# Three kinds of warnings for "minres":
# 1: In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
# 2: In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#    The estimated weights for the factor scores are probably incorrect. Try a different 
#    factor score estimation method.
# 3: In polychoric(sampledata, correct = correct) :
#    The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 4

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 4
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 3

# ---------------------------------------------------------------------------- #
# Run EFA based on all items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 2 to 9 factors, but parallel analysis 
# suggests a smaller number (up to 4, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 2, 3, 4, or 5.

# Note: Random seed must be set for each analysis for reproducible results

# Note: For oblimin and geomin, estimator "MLM" gives two warnings per number of 
# factors about the smallest eigenvalue being negative:
#   "The variance-covariance matrix of the estimated parameters (vcov) does not 
#   appear to be positive definite! The smallest eigenvalue (= -8.822709e-16) 
#   is smaller than zero. This may be a symptom that the model is not identified."
# However, the smallest eigenvalue across all warnings is -3.986146e-13, which is
# just barely negative and may simply be due to a machine precision issue (see
# https://groups.google.com/g/lavaan/c/4y5pmqRz4nk/m/PXSq9VEdBwAJ). Adding 
# "check.vcov = FALSE" (see http://127.0.0.1:26718/library/lavaan/html/lavOptions.html) 
# removes the warnings, as does changing the estimator to "ML" (suggesting that
# the issue is tied to the robust standard errors and test statistic of "MLM").

# No Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl, nfactors = 2:5, rotation = "oblimin", 
                       estimator = "MLM", check.vcov = FALSE)
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl, nfactors = 2:5, rotation = "geomin",  
                       estimator = "MLM", check.vcov = FALSE)
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl, nfactors = 2:5, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, all_items_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  all_items_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  all_items_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on all items using WLSMV estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_ord <- as.data.frame(lapply(mdib_bl, factor, levels = 0:4, ordered = TRUE))

# For "mdib_ben_int_email_6a", which no participant rated 0, remove the level 0
# and consider the level 1 to mean "0 or 1." Before doing this, "efa()" yielded
# the error "some categories of variable mdib_ben_int_email_6a' are empty in group 
# 1." For items that contain empty response categories, Yves Rosseel recommends 
# collapsing across two adjacent response categories so categories are no longer
# empty (see https://groups.google.com/g/lavaan/c/ZvIjcQFTRQY/m/SyY9DOzrsNkJ). He
# confirmed via personal communication on 9/17/2023 that response categories should
# be collapsed only for the item with empty response categories (not for all items).

mdib_bl_ord$mdib_ben_int_email_6a <- droplevels(mdib_bl_ord$mdib_ben_int_email_6a)

levels(mdib_bl_ord$mdib_ben_int_email_6a)[levels(mdib_bl_ord$mdib_ben_int_email_6a) == 1] <- "0 or 1"

# Consider 2, 3, 4, or 5 factors, as determined above

# Note: For oblimin and geomin, estimator "WLSMV" gives two warnings per number of 
# factors about the smallest eigenvalue being negative:
#   "The variance-covariance matrix of the estimated parameters (vcov) does not 
#   appear to be positive definite! The smallest eigenvalue (= -7.174704e-15) is 
#   smaller than zero. This may be a symptom that the model is not identified."
# However, the smallest eigenvalue across all warnings is -7.591752e-13, which is
# just barely negative and may simply be due to a machine precision issue (see
# above). Adding "check.vcov = FALSE" (see above) removes the warnings.

# Note: For oblimin, geomin, and promax and 3-5 factors, "WLSMV" estimator yields
# the following warning, and inspection of results shows a Heywood case on item 
# "mdib_neg_int_cough_12b" in which the uniqueness is negative and the commonality 
# is > 1, which is impossible (see Cooperman & Waller, 2022, https://doi.org/gmdnd7).
#   "some estimated ov variances are negative"
# Heywood cases in EFA are more likely in small samples (Cooperman & Waller, 2022).
# We will interpret only the 2-factor solution, which has no Heywood case. Future
# research may consider using regularized common factor analysis, which may avoid
# Heywood cases in small samples (see Study 4 of Cooperman & Waller, 2022, p. 170).

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_ord, nfactors = 2:5, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_ord, nfactors = 2:5, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_ord, nfactors = 2:5, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, all_items_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  all_items_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  all_items_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on only theorized negative bias items ----
# ---------------------------------------------------------------------------- #

# Restrict to 12 theorized negative bias items

mdib_bl_neg <- mdib_bl[, names(mdib_bl[grepl("mdib_neg", names(mdib_bl))])]

length(mdib_bl_neg) == 12

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_path <- paste0(efa_path,       "neg_items/")
scree_path     <- paste0(neg_items_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# Same two kinds of warnings as above for "ML"

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Same note and three kinds of warnings as above for "minres". And a fourth kind of warning:
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on only negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on only negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_ord <- as.data.frame(lapply(mdib_bl_neg, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: For oblimin and geomin, estimator "WLSMV" gives two warnings per number of 
# factors about the smallest eigenvalue being negative:
#   "The variance-covariance matrix of the estimated parameters (vcov) does not 
#   appear to be positive definite! The smallest eigenvalue (= -3.394061e-18) is 
#   smaller than zero. This may be a symptom that the model is not identified."
# However, the smallest eigenvalue across all warnings is -4.639708e-16, which is
# just barely negative and may simply be due to a machine precision issue (see
# above). Adding "check.vcov = FALSE" (see above) removes the warnings.

# Note: No Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 11 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# From 12 theorized negative bias items, consider excluding two items that are
# theorized to be external items but that loaded with internal items. Start by
# excluding "mdib_neg_ext_walk_9c", which loaded more strongly with internal
# items than "mdib_neg_ext_server_2a"

mdib_bl_neg_11 <- mdib_bl_neg[, names(mdib_bl_neg)[names(mdib_bl_neg) != 
                                                     "mdib_neg_ext_walk_9c"]]

length(mdib_bl_neg_11) == 11

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_11))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_11_path <- paste0(efa_path, "neg_items_11/")
scree_path        <- paste0(neg_items_11_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_11, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_11, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_11, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_11, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 11 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_11_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_11_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_11_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 11 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_11_ord <- as.data.frame(lapply(mdib_bl_neg_11, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: For oblimin and geomin, estimator "WLSMV" gives two warnings per number of 
# factors about the smallest eigenvalue being negative:
#   "The variance-covariance matrix of the estimated parameters (vcov) does not 
#   appear to be positive definite! The smallest eigenvalue (= -2.550818e-17) is 
#   smaller than zero. This may be a symptom that the model is not identified."
# However, the smallest eigenvalue across all warnings is -3.757449e-17, which is
# just barely negative and may simply be due to a machine precision issue (see
# above). Adding "check.vcov = FALSE" (see above) removes the warnings.

# Note: No Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_11_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_11_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_11_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 10 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# Now exclude "mdib_neg_ext_server_2a"

mdib_bl_neg_10 <- mdib_bl_neg_11[, names(mdib_bl_neg_11)[names(mdib_bl_neg_11) !=
                                                           "mdib_neg_ext_server_2a"]]

length(mdib_bl_neg_10) == 10

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_10))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_10_path <- paste0(efa_path,       "neg_items_10/")
scree_path        <- paste0(neg_items_10_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_10, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_10, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning as above for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_10, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_10, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 10 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_10_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_10_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_10_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 10 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_10_ord <- as.data.frame(lapply(mdib_bl_neg_10, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV")
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV")
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_10_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_10_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_10_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 9 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# From 10 theorized negative bias items, consider excluding item theorized to be 
# an internal item ("mdib_neg_int_email_6b") but that has moderate cross-loading
# with external items

mdib_bl_neg_9 <- mdib_bl_neg_10[, names(mdib_bl_neg_10)[names(mdib_bl_neg_10) !=
                                                          "mdib_neg_int_email_6b"]]

length(mdib_bl_neg_9) == 9

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_9))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_9_path <- paste0(efa_path,         "neg_items_09/")
scree_path       <- paste0(neg_items_9_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_9, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 
# 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_9, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 2
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning as above for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_9, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_9, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 9 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_9_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_9_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_9_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 9 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_9_ord <- as.data.frame(lapply(mdib_bl_neg_9, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV")
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV")
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_9_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_9_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_9_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Export 9 reduced negative bias items for internal consistency analyses ----
# ---------------------------------------------------------------------------- #

save(mdib_bl_neg_9,     file = "./data/further_clean/mdib_bl_neg_9.Rdata")
save(mdib_bl_neg_9_ord, file = "./data/further_clean/mdib_bl_neg_9_ord.Rdata")