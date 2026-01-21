####################################
## Evi Analysis
## Frederik Saltre = 15/06/2025
####################################
# To evaluate variation in scores across sampling locations and the potential influence of observer identity, 
# I fitted generalized linear mixed models (GLMMs) using a Gamma distribution with a log link. 
# Prior to modelling, all non-positive score values were adjusted by adding a small constant (+0.01) 
# to satisfy Gamma distribution requirements. A baseline random intercept model was first fitted with Location as 
# a random effect to account for clustering. The intraclass correlation coefficient (ICC) indicated that approximately 34% 
# of the variance in scores was attributable to differences between locations, with a design effect of 27, justifying the 
# use of mixed-effects modeling. We then tested whether observer identity significantly influenced scores by including 
# obs as a fixed effect. The effect size for Observer2 was negligible (exp(β) ≈ 1.01), 
# and model comparison metrics (ΔAIC < 2, Bayes Factor ≈ 0.03) provided strong support for the simpler baseline model. 
# Thus, observer identity did not significantly explain score variability and was excluded from subsequent models.
####################################

# Clear the R environment to ensure a clean workspace
rm(list = ls())

# Load required libraries
library(lme4)         # For mixed-effects modeling
library(performance)  # For model diagnostics and ICC/design effect
library(dplyr)        # For data manipulation
library(flexplot)     # For visualizing model fit


# ──────────────────────────────────────────────
# Wide → Long reshape and save tidy data
# ──────────────────────────────────────────────

# Packages
suppressPackageStartupMessages({
  library(readxl)   # read_excel
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(readr)    # write_csv
})

# ── INPUT ─────────────────────────────────────
in_path <- "C:/Users/ehanninger/OneDrive - Massey University/Desktop/Evis PhD/Chapters/Chapter 3 Flipper development/Data analysis/Pectoral fin data.xlsx"

# Read the sheet "Ashley vs me (R)"
wide_raw <- read_excel(in_path, sheet = "Ashley vs me (R)")

# ── CLEAN & RESHAPE ───────────────────────────
long_tidy <- wide_raw %>%
  mutate(
    ID = str_squish(ID),              # trim stray spaces in IDs
    Location = as.integer(Location)    # ensure Location is integer
  ) %>%
  pivot_longer(
    cols = c(Observer1, Observer2),
    names_to = "obs",
    values_to = "score"
  ) %>%
  arrange(Location, ID, obs) %>%
  mutate(
    obs = as.character(obs),
    score = as.numeric(score)
  )

# ── OUTPUT ────────────────────────────────────
out_path <- "C:/Users/ehanninger/OneDrive - Massey University/Desktop/Evis PhD/Chapters/Chapter 3 Flipper development/Data analysis/evi.data.csv"

# Make sure folder exists
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

# Save to CSV
write_csv(long_tidy, out_path, na = "")

# Quick confirmation
cat("Saved:", out_path, "\n")
cat("Rows:", nrow(long_tidy), "  Columns:", ncol(long_tidy), "\n")
print(head(long_tidy, 10))


dat <- read.csv("C:/Users/ehanninger/OneDrive - Massey University/Desktop/Evis PhD/Chapters/Chapter 3 Flipper development/Data analysis/evi.data.csv",
                header = TRUE, sep = ",", stringsAsFactors = FALSE)


# Convert categorical variables to factors
dat$obs <- as.factor(dat$obs)

##==================================================================================
## STEP #1: BASELINE MODEL - RANDOM INTERCEPT FOR LOCATION (MIXED-EFFECTS ANOVA)
##==================================================================================

# Visualize the distribution and variation of score across locations
hist(dat$score, breaks = 50, main = "Distribution of score", xlab = "scores")
boxplot(score ~ Location, data = dat, main = "Boxplot of score by locations", 
        xlab = "Locations", ylab = "Score")

# Adjust score to ensure positivity for Gamma model (Gamma cannot handle zeros)
dat <- dat %>% mutate(score_shifted = score + 0.01)

# Fit baseline model: random intercept for Location using Gamma GLMM
base <- glmer(score_shifted ~ 1 + (1 | Location), 
              family = Gamma(link = "log"), data = dat)

# Evaluate model: estimates, diagnostics, and visualization
estimates(base)         # Fixed and random effects, ICC, design effect
check_model(base)       # Check residuals, distribution, etc.
visualize(base)         # Visualize fit

# ICC (Intra-Class Correlation): proportion of variance explained by clusters
# ~34% of variance in score is due to differences between locations

# Design Effect: ignoring clustering would inflate precision by a factor of 27
# Justifies the use of mixed-effects modeling

##==================================================================================
## STEP #2: TESTING OBSERVER EFFECT
##==================================================================================

# Fit a model including observer identity as a fixed effect
mod.obs <- glmer(score_shifted ~ obs + (1 | Location), 
                 family = Gamma(link = "log"), data = dat)

# Evaluate model fit and diagnostics
visualize(mod.obs, plot = "model")
check_model(mod.obs)
estimates(mod.obs)
summary(mod.obs)

# Interpretation:
# - Intercept ≈ 1.821 : geometric mean score ≈ exp(1.821) ≈ 6.18
# - Effect of Observer2 = 0.011 : exp(0.011) ≈ 1.01 (≈1.1% increase)
# - Statistically non-significant (p = 0.662) and negligible in magnitude

## MODEL COMPARISON
model.comparison(base, mod.obs)

# Interpretation:
# - Adding observer identity does NOT improve model fit
# - ΔAIC < 2, Bayes Factor ≈ 0.03 → strong support for baseline model
# - Predicted differences are trivial
# - Observer is not a meaningful predictor of score

