# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 29-10-2025

# RQ: Does fluctuation complexity decrease over time?
# analysis level: sentence
# measure: UIDev

# based on analysis_fluct_comp_20250820_uid_dev.Rmd
# and analysis_fluct_comp.R
# script for running models on server

# libraries
library(dplyr)
library(effects,lib.loc="/scratch/landwehr/R/4.5")
library(estimability,lib.loc="/scratch/landwehr/R/4.5")
library(future,lib.loc="/scratch/landwehr/R/4.5")
library(ggeffects,lib.loc="/scratch/landwehr/R/4.5")
library(ggplot2)
library(glmmTMB)
library(performance)
library(rsample,lib.loc="/scratch/landwehr/R/4.5")

### FIRST STEPS ###

# open data file
#data <- read.table(file="test/test_sent.csv",
data <- read.table(file="sentence_data_no_content.csv",
                   sep=",",
                   header=TRUE,
                   quote='"',
                   fill=TRUE)
head(data)

### PREPROCESS ###

# filter data
sent_data <- data %>%
  filter(sent_len <= 100) %>% # keep observations with sentence length <= 100
  filter(sent_len > 3) %>% # keep observations with sentence length > 3
  filter(year <= 1989) %>% # remove observations after 1989
  filter(!is.na(uid_dev)) %>% # remove observations without uid_dev value
  filter(!is.na(sigma_gamma)) # remove observations without sigma_gamma value

# sample
set.seed(155)
sent_data <- sent_data %>%
  group_by(year) %>%  # stratify by year
  sample_frac(0.5) %>%  # take 50 % from each category
  ungroup()

# transform year variable
sent_data$year_c <- scale(sent_data$year, center=T, scale=F)
# transform sentence length variable
sent_data$sent_len_c <- scale(sent_data$sent_len, center=T, scale=F)
# transform average surprisal variable
sent_data$avg_srp_c <- scale(sent_data$avg_srp, center=T, scale=F)

# transform journal to factor
sent_data$journal_F <- as.factor(sent_data$journal)
levels(sent_data$journal_F) # show factor levels

# redirect output to file
sink("uidev_sent_output.txt", append=T)

# check minimum value of uid dev
print("Minimum value of UIDev:")
min(sent_data$uid_dev)
# check how many uid dev values are 0 or smaller
print("Number of UIDev values <= 0:")
sum(sent_data$uid_dev <= 0, na.rm=TRUE)
# where are the 0 values
print("Location of 0 values:")
which(sent_data$uid_dev == 0)
sent_data$uid_dev[sent_data$uid_dev == 0]

# stop redirecting output to file
sink()

# adjust 0 values
#sent_data$uidev_adj <- ifelse(sent_data$uid_dev == 0, 1e-6, sent_data$uid_dev)

### REGRESSION MODEL ###

# regression model
lm_uidev <- glmmTMB::glmmTMB(uid_dev ~ year_c * avg_srp_c * sent_len_c
                             + (1|author)
                             + (1+sent_len_c|journal),
                             family=Gamma(link = "log"),
                             data=sent_data)
summary(lm_uidev)
# write model summary to file
output <- capture.output(summary(lm_uidev))
cat("model_summary", output, file="uidev_sent_model.txt", sep="\n", append=TRUE)

# model diagnostics

# redirect output to file
sink("uidev_sent_output.txt", append=T)

# AIC
print("AIC:")
AIC(lm_uidev)
# random effects variance
print("Random effects variance:")
VarCorr(lm_uidev)
# Variance Inflation Factor (collinearity)
print("Collinearity (VIF):")
performance::check_collinearity(lm_uidev)

# stop redirecting output to file
sink()

# create diagnostic plots
sim_lm_uidev <- DHARMa::simulateResiduals(lm_uidev)
pdf("uidev_sent_qqplot_residuals.pdf")
DHARMa::plotQQunif(sim_lm_uidev) # create qq-plot
DHARMa::plotResiduals(sim_lm_uidev) # plot residuals against expected value
DHARMa::plotResiduals(sim_lm_uidev, form = sent_data$year_c) # plot residuals against predictor
dev.off()

# plot model effects

# effect of year * average surprisal * sentence length
pdf("uidev_sent_effect_year_avgSrp_sentLen.pdf")
ggeffects::ggeffect(lm_uidev, c("year_c", "avg_srp_c", "sent_len_c")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and Sentence Length",
       y = "UID Dev",
       title = "")
dev.off()

# effect of year
pdf("uidev_sent_effect_year.pdf")
ggeffects::ggeffect(lm_uidev, c("year_c")) %>%
  plot() +
  labs(x = "Year",
       y = "UID Dev",
       title = "")
dev.off()

# effect of sentence length
pdf("uidev_sent_effect_sentLen.pdf")
ggeffects::ggeffect(lm_uidev, c("sent_len_c")) %>%
  plot() +
  labs(x = "Sentence Length",
       y = "UID Dev",
       title = "")
dev.off()

# effect of average surprisal
pdf("uidev_sent_effect_avgSrp.pdf")
ggeffects::ggeffect(lm_uidev, c("avg_srp_c")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "UID Dev",
       title = "")
dev.off()

### CROSS-VALIDATION ###

# use all available cores except one
future::plan(future::multisession, workers = parallel::detectCores() - 1)

set.seed(12)
# create 10 folds
folds <- rsample::vfold_cv(sent_data, v = 10)

results <- future.apply::future_lapply(
  folds$splits,
  function(split) {
    # select train and test data
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    # fit mixed-effects model
    fit <- glmmTMB::glmmTMB(uid_dev ~ year_c * avg_srp_c * sent_len_c
                            + (1|author)
                            + (1+sent_len_c|journal),
                            family=Gamma(link = "log"),
                            data=sent_data)
    
    # predict on test data
    preds <- predict(fit, newdata = test, allow.new.levels = TRUE)
    
    # compute error metrics
    # RMSE = Root Mean Squared Error
    # MAE = Mean Absolute Error
    dplyr::tibble(
      rmse = sqrt(mean((test$uid_dev - preds)^2)),
      mae  = mean(abs(test$uid_dev - preds))
    )
  },
  future.seed = TRUE,
  future.scheduling = 1,
  future.packages = c("lmerTest", "rsample", "dplyr")
)

cv_results <- dplyr::bind_rows(results)

cv_results %>%
  dplyr::summarise(
    mean_rmse = mean(rmse),
    mean_mae  = mean(mae)
  )

# save error metrics to csv file
write.csv(cv_results,
          file = "uidev_sent_cv_results.csv")
