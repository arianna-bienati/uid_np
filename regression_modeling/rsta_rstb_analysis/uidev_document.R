# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 29-10-2025

# RQ: Does fluctuation complexity decrease over time?
# analysis level: document
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

# open data files
#data <- read.table(file="test/test_v3.csv",
data <- read.table(file="document_data.csv",
                   sep=",",
                   header=TRUE,
                   quote='"',
                   fill=TRUE)
head(data)

vocab_data <- read.table(file="document_data_vocab_per_year.csv",
                         sep=",",
                         header=TRUE,
                         quote='"',
                         fill=TRUE)
head(vocab_data)

# filter data
data_filtered <- data %>%
  filter(year <= 1989) %>% # remove observations after 1989
  filter(!is.na(uid_dev)) %>% # remove observations without uid_dev value
  filter(!is.na(sigma_gamma)) # remove observations without sigma_gamma value

# combine with vocabulary size data
doc_data <- left_join(data_filtered,vocab_data,by="year")
doc_data <- doc_data %>%
  rename("vocab_size" = "vocab_size.y") 
head(doc_data)

# sample
set.seed(155)
doc_data <- doc_data %>%
  group_by(year) %>%  # stratify by year
  sample_frac(0.75) %>%  # take 75 % from each category
  ungroup()

# transform year variable
doc_data$year_c <- scale(doc_data$year, center=T, scale=F)
# transform document length variable
doc_data$doc_len_c <- scale(doc_data$doc_len, center=T, scale=F)
# transform average surprisal variable
doc_data$avg_srp_c <- scale(doc_data$avg_srp, center=T, scale=F)
# transform vocabulary size variable
doc_data$vocab_size_c <- scale(doc_data$vocab_size, center=T, scale=F)

# redirect output to file
sink("uidev_doc_output.txt", append=T)

# check minimum value of uid dev
print("Minimum value of UIDev:")
min(np_data$uid_dev)
# check how many uid dev values are 0 or smaller
print("Number of UIDev values <= 0:")
sum(np_data$uid_dev <= 0, na.rm=TRUE)
# where are the 0 values
print("Location of 0 values:")
which(np_data$uid_dev == 0)
np_data$uid_dev[np_data$uid_dev == 0]

# stop redirecting output to file
sink()

# adjust 0 values
doc_data$uidev_adj <- ifelse(doc_data$uid_dev == 0, 1e-6, doc_data$uid_dev)

# inspect data
head(doc_data)

### REGRESSION MODEL ###

# regression model
lm_uidev <- glmmTMB::glmmTMB(uidev_adj ~ year_c * avg_srp_c * doc_len_c
                             + vocab_size
                             + (1|author)
                             + (1+doc_len_c|journal),
                             family=Gamma(link = "log"),
                             data=doc_data)
summary(lm_uidev)
# write model summary to file
output <- capture.output(summary(lm_uidev))
cat("model_summary", output, file="uidev_doc_model.txt", sep="\n", append=TRUE)

# model diagnostics

# redirect output to file
sink("uidev_doc_output.txt", append=T)

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
pdf("uidev_doc_qqplot_residuals.pdf")
DHARMa::plotQQunif(sim_lm_uidev) # create qq-plot
DHARMa::plotResiduals(sim_lm_uidev) # plot residuals against expected value
DHARMa::plotResiduals(sim_lm_uidev, form = doc_data$year_c) # plot residuals against predictor
dev.off()

# plot model effects

# effect of year * average surprisal * document length
pdf("uidev_doc_effect_year_avgSrp_docLen.pdf")
ggeffects::ggeffect(lm_uidev, c("year_c", "avg_srp_c", "doc_len_c")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and Document Length",
       y = "UID Dev",
       title = "")
dev.off()

# effect of year 
pdf("uidev_doc_effect_year.pdf")
ggeffects::ggeffect(lm_uidev, c("year_c")) %>%
  plot() +
  labs(x = "Year",
       y = "UID Dev",
       title = "")
dev.off()

# effect of average surprisal
pdf("uidev_doc_effect_avgSrp.pdf")
ggeffects::ggeffect(lm_uidev, c("avg_srp_c")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "UID Dev",
       title = "")
dev.off()

# effect of document length
pdf("uidev_doc_effect_docLen.pdf")
ggeffects::ggeffect(lm_uidev, c("doc_len_c")) %>%
  plot() +
  labs(x = "Document Length",
       y = "UID Dev",
       title = "")
dev.off()

# effect of vocabulary size
pdf("uidev_doc_effect_vocSize.pdf")
ggeffects::ggeffect(lm_uidev, c("vocab_size_c")) %>%
  plot() +
  labs(x = "Vocabulary Size",
       y = "UID Dev",
       title = "")
dev.off()

### CROSS-VALIDATION ###

my_lib <- "/scratch/landwehr/R/4.5"
default_libs <- .libPaths()
.libPaths(c(my_lib, default_libs))

#print(my_lib, default_libs)

.libPaths(c(my_lib, default_libs))

# use all available cores except one
future::plan(future::multisession, workers = parallel::detectCores() - 1)

set.seed(12)
# create 10 folds
folds <- rsample::vfold_cv(doc_data, v = 10)

results <- future.apply::future_lapply(
  folds$splits,
  function(split) {
    # select train and test data
    train <- rsample::analysis(split)
    test  <- rsample::assessment(split)
    
    # fit mixed-effects model
    fit <- glmmTMB::glmmTMB(uidev_adj ~ year_c * avg_srp_c * doc_len_c
                            + vocab_size
                            + (1|author)
                            + (1+doc_len_c|journal),
                            family=Gamma(link = "log"),
                            data=doc_data)
    
    # predict on test data
    preds <- predict(fit, newdata = test, allow.new.levels = TRUE)
    
    # compute error metrics
    # RMSE = Root Mean Squared Error
    # MAE = Mean Absolute Error
    dplyr::tibble(
      rmse = sqrt(mean((test$sigma_gamma - preds)^2)),
      mae  = mean(abs(test$sigma_gamma - preds))
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
          file = "uidev_doc_cv_results.csv")
