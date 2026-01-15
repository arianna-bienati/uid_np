# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 29-10-2025

# RQ: Does fluctuation complexity decrease over time?
# analysis level: document
# measure: sigma

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
  group_by(year, journal) %>%  # stratify by year and journal
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
sink("sigma_doc_output.txt", append=T)

# get minimum and maximum values of year
print("Minimum value of year:")
min(doc_data$year)
print("Maximum value of year:")
max(doc_data$year)

# get number of observations per year
print("Number of observations per year:")
doc_data %>% count(year)

# get number of observations per journal
print("Number of observations per journal:")
doc_data %>% count(journal)

# get minimum, maximum, mean and standard deviation of average surprisal
print("Minimum value of average surprisal:")
min(doc_data$avg_srp)
print("Maximum value of average surprisal:")
max(doc_data$avg_srp)
print("Mean value of average surprisal:")
mean(doc_data$avg_srp)
print("Standard deviation of average surprisal:")
sd(doc_data$avg_srp)

# get minimum, maximum, mean and standard deviation of length
print("Minimum value of document length:")
min(doc_data$doc_len)
print("Maximum value of document length:")
max(doc_data$doc_len)
print("Mean value of document length:")
mean(doc_data$doc_len)
print("Standard deviation of doc length:")
sd(doc_data$doc_len)

# check minimum value of sigma
print("Minimum value of sigma:")
min(doc_data$sigma_gamma)
# check how many sigma values are 0 or smaller
print("Number of sigma values <= 0:")
sum(doc_data$sigma_gamma <= 0, na.rm=TRUE)
# where are the 0 values
print("Location of 0 values:")
which(doc_data$sigma_gamma == 0)
doc_data$uid_dev[doc_data$sigma_gamma == 0]

# stop redirecting output to file
sink()

### REGRESSION MODEL ###

# regression model
lm_sigma <- glmmTMB::glmmTMB(sigma_gamma ~ avg_srp_c * doc_len_c
                             + year_c
                             + vocab_size_c
                             + (1|journal),
                             data=doc_data)
summary(lm_sigma)
# write model summary to file
output <- capture.output(summary(lm_sigma))
cat("model_summary", output, file="sigma_doc_model.txt", sep="\n", append=TRUE)

# model diagnostics

# redirect output to file
sink("sigma_doc_output.txt", append=T)

# AIC
print("AIC:")
AIC(lm_sigma)
# random effects variance
print("Random effects variance:")
VarCorr(lm_sigma)
# Variance Inflation Factor (collinearity)
print("Collinearity (VIF):")
performance::check_collinearity(lm_sigma)

# stop redirecting output to file
sink()

# create diagnostic plots
sim_lm_sigma <- DHARMa::simulateResiduals(lm_sigma)
pdf("sigma_doc_qqplot_residuals.pdf")
DHARMa::plotQQunif(sim_lm_sigma) # create qq-plot
DHARMa::plotResiduals(sim_lm_sigma) # plot residuals against expected value
DHARMa::plotResiduals(sim_lm_sigma, form = doc_data$year_c) # plot residuals against predictor
dev.off()

# save model
save(lm_sigma, file = "lm_sigma_doc.rda")

# plot model effects

# function to center a concrete variable value
center_value <- function(data,column,x){
  mean_value = mean(data[[column]])
  centered = x - mean_value
}

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(doc_data,"avg_srp",10)
avgSrp_2 <- center_value(doc_data,"avg_srp",20)
avgSrp_3 <- center_value(doc_data,"avg_srp",30)
# get centered value of specific document length values
NPlen_1 <- center_value(doc_data,"doc_len",500)
NPlen_1 <- center_value(doc_data,"doc_len",1000)
NPlen_1 <- center_value(doc_data,"doc_len",2000)

# effect of year * average surprisal * document length
pdf("sigma_doc_effect_year_avgSrp_docLen.pdf")
ggeffects::ggeffect(lm_sigma, c("year_c",
                                "avg_srp_c[avgSrp_1,avg_Srp_2,avgSrp_3]",
                                "doc_len_c[docLen_1,docLen_2,docLen_3]")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and Document Length",
       y = "IFC",
       title = "")
dev.off()

# effect of year * average surprisal * document length
pdf("sigma_doc_effect_avgSrp_docLen.pdf")
ggeffects::ggeffect(lm_uidev, c("avg_srp_c",
                                "doc_len_c[docLen_1,docLen_2,docLen_3]")) %>%
  plot() +
  labs(x = "Average Surprisal and Document Length",
       y = "IFC",
       title = "")
dev.off()

# effect of year 
pdf("sigma_doc_effect_year.pdf")
ggeffects::ggeffect(lm_sigma, c("year_c")) %>%
  plot() +
  labs(x = "Year",
       y = "IFC",
       title = "")
dev.off()

# effect of average surprisal
pdf("sigma_doc_effect_avgSrp.pdf")
ggeffects::ggeffect(lm_sigma, c("avg_srp_c")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "IFC",
       title = "")
dev.off()

# effect of document length
pdf("sigma_doc_effect_docLen.pdf")
ggeffects::ggeffect(lm_sigma, c("doc_len_c")) %>%
  plot() +
  labs(x = "Document Length",
       y = "IFC",
       title = "")
dev.off()

# effect of vocabulary size
pdf("sigma_doc_effect_vocSize.pdf")
ggeffects::ggeffect(lm_sigma, c("vocab_size_c")) %>%
  plot() +
  labs(x = "Vocabulary Size",
       y = "IFC",
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
    fit <- glmmTMB::glmmTMB(sigma_gamma ~ year_c * avg_srp_c * doc_len_c
                            + vocab_size
                            + (1|author)
                            + (1+doc_len_c|journal),
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
          file = "sigma_doc_cv_results.csv")
