# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 05-03-2026

# script for generating camera-ready graphs from saved models
# NP models (UIDev and sigma)

# libraries
library(dplyr)
library(ggeffects)
library(ggplot2)

# load data
data <- read.table(file="./data/rsta_rstb/NP_data.csv",
                      sep=",",
                      header=TRUE,
                      quote='"',
                      fill=TRUE)

# preprocess data

# filter data
np_data <- data %>%
  group_by(head_lemma) %>% # remove observations with lemma occurrence >= 5
  filter(n() >= 5) %>%
  ungroup() %>%
  filter(year <= 1989) %>% # remove observations after 1989
  filter(!is.na(uid_dev)) %>% # remove observations without uid_dev value
  filter(!is.na(sigma_gamma)) # remove observations without sigma_gamma value

# sample
set.seed(957)
np_data <- np_data %>%
  group_by(year, journal) %>%  # stratify by year and journal
  sample_frac(0.5) %>%  # take 50 % from each category
  ungroup()

# transform year variable
np_data$year_c <- scale(np_data$year, center=T, scale=F)
# transform NP length variable
np_data$NP_len_c <- scale(np_data$NP_len, center=T, scale=F)
# transform average surprisal variable
np_data$avg_srp_c <- scale(np_data$avg_srp, center=T, scale=F)

# transform syntactic role to factor
np_data$head_synt_role_F <- as.factor(np_data$head_synt_role)
levels(np_data$head_synt_role_F) # show factor levels

# apply contrast coding
contrasts(np_data$head_synt_role_F) = contr.treatment(3, base=1) # baseline=nsubj

# adjust 0 values for uidev
np_data$uidev_adj <- ifelse(np_data$uid_dev == 0, 1e-6, np_data$uid_dev)

# function to center a concrete variable value
center_value <- function(data,column,x){
  mean_value = mean(data[[column]])
  centered = x - mean_value
}

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(np_data,"avg_srp",2)
avgSrp_2 <- center_value(np_data,"avg_srp",7)
avgSrp_3 <- center_value(np_data,"avg_srp",12)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific NP length values
NPlen_1 <- center_value(np_data,"NP_len",5)
NPlen_2 <- center_value(np_data,"NP_len",15)
NPlen_3 <- center_value(np_data,"NP_len",20)
NPlen_1
NPlen_2
NPlen_3
# get centered value of specific year values
year_1 <- center_value(np_data, "year", 1890)
year_2 <- center_value(np_data, "year", 1920)
year_3 <- center_value(np_data, "year", 1950)
year_4 <- center_value(np_data, "year", 1980)
year_1
year_2
year_3
year_4

###### UIDev ######

# load model
load("./results/server/uidev_np/20260203/lm_uidev_np.rda")

# effect of year * average surprisal * NP length
ggeffects::ggeffect(lm_uidev,
                    c("year_c",
                      "avg_srp_c[-5.448382,-0.4483816,4.551618]",
                      "NP_len_c[-3.532716,6.467284,11.46728]")
                    ) %>%
  plot() +
  labs(x = "Year, Average Surprisal and NP Length",
       y = "UID Dev",
       title = "",
       color = "Avg. Surprisal") +
  theme_minimal() +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-3.532716" = "NP Length: 5",
                         "6.467284" = "NP Length: 15",
                         "11.46728" = "NP Length: 20"))) +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("2", "7", "12")) +
  scale_x_continuous(breaks = c(-71.95009,-41.95009,-11.95009,18.04991),
                     labels = c("1890","1920","1950","1980"))

# effect of head syntactic role
ggeffects::ggeffect(lm_uidev, c("head_synt_role_F")) %>%
  plot() +
  labs(x = "Head Syntactic Role",
       y = "UID Dev",
       title = "") +
  theme_minimal()


###### Sigma ######

# load model
load("./results/server/sigma_np/20260204/lm_sigma_np.rda")

# effect of year * average surprisal * NP length
ggeffects::ggeffect(lm_sigma,
                    c("year_c",
                      "avg_srp_c[-5.448382,-0.4483816,4.551618]",
                      "NP_len_c[-3.532716,6.467284,11.46728]")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and NP Length",
       y = "IFC",
       title = "",
       color = "Avg. Surprisal") +
  theme_minimal() +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-3.532716" = "NP Length: 5",
                         "6.467284" = "NP Length: 15",
                         "11.46728" = "NP Length: 20"))) +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("2", "7", "12")) +
  scale_x_continuous(breaks = c(-71.95009,-41.95009,-11.95009,18.04991),
                     labels = c("1890","1920","1950","1980"))

# effect of head syntactic role
ggeffects::ggeffect(lm_sigma, c("head_synt_role_F")) %>%
  plot() +
  labs(x = "Head Syntactic Role",
       y = "IFC",
       title = "") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"))

