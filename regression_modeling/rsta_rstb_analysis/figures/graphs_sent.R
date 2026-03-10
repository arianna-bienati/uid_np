# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 10-03-2026

# script for generating camera-ready graphs from saved models
# Sentence models (UIDev and sigma)

# libraries
library(dplyr)
library(ggeffects)
library(ggplot2)

# load data
data <- read.table(file="./data/rsta_rstb/sentence_data_no_content.csv",
                   sep=",",
                   header=TRUE,
                   quote='"',
                   fill=TRUE)

# preprocess data

# filter data
sent_data <- data %>%
  filter(sent_len <= 100) %>% # keep observations with sentence length <= 100
  filter(sent_len > 3) %>% # keep observations with sentence length > 3
  filter(year <= 1989) %>% # remove observations after 1989
  filter(!is.na(uid_dev)) %>% # remove observations without uid_dev value
  filter(!is.na(sigma_gamma)) # remove observations without sigma_gamma value

# function to center a concrete variable value
center_value <- function(data,column,x){
  mean_value = mean(data[[column]])
  centered = x - mean_value
}

###### UIDev ######

# preprocess (UIDev-specific since different seed was used than for sigma)

# sample
set.seed(328)
sent_data <- sent_data %>%
  group_by(year, journal) %>%  # stratify by year and journalr
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

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(sent_data,"avg_srp",10)
avgSrp_2 <- center_value(sent_data,"avg_srp",12)
avgSrp_3 <- center_value(sent_data,"avg_srp",15)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific sentence length values
SentLen_1 <- center_value(sent_data,"sent_len",10)
SentLen_2 <- center_value(sent_data,"sent_len",20)
SentLen_3 <- center_value(sent_data,"sent_len",30)
SentLen_1
SentLen_2
SentLen_3
# get centered value of specific year values
year_1 <- center_value(sent_data, "year", 1890)
year_2 <- center_value(sent_data, "year", 1920)
year_3 <- center_value(sent_data, "year", 1950)
year_4 <- center_value(sent_data, "year", 1980)
year_1
year_2
year_3
year_4

# load model
load("./results/server/uidev_sent/20260204/lm_uidev_sent.rda")

# effect of year * average surprisal * sentence length
ggeffects::ggeffect(lm_uidev,
                    c("year_c",
                      "avg_srp_c[3.192307,5.192307,8.192307]",
                      "sent_len_c[-20.00234,-10.00234,-0.002344595]")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and Sentence Length",
       y = "UID Dev",
       title = "",
       color = "Avg. Surprisal") +
  theme_minimal() +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-20.00234" = "Sent. Length: 10",
                         "-10.00234" = "Sent. Length: 20",
                         "-0.002344595" = "Sent. Length: 30"))) +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("10", "12", "15")) +
  scale_x_continuous(breaks = c(-72.12175,-42.12175,-12.12175,17.87825),
                     labels = c("1890","1920","1950","1980"))
  

###### Sigma ######

# preprocess

# sample
set.seed(957)
sent_data <- sent_data %>%
  group_by(year, journal) %>%  # stratify by year and journalr
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

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(sent_data,"avg_srp",10)
avgSrp_2 <- center_value(sent_data,"avg_srp",12)
avgSrp_3 <- center_value(sent_data,"avg_srp",15)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific sentence length values
SentLen_1 <- center_value(sent_data,"sent_len",10)
SentLen_2 <- center_value(sent_data,"sent_len",20)
SentLen_3 <- center_value(sent_data,"sent_len",30)
SentLen_1
SentLen_2
SentLen_3
# get centered value of specific year values
year_1 <- center_value(sent_data, "year", 1890)
year_2 <- center_value(sent_data, "year", 1920)
year_3 <- center_value(sent_data, "year", 1950)
year_4 <- center_value(sent_data, "year", 1980)
year_1
year_2
year_3
year_4

# load model
load("./results/server/sigma_sent/20260204/lm_sigma_sent.rda")

# effect of year * average surprisal
ggeffects::ggeffect(lm_sigma,
                    c("year_c",
                      "avg_srp_c[3.191468,5.191468,8.191468]")) %>%
  plot() +
  labs(x = "Year and Average Surprisal",
       y = "IFC",
       title = "",
       color = "Avg. Surprisal") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("10", "20", "30")) +
  scale_x_continuous(breaks = c(-72.12166,-42.12166,-12.12166,17.87834),
                     labels = c("1890","1920","1950","1980"))

# effect of year * sentence length
ggeffects::ggeffect(lm_sigma,
                    c("year_c",
                      "sent_len_c[-20.02675,-10.02675,-0.02675182]")) %>%
  plot() +
  labs(x = "Year and Sentence Length",
       y = "IFC",
       title = "",
       color = "Sent. Length") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("10", "20", "30")) +
  scale_x_continuous(breaks = c(-72.12166,-42.12166,-12.12166,17.87834),
                     labels = c("1890","1920","1950","1980"))

# effect of average surprisal * sentence length
ggeffects::ggeffect(lm_sigma,
                    c("avg_srp_c",
                      "sent_len_c[-20.02675,-10.02675,-0.02675182]")) %>%
  plot() +
  labs(x = "Average Surprisal and Sentence Length",
       y = "IFC",
       title = "",
       color = "Sent. Length") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("10", "20", "30")) +
  scale_x_continuous(breaks = c(3.191468,5.191468,8.191468),
                     labels = c("10","20","30"))
