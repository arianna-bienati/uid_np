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

# get mean and standard deviation of average surprisal
mean(sent_data$avg_srp) # 6.807693
sd(sent_data$avg_srp) # 1.509574

# get mean and standard deviation of sentence length
mean(sent_data$sent_len) # 30.00234
sd(sent_data$sent_len) # 15.57855

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(sent_data,"avg_srp",3)
avgSrp_2 <- center_value(sent_data,"avg_srp",7)
avgSrp_3 <- center_value(sent_data,"avg_srp",11)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific sentence length values
SentLen_1 <- center_value(sent_data,"sent_len",14)
SentLen_2 <- center_value(sent_data,"sent_len",30)
SentLen_3 <- center_value(sent_data,"sent_len",46)
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
                      "avg_srp_c[-3.807693,0.1923069,4.192307]",
                      "sent_len_c[-16.00234,-0.002344595,15.99766]")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and Sentence Length",
       y = "UID Dev",
       title = "",
       color = "Avg. Surprisal",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-16.00234" = "Sent. Length: 14 (mean - SD)",
                         "-0.002344595" = "Sent. Length: 30 (mean)",
                         "15.99766" = "Sent. Length: 46 (mean + SD)"))) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("3 (mean - 2 SD)", "7 (mean)", "11 (mean + 2 SD)")) +
  scale_x_continuous(breaks = c(-72.12175,-42.12175,-12.12175,17.87825),
                     labels = c("1890","1920","1950","1980"))
# save plot
ggsave("./results/figures/uidev_sent_year_avgSrp_sentLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")
  

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

# get mean and standard deviation of average surprisal
mean(sent_data$avg_srp) # 6.805563
sd(sent_data$avg_srp) # 1.505471

# get mean and standard deviation of sentence length
mean(sent_data$sent_len) # 30.00658
sd(sent_data$sent_len) # 15.6016

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(sent_data,"avg_srp",3)
avgSrp_2 <- center_value(sent_data,"avg_srp",7)
avgSrp_3 <- center_value(sent_data,"avg_srp",11)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific sentence length values
SentLen_1 <- center_value(sent_data,"sent_len",14)
SentLen_2 <- center_value(sent_data,"sent_len",30)
SentLen_3 <- center_value(sent_data,"sent_len",46)
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
                      "avg_srp_c[-3.805563,0.194437,4.194437]")) %>%
  plot() +
  labs(x = "Year",
       y = "IFC",
       title = "",
       color = "Avg. Surprisal",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("3 (mean - 2 SD)", "7 (mean)", "11 (mean + 2 SD)")) +
  scale_x_continuous(breaks = c(-72.12166,-42.12166,-12.12166,17.87834),
                     labels = c("1890","1920","1950","1980"))

# save plot
ggsave("./results/figures/sigma_sent_year_avgSrp.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of year * sentence length
ggeffects::ggeffect(lm_sigma,
                    c("year_c",
                      "sent_len_c[-16.00658,-0.006578278,15.99342]")) %>%
  plot() +
  labs(x = "Year",
       y = "IFC",
       title = "",
       color = "Sent. Length",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("14 (mean - SD)", "20 (mean)", "30 (mean + SD)")) +
  scale_x_continuous(breaks = c(-72.12166,-42.12166,-12.12166,17.87834),
                     labels = c("1890","1920","1950","1980"))

# save plot
ggsave("./results/figures/sigma_sent_year_sentLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of average surprisal * sentence length
ggeffects::ggeffect(lm_sigma,
                    c("avg_srp_c",
                      "sent_len_c[-16.00658,-0.006578278,15.99342]")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "IFC",
       title = "",
       color = "Sent. Length",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("14 (mean - SD)", "20 (mean)", "30 (mean + SD)")) +
  scale_x_continuous(breaks = c(-3.805563,0.194437,4.194437),
                     labels = c("3","7","11"))

# save plot
ggsave("./results/figures/sigma_sent_sentLen_avgSrp.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")
