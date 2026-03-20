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

# get mean and standard deviation of average surprisal
mean(np_data$avg_srp) # 7.448382
sd(np_data$avg_srp) # 2.431156

# get mean and standard deviation of NP length
mean(np_data$NP_len) # 8.532716
sd(np_data$NP_len) # 8.297404

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(np_data,"avg_srp",3)
avgSrp_2 <- center_value(np_data,"avg_srp",7)
avgSrp_3 <- center_value(np_data,"avg_srp",11)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific NP length values
NPlen_1 <- center_value(np_data,"NP_len",1)
NPlen_2 <- center_value(np_data,"NP_len",9)
NPlen_3 <- center_value(np_data,"NP_len",17)
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
                      "avg_srp_c[-4.448382,-0.4483816,3.551618]",
                      "NP_len_c[-7.532716,0.4672841,8.467284]")
                    ) %>%
  plot() +
  labs(x = "Year, Average Surprisal and NP Length",
       y = "UID Dev",
       title = "",
       color = "Avg. Surprisal",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size=15) +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-7.532716" = "NP Length: 1 (mean - SD)",
                         "0.4672841" = "NP Length: 9 (mean)",
                         "8.467284" = "NP Length: 17 (mean + SD)"))) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("3 (mean - 2 SD)", "7 (mean)", "11 (mean + 2 SD)")) +
  scale_x_continuous(breaks = c(-71.95009,-41.95009,-11.95009,18.04991),
                     labels = c("1890","1920","1950","1980"))

# save plot
ggsave("./results/figures/uidev_np_time_avgSrp_npLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of head syntactic role
ggeffects::ggeffect(lm_uidev, c("head_synt_role_F")) %>%
  plot() +
  labs(x = "Head Syntactic Role",
       y = "UID Dev",
       title = "",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15)

# save plot
ggsave("./results/figures/uidev_np_headSynRole.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")



###### Sigma ######

# load model
load("./results/server/sigma_np/20260204/lm_sigma_np.rda")

# effect of year * average surprisal * NP length
ggeffects::ggeffect(lm_sigma,
                    c("year_c",
                      "avg_srp_c[-4.448382,-0.4483816,3.551618]",
                      "NP_len_c[-7.532716,0.4672841,8.467284]")) %>%
  plot() +
  labs(x = "Year, Average Surprisal and NP Length",
       y = "IFC",
       title = "",
       color = "Avg. Surprisal",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  facet_wrap(~facet,
             labeller = labeller(
               facet = c("-7.532716" = "NP Length: 1 (mean - SD)",
                         "0.4672841" = "NP Length: 9 (mean)",
                         "8.467284" = "NP Length: 17 (mean + SD)"))) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("3 (mean - 2 SD)", "7 (mean)", "11 (mean + 2 SD)")) +
  scale_x_continuous(breaks = c(-71.95009,-41.95009,-11.95009,18.04991),
                     labels = c("1890","1920","1950","1980"))

# save plot
ggsave("./results/figures/sigma_np_year_avgSrp_npLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of head syntactic role
ggeffects::ggeffect(lm_sigma, c("head_synt_role_F")) %>%
  plot() +
  labs(x = "Head Syntactic Role",
       y = "IFC",
       title = "",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15)

# save plot
ggsave("./results/figures/sigma_np_headSynRole.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

