# RSC analysis: NP fluctuation complexity

# Isabell Landwehr
# 10-03-2026

# script for generating camera-ready graphs from saved models
# Document models (UIDev and sigma)

# libraries
library(dplyr)
library(ggeffects)
library(ggplot2)

# load data
data <- read.table(file="./data/rsta_rstb/document_data.csv",
                   sep=",",
                   header=TRUE,
                   quote='"',
                   fill=TRUE)

vocab_data <- read.table(file="./data/rsta_rstb/document_data_vocab_per_year.csv",
                         sep=",",
                         header=TRUE,
                         quote='"',
                         fill=TRUE)

# preprocess data

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
set.seed(957)
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

# adjust 0 values
doc_data$uidev_adj <- ifelse(doc_data$uid_dev == 0, 1e-6, doc_data$uid_dev)

# function to center a concrete variable value
center_value <- function(data,column,x){
  mean_value = mean(data[[column]])
  centered = x - mean_value
}

# get mean and standard deviation of average surprisal
mean(doc_data$avg_srp) # 6.876983
sd(doc_data$avg_srp) # 1.220323

# get mean and standard deviation of document length
mean(doc_data$doc_len) # 4744.523
sd(doc_data$doc_len) # 5642.37

# get mean and standard deviation of vocabulary size
mean(doc_data$vocab_size) # 29796.23
sd(doc_data$vocab_size) # 14346.26

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(doc_data,"avg_srp",5)
avgSrp_2 <- center_value(doc_data,"avg_srp",7)
avgSrp_3 <- center_value(doc_data,"avg_srp",9)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific document length values
docLen_1 <- center_value(doc_data,"doc_len",2000)
docLen_2 <- center_value(doc_data,"doc_len",5000)
docLen_3 <- center_value(doc_data,"doc_len",10000)
docLen_1
docLen_2
docLen_3
# get centered value of specific vocabulary size values
vocab_1 <- center_value(doc_data, "vocab_size", 16000)
vocab_2 <- center_value(doc_data, "vocab_size", 30000)
vocab_3 <- center_value(doc_data, "vocab_size", 44000)
vocab_1
vocab_2
vocab_3
# get centered value of specific year values
year_1 <- center_value(doc_data, "year", 1890)
year_2 <- center_value(doc_data, "year", 1920)
year_3 <- center_value(doc_data, "year", 1950)
year_4 <- center_value(doc_data, "year", 1980)
year_1
year_2
year_3
year_4

###### UIDev ######

# load model
load("./results/server/uidev_doc/20260204/lm_uidev_doc.rda")

# effect of average surprisal * document length
ggeffects::ggeffect(lm_uidev,
                    c("avg_srp_c",
                      "doc_len_c[-2744.523,255.4767,5255.477]")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "UID Dev",
       title = "",
       color = "Text Length",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("2000", "5000", "10000")) +
  scale_x_continuous(breaks = c(-1.876983,0.1230171,2.123017),
                     labels = c("5","7","9"))

# save plot
ggsave("./results/figures/uidev_doc_avgSrp_textLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of average surprisal * document length (v2)
ggeffects::ggeffect(lm_uidev,
                    c("doc_len_c",
                      "avg_srp_c[-1.876983,0.1230171,2.123017]")) %>%
  plot() +
  labs(x = "Text Length",
       y = "UID Dev",
       title = "",
       color = "Avg. Srp",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("5", "7", "9")) +
  scale_x_continuous(breaks = c(-2744.523,255.4767,5255.477),
                     labels = c("2000","5000","1000"))

# save plot
ggsave("./results/figures/uidev_doc_avgSrp_textLen_v2.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of vocabulary size
ggeffects::ggeffect(lm_uidev,
                    c("vocab_size_c")) %>%
  plot() +
  labs(x = "Vocabulary Size",
       y = "UID Dev",
       title = "",
       color = "Doc. Length",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = c(-13796.23,203.7735,14203.77),
                     labels = c("16000","30000","44000"))

# save plot
ggsave("./results/figures/uidev_doc_vocSize.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")


###### Sigma ######

# load model
load("./results/server/sigma_doc/20260204/lm_sigma_doc.rda")

# effect of average surprisal * document length
ggeffects::ggeffect(lm_sigma,
                    c("avg_srp_c",
                      "doc_len_c[-2744.523,255.4767,5255.477]")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "IFC",
       title = "",
       color = "Text Length",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_color_manual(values=c("steelblue", "#3c901d", "orange"),
                     labels = c("2000", "5000", "10000")) +
  scale_x_continuous(breaks = c(-1.876983,0.1230171,2.123017),
                     labels = c("5","7","9"))

# save plot
ggsave("./results/figures/sigma_doc_avgSrp_textLen.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")

# effect of year
ggeffects::ggeffect(lm_uidev,
                    c("year_c")) %>%
  plot() +
  labs(x = "Year",
       y = "UID Dev",
       title = "",
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12)) +
  theme_minimal(base_size = 15) +
  scale_x_continuous(breaks = c(-79.25988,-49.25988,-19.25988,10.74012),
                     labels = c("1890","1920","1950","2980"))

# save plot
ggsave("./results/figures/sigma_doc_year.png",
       device = "png", create.dir = TRUE,
       width = 24, height = 12, unit = "cm",
       dpi = 300, bg = "white")
