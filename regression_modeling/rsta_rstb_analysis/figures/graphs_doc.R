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

# get centered value of specific average surprisal values
avgSrp_1 <- center_value(doc_data,"avg_srp",2.5)
avgSrp_2 <- center_value(doc_data,"avg_srp",5)
avgSrp_3 <- center_value(doc_data,"avg_srp",7.5)
avgSrp_1
avgSrp_2
avgSrp_3
# get centered value of specific document length values
docLen_1 <- center_value(doc_data,"doc_len",5000)
docLen_2 <- center_value(doc_data,"doc_len",10000)
docLen_3 <- center_value(doc_data,"doc_len",20000)
docLen_1
docLen_2
docLen_3
# get centered value of specific vocabulary size values
vocab_1 <- center_value(doc_data, "vocab_size", 15000)
vocab_2 <- center_value(doc_data, "vocab_size", 30000)
vocab_3 <- center_value(doc_data, "vocab_size", 45000)
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
                      "doc_len_c[255.4767,5255.477,15255.48]")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "UID Dev",
       title = "",
       color = "Text Length") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("5000", "10000", "20000")) +
  scale_x_continuous(breaks = c(-4.376983,-1.876983,0.6230171),
                     labels = c("2.5","5","7.5"))

# effect of vocabulary size
ggeffects::ggeffect(lm_uidev,
                    c("vocab_size_c")) %>%
  plot() +
  labs(x = "Vocabulary Size",
       y = "UID Dev",
       title = "",
       color = "Sent. Length") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-14796.23,203.7735,15203.77),
                     labels = c("15000","30000","45000"))


###### Sigma ######

# load model
load("./results/server/sigma_doc/20260204/lm_sigma_doc.rda")

# effect of average surprisal * document length
ggeffects::ggeffect(lm_sigma,
                    c("avg_srp_c",
                      "doc_len_c[255.4767,5255.477,15255.48]")) %>%
  plot() +
  labs(x = "Average Surprisal",
       y = "IFC",
       title = "",
       color = "Text Length") +
  theme_minimal() +
  scale_color_manual(values=c("#69b3a2", "purple", "black"),
                     labels = c("5000", "10000", "20000")) +
  scale_x_continuous(breaks = c(-4.376983,-1.876983,0.6230171),
                     labels = c("2.5","5","7.5"))

# effect of year
ggeffects::ggeffect(lm_uidev,
                    c("year_c")) %>%
  plot() +
  labs(x = "Year",
       y = "UID Dev",
       title = "") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-79.25988,-49.25988,-19.25988,10.74012),
                     labels = c("1890","1920","1950","2980"))
