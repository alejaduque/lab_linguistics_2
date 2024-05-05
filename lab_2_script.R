library(corrr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(mblm) 
library(MASS)
library(gridExtra)
# library(RobustLinearReg)

#List with files 
list_datasets <- list.files(path = "data", pattern = ".csv", full.names = TRUE)

#Sets lists with items 

alpha_list <- c()
beta_list <- c()
beta_prime_list <- c()

#Loop to perform calculations for each dataset 

for (df in list_datasets){
  data <- read.csv(df)
  freq_l <- data$Frequency
  freq <- c(freq_l) #frequencies
  ranks <- seq_along(freq) #ranks 
  
  log_ranks <- log(ranks)
  log_freq <- log(freq)
  
  #Frequencies-rank law 
  model_freq <- lm(log_freq ~ log_ranks)
  alpha <- coef(model_freq)[2]
  
  #Number-frequencies law 
  model_rank <- mblm(log_ranks ~ log_freq)
  beta <- coef(model_rank)[2]
  
  # Forward cumulative and retrieve beta_prime
  
  log_freq_unique <- log(unique(freq))
  
  forward_cumulative <- function(freq_x){
    result <- c()
    freq_unique <- unique(freq_x)
    for (i in 1:length(freq_unique)){
      result <- c(result, sum(freq_x >= freq_unique[i]))
    }
    return(result)
  }
  
  log_fwd_cum <- log(forward_cumulative(freq))
  model_fc <- mblm(log_fwd_cum ~ log_freq_unique)
  
  beta_prime <- coef(model_fc)[2]
  
  # Append results to lists
  alpha_list <- c(alpha_list, alpha)
  beta_list <- c(beta_list, beta)
  beta_prime_list <- c(beta_prime_list, beta_prime)
}


#df with information about languagues
lang_info <- read_csv('language_data_2.csv')
languages <- lang_info$Language

#Append columns with new values: alpha, beta, beta prime
lang_info$alpha <- alpha_list
lang_info$beta <- beta_list
lang_info$beta_prime <- beta_prime_list

# Save the dataset with new values 
write.csv(lang_info, "data_languages.csv", row.names = FALSE)

#Multipanel figures 

#1 
data <- read_csv(list_datasets[1])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p1<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Arabic regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#2 
data <- read_csv(list_datasets[4])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p2<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Chinese regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#3 
data <- read_csv(list_datasets[10])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p3<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Japanese regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#4 
data <- read_csv(list_datasets[12])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p4<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Korean regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#5 
data <- read_csv(list_datasets[11])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p5<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Kannada regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#6 
data <- read_csv(list_datasets[17])
freq_l<- data$Frequency
freq <- c(freq_l) 
ranks <- seq_along(freq)
log_ranks <- log(ranks)
log_freq <- log(freq)
model_freq <- lm(log_freq ~ log_ranks) #Frequency-Rank law

p6<- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, col = "purple") +
  labs(title = "Spanish regression on Frequencies-rank law",
       x = "Ranks",
       y = "Frequencies")

#Arranging plots 
multiplot <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)
#Number-frequencies law 

model_rank <- mblm(log_ranks ~ log_freq)
# Theil_sen_regression
plot(log_ranks ~ log_freq)
abline(model_rank,col='blue')
beta <- coef(model_rank)[2]

# Forward cumulative and retrieve beta_prime

log_freq_unique <- log(unique(freq))

forward_cumulative <- function(freq_x){
  result <- c()
  freq_unique <- unique(freq_x)
  for (i in 1:length(freq_unique)){
    result <- c(result, sum(freq_x >= freq_unique[i]))
  }
  return(result)
}

log_fwd_cum <- log(forward_cumulative(freq))


model_fc <- mblm(log_fwd_cum ~ log_freq_unique)
# Theil_sen_regression
plot(log_fwd_cum ~ log_freq_unique)
abline(model_fc, col='blue')
beta_prime <- coef(model_fc)[2]
