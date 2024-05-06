library(corrr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(mblm) 
library(MASS)

#List with files 
list_datasets <- list.files(path = "data_1", pattern = ".csv", full.names = TRUE)

#Sets lists with items 

alpha_list <- c()
beta_list <- c()
beta_prime_list <- c()

#Loop to perform calculations for each dataset 

for (df in list_datasets){
  data <- read.csv(df)
  freq <- data$Frequency
  ranks <- seq_along(freq) #ranks 
  
  log_ranks <- log(ranks)
  log_freq <- log(freq)
  
  #Frequencies-rank law 
  model_freq <- rlm(log_freq ~ log_ranks)
  summary(model_freq)
  beta <- coef(model_freq)[2]
  
  #Number-frequencies law 
  model_rank <- rlm(log_ranks ~ log_freq)
  summary(model_rank)
  beta_prime <- coef(model_rank)[2]
  
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
  model_fc <- rlm(log_freq_unique ~ log_fwd_cum)
  summary(model_fc)
  alpha <- coef(model_fc)[2]
  
  # Append results to lists
  alpha_list <- c(alpha_list, alpha)
  beta_list <- c(beta_list, beta)
  beta_prime_list <- c(beta_prime_list, beta_prime)
}


#df with information about languagues
lang_info <- read_csv('language_data_2.csv', show_col_types = FALSE)
languages <- lang_info$Language

#Append columns with new values: alpha, beta, beta prime
lang_info$alpha <- alpha_list
lang_info$beta <- beta_list
lang_info$beta_prime <- beta_prime_list

# Save the dataset with new values 
write.csv(lang_info, "data_languages.csv", row.names = FALSE)

#Multipanel figures 
build_plot <- function(lang_id, lang_name) {
  data <- read_csv(list_datasets[lang_id], show_col_types = FALSE)
  freq<- data$Frequency
  ranks <- seq_along(freq)
  log_ranks <- log(ranks)
  log_freq <- log(freq)
  model_freq <- rlm(log_freq ~ log_ranks) #Frequency-Rank law
  print(summary(model_freq))
  
  label <- paste(lang_name, "regression on frequencies-rank law")
  
  new_plot <- ggplot(data = model_freq, aes(x = log_ranks, y = log_freq)) +
    geom_point(color = "red") +
    geom_smooth(method = "rlm", se = FALSE, col = "purple") +
    labs(title = label,
         x = "Ranks",
         y = "Frequencies")
  return(new_plot)
}

mult_binning <- function(lang_id, lang_name){
  lang_id <- 1
  lang_name <- "test"
  data <- read_csv(list_datasets[lang_id], show_col_types = FALSE)
  freq<- data$Frequency
  # freq <- freq[freq > 0]
  ranks <- seq_along(freq)
  label <- paste("Smoothed Curve", lang_name, "using Multiplicative Binning")
  
  min_value <- min(freq)
  max_value <- max(freq)
  num_bins <- 10
  factor <- exp(log(max_value/min_value) / num_bins)
  bin_edges <- min_value * factor^(0:num_bins)
  bin <- cut(freq, breaks = bin_edges, labels = FALSE)
  smoothed_data <- aggregate(bin ~ freq, FUN = mean)
  bin
  bin <- bin[order(bin)]
  bin
  rg <- lm(freq ~ bin)
  summary(rg)
  plot(bin, freq)
  new_plot <- ggplot(smoothed_data, aes(x = num_bins - bin, y = freq)) +
    geom_line() +
    scale_y_continuous(trans = "log2") +
    labs(x = "Bins", y = "Frequency", title = label)
  return(new_plot)
}

arabic <- build_plot(1, "Arabic")
chinese <- build_plot(4, "Chinese")
japanese <- build_plot(10, "Japanese")
korean <- build_plot(12, "Korean")
kannada <- build_plot(11, "Kannada")
spanish <- build_plot(17, "Spanish")

multiplot <- grid.arrange(arabic, chinese, japanese, korean, kannada, spanish, nrow = 2)

arabic <- mult_binning(1, "Arabic")
chinese <- mult_binning(4, "Chinese")
japanese <- mult_binning(10, "Japanese")
korean <- mult_binning(12, "Korean")
kannada <- mult_binning(11, "Kannada")
spanish <- mult_binning(17, "Spanish")

multiplot <- grid.arrange(arabic, chinese, japanese, korean, kannada, spanish, nrow = 2)






# ----------------------------------------------------

mult_bin <- function(lang_id, lang_name) {
  data <- read_csv(list_datasets[lang_id], show_col_types = FALSE)
  freq<- data$Frequency
  freq <- freq[order(freq)]
  logfr <- log(freq)
  cum_freq <- cumsum(freq)
  percentile_rank <- 1- cum_freq / sum(freq)
  num_bins <- 10
  x_values <- seq(0, 1, length.out = num_bins)
  model <- lm(logfr ~ percentile_rank)
  print(summary(model))
  plot(percentile_rank, freq, type = "b", 
       log = "y",
       xlab = "Percentile Rank", ylab = "Frequency",
       main = "Multiplicative Binning Graph",
  )
  abline(model)
  
}




model_rank <- rlm(log_freq ~ log_ranks)
# Theil_sen_regression
plot(log_freq ~ log_ranks)
abline(model_rank,col='blue')
beta <- coef(model_rank)[2]
beta
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
beta_prime
