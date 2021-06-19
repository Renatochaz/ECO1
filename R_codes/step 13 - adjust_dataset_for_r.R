#Load
setwd("G:/Meu Drive/GIT/ECO1")
full_sample <- read.csv("quint_pre_yearly_full_sample.csv", stringsAsFactors = FALSE)
full_sample <- subset(full_sample, kz_quintile != 2)
# Function to create lagged variable
# Considere that there are gaps in years, and you cant use quarter 10 as lagged for quarter 12

fc_construct_lagged <- function(df, var1) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"year"] - df[i-1,"year"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
      vec_tmp <- c(vec_tmp,(df[i-1,var1])) 
      
    }
    else {
      vec_skips <- c(vec_skips,i)
    }
  }
  
  vec_tmp <- vec_tmp[-c(1)]
  vec_skips <- vec_skips[-c(1)]
  
  vec_skips <<- vec_skips
  return(vec_tmp)
  
}

df_pos1_lag_AT <- fc_construct_lagged(full_sample,"INV_AT")

full_sample <- full_sample[-c(vec_skips),]
rownames(full_sample) <- 1:nrow(full_sample)

full_sample$INV_AT_L1 <- df_pos1_lag_AT

write.csv(full_sample, "quint_yearly_r_analize.csv", row.names = FALSE)
rm(list = ls())
