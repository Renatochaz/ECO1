#Load
#install.packages("outliers")
library(outliers)
library(DescTools)

setwd("G:/Meu Drive/GIT/ECO1")
full_sample <- read.csv("quint_pre_yearly_full_sample.csv", stringsAsFactors = FALSE)

## Adjust outliers
names <- c("INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT",
           "TAM","QTB","COB_JUROS","DIV_PL","PU","ROA","ROE","ROK")

summary(full_sample[,names])

for (i in 1:length(names)) {
  
  test <- grubbs.test(full_sample[,names[i]])
  test2 <- grubbs.test(full_sample[,names[i]], opposite = TRUE)
  print(names[i])
  print(test)
  print(test2)
}

names <- c("INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT"
           ,"QTB","COB_JUROS","DIV_PL","ROA","ROE","ROK")

### Pos winsorizing at 1% all from names except pu and tam
  t <- full_sample[,names]
  t <- lapply(t, Winsorize, probs = c (0.01, 0.99))
  
  df_after <- full_sample
  df_after[c(32:35,41,43,45:46,49,47,50:51)] <- t[1:12]
  
  rownames(df_after) <- 1:nrow(df_after)
  
  rm(t)

head(full_sample[c(32:35,41,43,45:46,49,47,50:51)])
head(df_after[c(32:35,41,43,45:46,49,47,50:51)])

full_sample <- df_after

## Save for stata
write.csv(full_sample, "wins_quint_yearly_full_sample.csv", row.names = FALSE)



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


summary(full_sample[,names])

write.csv(full_sample, "v2_quint_yearly_r_analize.csv", row.names = FALSE)
rm(list = ls())
