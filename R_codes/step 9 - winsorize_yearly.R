### Winsorizing variables
# Change year to beginning of dataset
df_after <- df_after[,c(1,27,2:47)]
df_after <- subset(df_after, select = -c(year.1))

### Pos winsorizing at 1% all variables
{
  t <- df_after[,13:47]
  t <- lapply(t, Winsorize, probs = c (0.01, 0.99))
  
  df_after1 <- df_after
  df_after1[13:47] <- t[1:35]
  
  rownames(df_after1) <- 1:nrow(df_after1)
  
  rm(t)
}

setwd("G:/Meu Drive/GIT/ECO1")
write.csv(df_after1, "pos_win_1_constructed_yearly_data_2005-2019.csv", row.names = FALSE)

rm(list = ls())
