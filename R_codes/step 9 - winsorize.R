### Winsorizing variables

### Pos winsorizing at 1% all variables
{
  t <- df_after[,12:46]
  t <- lapply(t, Winsorize, probs = c (0.01, 0.99))
  
  df_after1 <- df_after
  df_after1[12:46] <- t[1:35]
  
  rownames(df_after1) <- 1:nrow(df_after1)
  
  rm(t)
}

setwd("G:/Meu Drive/GIT/ECO1")
write.csv(df_after1, "pos_win_1_constructed_quarterly_data_2006-2019.csv", row.names = FALSE)

rm(list = ls())
