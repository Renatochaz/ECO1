### Create lagged, quadratic and interactions variables

setwd("G:/Meu Drive/GIT/ECO1")

df <- read.csv("pos_win_1_constructed_quarterly_data_2006-2019.csv", stringsAsFactors = FALSE)

# Function to create lagged variable
# Considere that there are gaps in quarters, and you cant use quarter 10 as lagged for quarter 12

fc_construct_lagged <- function(df, var1) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"Quarter"] - df[i-1,"Quarter"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
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

# Constructing and adding
df_pos1_lag_AT <- fc_construct_lagged(df,"INV_AT")
df_pos1_lag_K <- fc_construct_lagged(df, "INV_K")

df_pos1_lag <- df

df_pos1_lag <- df_pos1_lag[-c(vec_skips),]
rownames(df_pos1_lag) <- 1:nrow(df_pos1_lag)

df_pos1_lag$INV_AT_L1 <- df_pos1_lag_AT
df_pos1_lag$INV_K_L1 <- df_pos1_lag_K
df_pos1_lag$INV_AT_L1_quad <- df_pos1_lag$INV_AT_L1^2
df_pos1_lag$INV_K_L1_quad <- df_pos1_lag$INV_K_L1^2
df_pos1_lag$LN_PU_FC_AT <- df_pos1_lag$LN_PU*df_pos1_lag$FC_AT
df_pos1_lag$LN_PU_FC_K <- df_pos1_lag$LN_PU*df_pos1_lag$FC_K
