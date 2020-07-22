###### Data analysis and constructed variables

## Starting packages
{
  library(readr)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(plyr)
  library(stringi)
  library(tm)
  library(readxl) 
  library(stringr) 
  library(tidyverse)
  library(stargazer)
  library(DescTools)
}

## Setting working directory
setwd("G:/Meu Drive/USP/Mestrado/Dissertação/R_Mestrado_Principal/base_economat/Dados_clean")

# Reading
{
df_main <- read.csv("clean_quarterly_data_2006-2019.csv", stringsAsFactors = FALSE)
str(df_main)
df <- df_main
}

### Subsetting finance sector (bovespa classification)
### As for now we have 9225 observations and 260 firms
df <- subset(df, !SETOR_ECON_BOVESPA == "Financeiro")
length(unique(df$Código))
rownames(df) <- 1:nrow(df)

### Winsorizing at 1% all variables (except PU)
{
t <- df[,13:26]
t <- lapply(t, Winsorize, probs = c (0.01, 0.99))

df_wins1 <- df
df_wins1[13:26] <- t[1:14]

rownames(df_wins1) <- 1:nrow(df_wins1)

rm(t)
}

### Function for constructing variables with single operation (normal and defased) and normalizing dividend (defased)
fc_construct_1 <- function(df, var1,var2,var3,operator) {
  
  vec_tmp <- 0
  vec_skips <- 0

  for (i in 1:nrow(df)) {
  
    if (length(which(((df[i,"Quarter"] - df[i-1,"Quarter"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
  
      vec_tmp <- c(vec_tmp,(getFunction(operator)((df[i,var1]),(df[i-1,var2])))/df[i-1,var3]) 
    
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

vec_at <- fc_construct_1(df_wins1,13,13,13,"-")
vec_k <- fc_construct_1(df_wins1,14,14,14,"-")
vec_v_at <- fc_construct_1(df_wins1,18,18,13,"-")
vec_v_k <- fc_construct_1(df_wins1,18,18,14,"-")

### Function for constructing variables with single operation (normal) and normalizing dividend (defased)
fc_construct_2 <- function(df, var1,var2,var3,operator) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"Quarter"] - df[i-1,"Quarter"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
      vec_tmp <- c(vec_tmp,(getFunction(operator)((df[i,var1]),(df[i,var2])))/df[i-1,var3]) 
      
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

vec_fc_at <- fc_construct_2(df_wins1,15,19,13,"+")
vec_fc_k <- fc_construct_2(df_wins1,15,19,14,"+")
vec_divida_at <- fc_construct_2(df_wins1,16,17,13,"+")
vec_divida_k <- fc_construct_2(df_wins1,16,17,14,"+")

### Function for constructing variables with single variable and normalizing dividend (defased)
fc_construct_3 <- function(df, var1,var2) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"Quarter"] - df[i-1,"Quarter"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
      vec_tmp <- c(vec_tmp,(df[i,var1]/df[i-1,var2])) 
      
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

vec_caixanorm_at <- fc_construct_3(df_wins1,20,13)
vec_caixanorm_k <- fc_construct_3(df_wins1,20,14)
vec_divpagos_at <- fc_construct_3(df_wins1,26,13)
vec_divpagos_k <- fc_construct_3(df_wins1,26,14)

## DF for constructed variables
df_constructs <- df_wins1

## Subsetting skipped observations
df_constructs <- df_constructs[-c(vec_skips),]
rownames(df_constructs) <- 1:nrow(df_constructs)


# Adding constructed variables
{
df_constructs$INV_AT <- vec_at
df_constructs$FC_AT <- vec_fc_at
df_constructs$D_AT <- vec_divida_at
df_constructs$CV_AT <- vec_v_at
df_constructs$TAM <- log(df_constructs$AT)
df_constructs$INV_K <- vec_k
df_constructs$FC_K <- vec_fc_k
df_constructs$D_K <- vec_divida_k
df_constructs$CV_K <- vec_v_k
df_constructs$ALAVANC <- ((df_constructs$D_CP + df_constructs$D_LP)/(df_constructs$D_CP + df_constructs$D_LP + df_constructs$PL))
df_constructs$CAIXA_AT <- vec_caixanorm_at
df_constructs$CAIXA_K <- vec_caixanorm_k
df_constructs$DIVIDENDOS_AT <- vec_divpagos_at
df_constructs$DIVIDENDOS_K <- vec_divpagos_k
df_constructs$QTB <- ((df_constructs$V_MERC + df_constructs$D_CP + df_constructs$D_LP)/df_constructs$AT)
df_constructs$COB_JUROS <- (df_constructs$EBIT/df_constructs$DESP_FIN)
df_constructs$ROA <- (df_constructs$LL/df_constructs$AT)
df_constructs$LN_PU <- log(df_constructs$PU)
}

# Final set with 8924 observations, 260 unique firms and 44 variables
length(unique(df_constructs$Código))

df_w1 <- df_constructs

write.csv(df_w1, "constructed_quarterly_data_1%_2006-2019.csv", row.names = FALSE)

########### 2% ###################

### Winsorizing at 2% all variables (except PU)
{
  t <- df[,13:26]
  t <- lapply(t, Winsorize, probs = c (0.02, 0.98))
  
  df_wins2 <- df
  df_wins2[13:26] <- t[1:14]
  
  rownames(df_wins2) <- 1:nrow(df_wins2)
  
  rm(t)
}
{
vec_at <- fc_construct_1(df_wins2,13,13,13,"-")
vec_k <- fc_construct_1(df_wins2,14,14,14,"-")
vec_v_at <- fc_construct_1(df_wins2,18,18,13,"-")
vec_v_k <- fc_construct_1(df_wins2,18,18,14,"-")
vec_fc_at <- fc_construct_2(df_wins2,15,19,13,"+")
vec_fc_k <- fc_construct_2(df_wins2,15,19,14,"+")
vec_divida_at <- fc_construct_2(df_wins2,16,17,13,"+")
vec_divida_k <- fc_construct_2(df_wins2,16,17,14,"+")
vec_caixanorm_at <- fc_construct_3(df_wins2,20,13)
vec_caixanorm_k <- fc_construct_3(df_wins2,20,14)
vec_divpagos_at <- fc_construct_3(df_wins2,26,13)
vec_divpagos_k <- fc_construct_3(df_wins2,26,14)
}

## DF for constructed variables
df_constructs <- df_wins2

## Subsetting skipped observations
df_constructs <- df_constructs[-c(vec_skips),]
rownames(df_constructs) <- 1:nrow(df_constructs)

# Adding constructed variables
{
  df_constructs$INV_AT <- vec_at
  df_constructs$FC_AT <- vec_fc_at
  df_constructs$D_AT <- vec_divida_at
  df_constructs$CV_AT <- vec_v_at
  df_constructs$TAM <- log(df_constructs$AT)
  df_constructs$INV_K <- vec_k
  df_constructs$FC_K <- vec_fc_k
  df_constructs$D_K <- vec_divida_k
  df_constructs$CV_K <- vec_v_k
  df_constructs$ALAVANC <- ((df_constructs$D_CP + df_constructs$D_LP)/(df_constructs$D_CP + df_constructs$D_LP + df_constructs$PL))
  df_constructs$CAIXA_AT <- vec_caixanorm_at
  df_constructs$CAIXA_K <- vec_caixanorm_k
  df_constructs$DIVIDENDOS_AT <- vec_divpagos_at
  df_constructs$DIVIDENDOS_K <- vec_divpagos_k
  df_constructs$QTB <- ((df_constructs$V_MERC + df_constructs$D_CP + df_constructs$D_LP)/df_constructs$AT)
  df_constructs$COB_JUROS <- (df_constructs$EBIT/df_constructs$DESP_FIN)
  df_constructs$ROA <- (df_constructs$LL/df_constructs$AT)
  df_constructs$LN_PU <- log(df_constructs$PU)
}

# Final set with 8924 observations, 260 unique firms and 44 variables
length(unique(df_constructs$Código))

df_w2 <- df_constructs


########### 5%% ###################

### Winsorizing at 5% all variables (except PU)
{
  t <- df[,13:26]
  t <- lapply(t, Winsorize, probs = c (0.05, 0.95))
  
  df_wins5 <- df
  df_wins5[13:26] <- t[1:14]
  
  rownames(df_wins5) <- 1:nrow(df_wins5)
  
  rm(t)
}

{
vec_at <- fc_construct_1(df_wins5,13,13,13,"-")
vec_k <- fc_construct_1(df_wins5,14,14,14,"-")
vec_v_at <- fc_construct_1(df_wins5,18,18,13,"-")
vec_v_k <- fc_construct_1(df_wins5,18,18,14,"-")
vec_fc_at <- fc_construct_2(df_wins5,15,19,13,"+")
vec_fc_k <- fc_construct_2(df_wins5,15,19,14,"+")
vec_divida_at <- fc_construct_2(df_wins5,16,17,13,"+")
vec_divida_k <- fc_construct_2(df_wins5,16,17,14,"+")
vec_caixanorm_at <- fc_construct_3(df_wins5,20,13)
vec_caixanorm_k <- fc_construct_3(df_wins5,20,14)
vec_divpagos_at <- fc_construct_3(df_wins5,26,13)
vec_divpagos_k <- fc_construct_3(df_wins5,26,14)
}

## DF for constructed variables
df_constructs <- df_wins5

## Subsetting skipped observations
df_constructs <- df_constructs[-c(vec_skips),]
rownames(df_constructs) <- 1:nrow(df_constructs)

# Adding constructed variables
{
  df_constructs$INV_AT <- vec_at
  df_constructs$FC_AT <- vec_fc_at
  df_constructs$D_AT <- vec_divida_at
  df_constructs$CV_AT <- vec_v_at
  df_constructs$TAM <- log(df_constructs$AT)
  df_constructs$INV_K <- vec_k
  df_constructs$FC_K <- vec_fc_k
  df_constructs$D_K <- vec_divida_k
  df_constructs$CV_K <- vec_v_k
  df_constructs$ALAVANC <- ((df_constructs$D_CP + df_constructs$D_LP)/(df_constructs$D_CP + df_constructs$D_LP + df_constructs$PL))
  df_constructs$CAIXA_AT <- vec_caixanorm_at
  df_constructs$CAIXA_K <- vec_caixanorm_k
  df_constructs$DIVIDENDOS_AT <- vec_divpagos_at
  df_constructs$DIVIDENDOS_K <- vec_divpagos_k
  df_constructs$QTB <- ((df_constructs$V_MERC + df_constructs$D_CP + df_constructs$D_LP)/df_constructs$AT)
  df_constructs$COB_JUROS <- (df_constructs$EBIT/df_constructs$DESP_FIN)
  df_constructs$ROA <- (df_constructs$LL/df_constructs$AT)
  df_constructs$LN_PU <- log(df_constructs$PU)
}

# Final set with 8924 observations, 260 unique firms and 44 variables
length(unique(df_constructs$Código))

df_w5 <- df_constructs

rm(list = setdiff(ls(),c("df_main","df","df_w1","df_w2","df_w5")))

######## DATA ANALYSIS #############
####################################

stargazer(df_w1[c("INV_AT","FC_AT","D_AT","CV_AT","CAIXA_AT","DIVIDENDOS_AT",
                          "INV_K","FC_K","D_K","CV_K","CAIXA_K","DIVIDENDOS_K",
                          "TAM","ALAVANC","QTB","COB_JUROS","ROA","LN_PU")],
          title = "Descriptive Statistics winsorized at 1", digits = 4)
stargazer(df_w2[c("INV_AT","FC_AT","D_AT","CV_AT","CAIXA_AT","DIVIDENDOS_AT",
                          "INV_K","FC_K","D_K","CV_K","CAIXA_K","DIVIDENDOS_K",
                          "TAM","ALAVANC","QTB","COB_JUROS","ROA","LN_PU")],
          title = "Descriptive Statistics winsorized at 2", digits = 4)
stargazer(df_w5[c("INV_AT","FC_AT","D_AT","CV_AT","CAIXA_AT","DIVIDENDOS_AT",
                          "INV_K","FC_K","D_K","CV_K","CAIXA_K","DIVIDENDOS_K",
                          "TAM","ALAVANC","QTB","COB_JUROS","ROA","LN_PU")],
          title = "Descriptive Statistics winsorized at 5", digits = 4)

plot(df_w5$COB_JUROS, df_w5$INV_AT, main="Scatterplot Example",
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
