############ Start data cleaning #################
### This step cleans missing observations and sorts out
### the best variables to work with, since there were some
### regualatory changes in brazil's accounting laws
### some variables stopped or started in different time periods
### here we find the variables with more raw volume of observations to work
df <- df_main

## Cleaning important variables
## Drop from 56160 to 25591 observations
## Drop from 702 to 593 firms
{
  df <- df[!is.na(df$AT), ]
  df <- df[!is.na(df$K), ]
  df <- df[!is.na(df$LL), ]
  df <- df[!is.na(df$D_CP), ]
  df <- df[!is.na(df$D_LP), ]
  df <- df[!is.na(df$V), ]
  
  length(unique(df$Código))
  
  rownames(df) <- 1:nrow(df)
}

## Caixa information
# Caixa EQ and CAIXA_DISP don't have observations in same periods, we just need to join then
{
  {
    info_caixa_eq <- which(!is.na(df$CAIXA_EQ) == TRUE)
    info_caixa_disp <- which(!is.na(df$CAIXA_DISP) == TRUE)
    
    which(!is.na(match(info_caixa_disp, info_caixa_eq)))
    which(!is.na(match(info_caixa_eq, info_caixa_disp)))
    which(info_caixa_disp %in% info_caixa_eq)
    which(info_caixa_eq %in% info_caixa_disp)
  }
  
  # Joining
  {
    df$CAIXA_EQ[is.na(df$CAIXA_EQ)] <- ""
    df$CAIXA_DISP[is.na(df$CAIXA_DISP)] <- ""
    
    df <- unite(df, CAIXA, c(CAIXA_EQ, CAIXA_DISP), sep="")
    
    df$CAIXA[df$CAIXA == ""] <- NA
  }
  
  # Cleaning NA's from CAIXA
  df <- df[!is.na(df$CAIXA), ]
}

## Depreciation information
### Work with dep 3
summary(df$DEP_2)
summary(df$DEP_3)

{
  length(which(is.na(df$DEP_1) == TRUE))
  length(which(is.na(df$DEP_2) == TRUE))
  length(which(is.na(df$DEP_3) == TRUE))
  
  df1 <- df
  
  df1 <- df1 %>%
    mutate(DEP = coalesce(DEP_3,DEP_2))
  
  length(which(is.na(df1$DEP) == TRUE))
  
  df1 <- df1 %>%
    mutate(DEPP = coalesce(DEP,DEP_1))
  
  length(which(is.na(df1$DEPP) == TRUE))
  rm(df1)
}

# Cleaning NA's from DEP
df <- df[,-c(19,20)]
colnames(df)[19] <- "DEP"

# At this point the data has 14419 observations and 442 firms
df <- df[!is.na(df$DEP), ]
length(unique(df$Código))

## Adjusting other variables with few missings
{
  # 53 MISSINGS
  length(which((is.na(df$QTD_ACOES) == TRUE)))
  df <- df[!is.na(df$QTD_ACOES), ]
  
  # 1 MISSING
  length(which((is.na(df$PL) == TRUE)))
  df <- df[!is.na(df$PL), ]
  
  # 270 MISSINGS 
  length(which((is.na(df$DESP_FIN) == TRUE)))
  df <- df[!is.na(df$DESP_FIN), ]
}
## Adjusting dataset order: Variables ok >> not ok, so far
df <- df[ , c(1:20,26,28,22,21,23:25,27,34,29:33)]

# Adjusting EBIT
# 1 MISSING
length(which((is.na(df$EBIT) == TRUE)))
df <- df[!is.na(df$EBIT), ]

## Re ordering
df <- df[ , c(1:22,32,23:31,33:34)]

## Columns lucro operacional and ebit/lucro not needed anymore
## Consistent information in columns EBIT and DESP_FIN
df <- df[ , -c(31:34)]

# Now we need to find dividend expenses and market value
# At this moment we have 14094 observations and 440 firms

# Market value is partially ok, cleaning we got 12107 observations and 380 firms
df <- df[!is.na(df$V_MERC), ]
# Cleaning columns which won't be used
df <- df[ , -c(30)]
# Re ordering
df <- df[ ,c(1:23,29,24,25:28)]

#### Looking at dividendos
length(which((is.na(df$DIV_PAGOS) == TRUE)))
length(which((is.na(df$DIVIDENDOS) == TRUE)))

## dIV PAGOS = Flx de caixa and DIVIDENDOS = DOAR, cheking if information is complementary
## (only 1 of then have information at a given time)
{
  info_div_pagos <- which(!is.na(df$DIV_PAGOS) == TRUE)
  info_dividendos <- which(!is.na(df$DIVIDENDOS) == TRUE)
  
  
  length(which(info_div_pagos %in% info_dividendos))
  
}
## No, it's not complementar and dividendos(doar) brings close to 0 new information


# 4994 NA'S
length(which((is.na(df$DIV_PAGOS) == TRUE)))

## 413 observations of informatio in div_por_acao to complement
## Not worth because the values doesn't match with gastos_div
{
  info_na_gastos_div <- which((is.na(df$GASTOS_DIV) == TRUE))
  info_divacao <- which((!is.na(df$DIV_POR_ACAO) == TRUE))
  
  length(which(info_divacao %in% info_na_gastos_div))
}

# Changing DIV_PAGOS NA to 0
df$DIV_PAGOS[is.na(df$DIV_PAGOS)] <- 0
sum(is.na(df$DIV_PAGOS))


# cleaning
df <- df[ , -c(27:29)]

# At this point dataset has 12107 observations to 380 firms
# Information starts from quarter 21 (1ª tri/2006)
length(unique(df$Código))
rownames(df) <- 1:nrow(df)
rm (list = setdiff(ls(),c("df_main","df")))

str(df)

df$CAIXA <- as.numeric(as.character(df$CAIXA))

str(df)

# Changing div_pagos to positive values 
df$DIV_PAGOS <- df$DIV_PAGOS * (-1)

# Cleaning negative values (V, DIV_PAGOS and PL)

# 13 drops from div_pagos
df <- df[df$DIV_PAGOS >= 0 ,]

# 20 drops from V
df <- df[df$V >= 0 ,]

# 926 drops from PL
df <- df[df$PL >= 0 ,]

## 11149 observations to 359 firms
length(unique(df$Código))
df_backup <- df
