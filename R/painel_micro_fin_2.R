### RAW DATA ###
### 702 Firms and 56160 observations
### Variables deflated with IPCA IBGE ACUMUL - 14/07/2020
### Variables NOT deflated: EBIT_POR_DESPFIN_LIQ e EBIT_POR_DESPFIN_BRUT
### Firms with open capital, traded in Bovespa

### VARIABLES DEFINITION
# AT = ATIVO TOTAL
# K = IMOBILIZADO
# LL = LUCRO LÍQUIDO
# D_CP = PASSIVO CIRCULANTE
# D_LP = PASSIVO NÃO CIRCULANTE
# V = VENDAS (RECEITA OPERACIONAL LÍQUIDA)
# DEP_1 = DEPRECIAÇÃO (DOAR)
# DEP_2 = DEPRECIAÇÃO (FLUXO DE CAIXA)
# DEP_3 = DEPRECIAÇÃO E AMORTIZAÇÃO (INDICADOR ECONOMÁTICA) - GRANDE VARIAÇÃO EM RELAÇÃO A DEP_2
# CAIXA_EQ = CAIXA E EQUIVALANTES (FLUXO DE CAIXA)
# CAIXA_DISP = CAIXA (DISPONÍVEL E INVESTIMENTOS DE CURTO PRAZO)
# DIV_PAGOS = DIVIDENDOS PAGOS (FLUXO DE CAIXA)
# QTD_AÇÕES = QUANTIDADE DE AÇÕES
# DIVIDENDOS = GASTOS COM DIVIDENDOS (DOAR)
# DIV_YIELD = DIVIDEND YIELD (INDICADOR ECONOMÁTICA)
# DIV_POR_AÇÃO = DIVIDENDO POR AÇÃO (INDICADOR ECONOMÁTICA)
# PL = PATRIMÔNIO LÍQUIDO
# V_MERC = VALOR DE MERCADO (INDICADOR ECONOMÁTICA)
# DESP_FIN = DESPESAS FINANCEIRAS (DRE)
# LUC_OP1 = LUCRO OPERACIONAL (DRE)
# LUC_OP2 = LUCRO OPERACIONAL (ANTIGO) (DRE)
# EBIT = LUCRO ANTES DE JUROS E IMPOSTOS EBIT (DRE)
# EBIT_POR_DESPFIN_LIQ = INDICADOR ECONOMÁTICA
# EBIT_POR_DESPFIN_BRUT = INDICADOR ECONOMÁTICA
# PRECO_FECHAMENTO = PREÇO DA AÇÃO (FECHAMENTO)


### CLEANED DATA ###
### 284 FIRMS AND 10079 OBSERVATIONS
### first observations starts at 2006

### VARIABLES DEFINITION
# DEP = DEP_2
# CAIXA = JOIN FROM CAIXA_EQ AND CAIXA_DISP
# DIV_PAGOS = DIVIDENDOS PAGOS (FLUXO DE CAIXA)
# AGE = DATA DE CONSTITUIÇÃO CVM

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
  library(GetDFPData2)
}

## Setting working directory
setwd("G:/Meu Drive/USP/Mestrado/Dissertação/R_Mestrado_Principal/base_economat/Dados_raw_2")

## Starting string of base name of files
name_base_files <- c("dados_st2000_")

## Reading all data as UTF-8, data.frame, excluding first 2 columns and 
## columns as: 1 = char, 2:26 = numeric
## Creating vector of datasets names
# Setting a index column for quarters which will have value 1 to 80, indicating quarter
count_datasets <- 0

vec_names_dfs <- 0

for (i in 1:80) {
  
  db <- read.csv(paste0(name_base_files,i,".csv"), stringsAsFactors = FALSE)
  
  colnames(db) <- stri_encode(colnames(db), "", "UTF-8")
  
  db[ ,3] <- as.factor(db[ ,3])
  
  db <- db %>% mutate_if (is.character, as.numeric)
  
  db <- db %>% mutate_if (is.factor, as.character)
  
  db$Quarter <- i
  db <- db[ , c(1:3,29,4:28)]
  
  db[db == 0] <- NA
  
  count_datasets <- count_datasets +1
  
  assign(paste0("dataset",i), db[ , 3:29])
  
  vec_names_dfs <- c(vec_names_dfs, paste0("dataset",i))
  
  rm(db)
  
}

vec_names_dfs <- vec_names_dfs[-c(1)]

str(dataset1)
str(dataset80)

## Function to check for wrong information (repeated columns = wrong data collect)
## Test should be TRUE

fc_get_colnames <- function (x) {
    
    vec_names <- 0
    
    for (i in 1:count_datasets) {
   
    namedb <- paste0("dataset",i)
    tmp <- get(namedb)
    vec_names <- c(vec_names, colnames(tmp)[3:27])
    
  }
  
  return(vec_names[-c(1)])
  
}
fc_check_information <- function (x) {
  
  vec_colnames <- fc_get_colnames(x)
  
  test <- length(vec_colnames) == length(unique(vec_colnames))
  
  return (test)
  
}

fc_check_information(x)

## Changing colnames
names_cols <- c("AT","K","LL","D_CP","D_LP","V","DEP_1","DEP_2","DEP_3",
                "CAIXA_EQ","CAIXA_DISP","DIV_PAGOS","QTD_ACOES","DIVIDENDOS",
                "DIV_YIELD","DIV_POR_ACAO","PL","V_MERC","DESP_FIN","LUC_OP1","LUC_OP2",
                "EBIT","EBIT_POR_DESPFIN_LIQ","EBIT_POR_DESPFIN_BRUT","PRECO_FECHAMENTO")
for (i in 1:count_datasets) {
  
  namedb <- paste0("dataset",i)
  tmp <- get(namedb)
  
  colnames(tmp)[3:27] <- names_cols
  
  assign(paste0("dataset",i), tmp)
  
  rm(namedb, tmp)
  
}

## Function to melt datasets in to panel

fc_melt_to_panel <- function (x) {
  
  firm <- dataset1[1,]
  
  count_firms <- 0
  
  for (i in 1:nrow(dataset1)) {
  
    for (j in 1:count_datasets) {
    
    namedb <- paste0("dataset",j)
    tmp <- get(namedb)
    firm[j, ] <- tmp[i,]
  }
      assign(paste0("firm",i), firm)
      count_firms <- count_firms + 1
    }

  df_main <- rbind(firm1, firm2)  

  for (i in 3:count_firms) {
  
    df_main <- rbind(df_main, eval(as.name(paste0("firm",i))))
  
  }
  
  return(df_main)
}

df_main <- fc_melt_to_panel(x)

## Checking info information is correctly melted
## Outcome should be TRUE and 0
{
# Check correct melt length
702*80 == nrow(df_main)

# Check name orders, outcome should be 0
fc_check_name_order <- function (x) {
  
  tmp_trues <- unique(dataset1$Código) == unique(df_main$Código)
  length(tmp_trues[tmp_trues == FALSE])
}

fc_check_name_order(x)
}

## Adding PU information (variable)
fc_add_pu <- function (x) {
  
  PU <- read_excel("Brazil_Policy_Uncertainty_Data.xlsx")
  
  PU <- PU[109:348, ]
  EPU <- PU$`Brazil News-Based EPU`
  
  t <- split(EPU, ceiling(seq_along(EPU)/3))
  
  EPU_medio_quarter <- 0
  
  for (i in 1:80) {
    EPU_medio_quarter <- c(EPU_medio_quarter, sum(unlist(t[i]))/3)
  }
  
  EPU_medio_quarter <- EPU_medio_quarter[-c(1)]
  return(EPU_medio_quarter)
  
 
}

df_main$PU <- fc_add_pu(x)

## Adding firm information to dataset
{
info_empresas <- read.csv ("info_firms.csv")

info_empresas <- info_empresas[, c(12,2,11,3,7,13,9,16,10,15,14)]
colnames(info_empresas) <- c("ATIVO", "NOME", "ID_EMPRESA", "CLASSE", "SETOR_ECONOMATICA",
                             "SETOR_NAICS", "CAPITAL_ABERTO", "SEGMENTO_LISTAGEM_BOVESPA",
                             "SETOR_ECONOMICO_BOVESP", "SEGMENTO_BOVESPA", "SUBSETOR_BOVESPA")
info_empresas <- info_empresas[, -7]

tmp_trues <- unique(df_main$Código) == unique(info_empresas$ATIVO)
length(tmp_trues[tmp_trues == FALSE])

info_empresas <- info_empresas[rep(seq_len(nrow(info_empresas)), each = 80), ]

nrow(info_empresas) == nrow(df_main) 

str(info_empresas)

info_empresas <- info_empresas %>% mutate_if(is.factor, as.character)

str(info_empresas)

df_main$NOME <- info_empresas$NOME
df_main$CLASSE <- info_empresas$CLASSE
df_main$SETOR_ECONOMATICA <- info_empresas$SETOR_ECONOMATICA
df_main$SETOR_NAICS <- info_empresas$SETOR_NAICS
df_main$SEGMENTO_LIST_BOVESPA <- info_empresas$SEGMENTO_LISTAGEM_BOVESPA
df_main$SETOR_ECON_BOVESPA <- info_empresas$SETOR_ECONOMICO_BOVESP
df_main$SEGMENTO_BOVESPA <- info_empresas$SEGMENTO_BOVESPA
df_main$SUBSETOR_BOVESPA <- info_empresas$SUBSETOR_BOVESPA
df_main$ID_CVM <- info_empresas$ID_EMPRESA
df_main <- df_main[ ,c(1:2,29:37,28,3:27)]
}

rownames(df_main) <- 1:nrow(df_main)

rm (list = setdiff(ls(),c("df_main")))


## Saving anual RAW dataset to Aline
annual_aline <- function (x) {
  
  vec_annual_quarter <- seq(from = 4, to = 80, by = 4)

  for (i in 1:length(vec_annual_quarter)) {
  
    db <- filter(df_main, Quarter == vec_annual_quarter[i])
  
    assign(paste0("db_",i),db)
  }

  df_aline_annual_raw <- rbind(db_1, db_2)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_3)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_4)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_5)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_6)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_7)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_8)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_9)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_10)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_11)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_12)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_13)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_14)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_15)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_16)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_17)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_18)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_19)
  df_aline_annual_raw <- rbind(df_aline_annual_raw,db_20)
  return(df_aline_annual_raw)
}

df_aline_raw <- annual_aline(x)
df_aline_raw <- df_aline_raw[ ,-12]

df_aline_raw <- df_aline_raw[order(match(df_aline_raw$NOME,df_main$NOME)),]
df_aline_raw <- df_aline_raw[order(df_main$NOME),]
write.csv(df_aline_raw, "raw_annual_data_aline.csv", row.names = FALSE)


############ Start data cleaning #################
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
## DEP3 has 600 observations more than DEP2, but with huge values and negative ones too
## DEP1 has close to null observations
## Joining DEP2 and DEP3 add 2000 observations to DEP, but with questionable information
## Better to work with DEP2 only
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
df <- df[,-c(19,21)]
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

###### Checking sequence of quarters for each firm

# Creating vector with name of remaining firms
vec_names_firms <- unique(df$Código)

df_count <- aggregate(data.frame(count = df$Código), list(value = df$Código), length)

## if the cut is 2 years of observations minimum, loses 223 observations and 56 firms
sum(df_count[which(df_count$count < 8),][2])
nrow(df_count[which(df_count$count < 8),][1])

## if the cut is 3 years of observations minimum, loses 506 observations and 86 firms
sum(df_count[which(df_count$count < 12),][2])
nrow(df_count[which(df_count$count < 12),][1])

## if the cut is 4 years of observations minimum, loses 765 observations and 106 firms
sum(df_count[which(df_count$count < 16),][2])
nrow(df_count[which(df_count$count < 16),][1])

## if the cut is 5 years of observations minimum, loses 975 observations and 118 firms
sum(df_count[which(df_count$count < 20),][2])
nrow(df_count[which(df_count$count <20),][1])
vec_names_remove <- df_count[which(df_count$count < 8),][1]
vec_names_remove <- vec_names_remove$value

# Removing very small observations for firms (less than 2 years, or 7 quarters)
for (i in 1:length(vec_names_remove)) {
  
  df <- subset(df, !Código == vec_names_remove[i])
}

## 10926 observations to 303 firms
length(unique(df$Código))

vec_names_remove_5 <- df_count[which(df_count$count < 20),][1]
vec_names_remove_5 <- vec_names_remove_5$value

df1 <- df
for (i in 1:length(vec_names_remove_5)) {
  
  df1 <- subset(df1, !Código == vec_names_remove_5[i])
}

## 10174 observations to 241 firms
length(unique(df1$Código))

## Function to get length of sequences
somefunction <- function(x){
  
  if(!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + 1L
  i <- c(which(y|is.na(y)),n)
  
  list(
    lengths = diff(c(0L,i)),
    values = x[head(c(0L,i)+1L,-1L)]
  )
  
}

## Setting visual dataset with lengths and positions of sequences
rle <- somefunction(df$Quarter)
rownames(df) <- 1:nrow(df)

indices <- data.frame(rle$lengths, rle$values)
indices$position <- 0
indices$position[1] <- 1
indices <- indices[,c(2,1,3)]

for (i in 2:nrow(indices)) {
  
  indices$position[i] <- indices$position[i-1] + indices$rle.lengths[i-1]
  
}

## Setting positions of small sequences ( < 8)
indices_small <- subset(indices, rle.lengths < 8)

## Settings rows to be subsetted, should be the sum of rle.lengths from indices_small (847 drops)
# Function to define rows from indices_small
somefunction_2 <- function(x) {
  
    vec_positions <- 0
    
    for (i in 1:nrow(x)) {
      
    vec_positions <- c(vec_positions,seq(from = x$position[i],
                                         to = x$position[i] + x$rle.lengths[i] - 1))

}
vec_positions <- vec_positions[-c(1)]

return(vec_positions)

}

vec_positions_delete <- somefunction_2(indices_small)

df_t <- df

df_t <- df_t[-c(vec_positions_delete), ]

somefunction(df_t$Quarter)


# At this point we have 10079 observations and 284 firms
length(unique(df_t$Código))

df <- df_t
rm (list = setdiff(ls(),c("df_main","df")))


### Getting firm age - NOT USED FOR NOW
{
# Setting name of firm and base df

vec_name_firms <- unique(df$NOME)

df_age <- data.frame(vec_name_firms, AGE = 0)

df_age <- df_age[order(df_age$vec_name_firms),]

vec_ages <- c("4","96","66","21","16",
                     "20","5","51","15","17",
                     "110","14","15","15","35",
                     "111","13","13","37","48",
                     "22","20","98","14","60",
                     "109","71","65","36","8",
                     "72","38","9","13","16",
                     "11","16","11","15","46",
                     "75","24","28","75","30",
                     "30","5","22","137","54",
                     "8","16","54","35","25",
                     "49","22","70","57","19",
                     "31","31","31","31","53",
                     "22","14","16","83","54",
                     "24","13","47","54","30",
                     "84","39","265","21","17",
                     "58","58","20","51","22",
                     "24","118","23","15","16",
                     "10","20","25","41","19",
                     "22","21","70","69","18",
                     "28","94","41","52","59",
                     "33","56","63","24","14",
                     "21","59","119","16","17",
                     "23","70","49","51","34",
                     "64","11","39","43","18",
                     "14","17","41","43","4",
                     "82","14","80","31","79",
                     "54","34","67","74","14",
                     "80","100","51","87","85",
                     "42","38","13","21","16",
                     "80","47","12","47","91",
                     "61","55","14","40","26",
                     "59","63","23","92","71",
                     "20","48","38","130","67",
                     "61","70","19","78","40",
                     "28","22","73","5","14",
                     "15","10","124","108","20",
                     "33","20","6","13","6",
                     "39","22","53","23","43",
                     "17","67","49","12","56",
                     "36","58","43","48","59",
                     "38","11","11","10","10",
                     "85","28","21","91","16",
                     "14","22","71","40","23",
                     "47","57","129","22","35",
                     "77","73","57","3","18",
                     "79","20","43","17","21",
                     "66","101","86","15","64",
                     "33","14","15","81","14",
                     "23","22","81","15","22",
                     "16","46","12","27","6",
                     "17","22","22","37","21",
                     "13","21","82","67","35",
                     "51","64","78","34","63",
                     "74","45","5","9","28",
                     "13","68","59","88","48",
                     "30","74","48","13")

df_age$AGE <- vec_ages
rownames(df_age) <- 1:nrow(df_age)

df_age2 <- df_age[match(vec_name_firms, df_age$vec_name_firms),]

unique(df_age2$vec_name_firms) == unique(df$NOME)

rm(df_search,vec_ages,vec_name_firms,df_info,df_age)

t <- as.data.frame(table(df$NOME))
t <- t[match(df_age2$vec_name_firms,t$Var1),]

df_age_exp <- df_age2[rep(rownames(df_age2), t$Freq), ]

df$AGE_ini <- df_age_exp$AGE
df <- df_o

rm(t,t_exp,df_age2,df_age_exp)
}


write.csv(df, "constructed_quarterly_data_2006-2019.csv", row.names = FALSE)


