### Creating variables (see information_variables and table_description to check names) to work in research

setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("constructed_yearly_data_2005-2019.csv", stringsAsFactors = FALSE)
str(df)

### Subsetting finance sector (bovespa classification)
### 9225 observations and 260 firms
df <- subset(df, !SETOR_ECON_BOVESPA == "Financeiro")
length(unique(df$Código))
rownames(df) <- 1:nrow(df)

### Function for constructing variables with single operation (normal and defased) and normalizing dividend (defased)
fc_construct_1 <- function(df, var1,var2,var3,operator) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"year"] - df[i-1,"year"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
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

### Function for constructing variables with single operation (normal) and normalizing dividend (defased)
fc_construct_2 <- function(df, var1,var2,var3,operator) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"year"] - df[i-1,"year"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
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

### Function for constructing variables with single variable and normalizing dividend (defased)
fc_construct_3 <- function(df, var1,var2) {
  
  vec_tmp <- 0
  vec_skips <- 0
  
  for (i in 1:nrow(df)) {
    
    if (length(which(((df[i,"year"] - df[i-1,"year"]) == 1) & (df[i,"Código"] == df[i-1,"Código"]))) != 0) {
      
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

## Change year location to adjust variable index below
df <- df[,c(1,3:27,2)]
# Function to create all variables
fc_construct_all <- function (dataset) {
  
  vec_at <- fc_construct_1(dataset,13,13,13,"-")
  vec_k <- fc_construct_1(dataset,14,14,14,"-")
  vec_cv <- fc_construct_1(dataset,18,18,18,"-")
  vec_fc_at <- fc_construct_2(dataset,15,19,13,"+")
  vec_fc_k <- fc_construct_2(dataset,15,19,14,"+")
  vec_divida_at <- fc_construct_2(dataset,16,17,13,"+")
  vec_divida_k <- fc_construct_2(dataset,16,17,14,"+")
  vec_caixanorm_at <- fc_construct_3(dataset,20,13)
  vec_caixanorm_k <- fc_construct_3(dataset,20,14)
  vec_divpagos_at <- fc_construct_3(dataset,26,13)
  vec_divpagos_k <- fc_construct_3(dataset,26,14)
  vec_divpl <- fc_construct_3(dataset,15,14)
  
  ## Subsetting skipped observations
  dataset <- dataset[-c(vec_skips),]
  rownames(dataset) <- 1:nrow(dataset)
  
  # Adding constructed variables
  {
    dataset$INV_AT <- vec_at
    dataset$FC_AT <- vec_fc_at
    dataset$D_AT <- vec_divida_at
    dataset$CV <- vec_cv
    dataset$TAM <- log(dataset$AT)
    dataset$INV_K <- vec_k
    dataset$FC_K <- vec_fc_k
    dataset$D_K <- vec_divida_k
    dataset$ALAVANC <- ((dataset$D_CP + dataset$D_LP)/(dataset$D_CP + dataset$D_LP + dataset$PL))
    dataset$CAIXA_AT <- vec_caixanorm_at
    dataset$CAIXA_K <- vec_caixanorm_k
    dataset$DIVIDENDOS_AT <- vec_divpagos_at
    dataset$DIVIDENDOS_K <- vec_divpagos_k
    dataset$QTB <- ((dataset$V_MERC + dataset$D_CP + dataset$D_LP)/dataset$AT)
    dataset$COB_JUROS <- (dataset$EBIT/dataset$DESP_FIN)
    dataset$ROA <- (dataset$LL/dataset$AT)
    dataset$LN_PU <- log(dataset$PU)
    dataset$DIV_PL <- (dataset$D_CP + dataset$D_LP)/dataset$PL
    dataset$ROE <- (dataset$LL/dataset$PL)
    dataset$ROK <- vec_divpl
  }
  
  return(dataset)
  
}

# Creating datasets with constructed variables
df_after <- fc_construct_all(df)
