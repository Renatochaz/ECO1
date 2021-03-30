##################### Starting descriptive statistics #####################

### Load
setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("full_sample.csv", stringsAsFactors = FALSE)

# slice to correlation
df_slice <- df[,c("INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT","TAM","QTB",
                  "COB_JUROS","DIV_PL","PU","ROA","ROE","ROK")]
fcorr <- round(cor(df_slice),2)
fcorr

# logical matrix for correlation triangulation
upper.tri(fcorr)

# hide upper triangle and importe to latex
upper <- fcorr
upper[upper.tri(fcorr)] <- ""
upper <- as.data.frame(upper)
upper
print(xtable(upper), type="latex")
