## Changing colnames for all datasets
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
