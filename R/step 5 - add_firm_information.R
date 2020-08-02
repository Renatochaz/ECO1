### Adding information of firms of external archieve
### Checking integrity of information and cleaning the global enviro

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
