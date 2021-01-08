# Table of variable definitions

tab_vars <- data.frame(Abreviatura = colnames(full_sample[,c(13:32,37:38,40,42:44)]), Definição = NA)
tab_vars <- tab_vars[c(26,2:25),]
tab_vars$Definição[1] <- "Log do Índice de Incerteza Econômica Política (EPU)"
tab_vars$Abreviatura <- as.character(tab_vars$Abreviatura)
tab_vars$Abreviatura[1] <- "EPU"

tab_vars$Definição[2] <- "Ativo Total"
tab_vars$Definição[3] <- "Estoque de Capital, medido pelo Imobilizado"
tab_vars$Definição[4] <- "Lucro Líquido"
tab_vars$Definição[5] <- "Dívida de curto prazo, medida pelo passivo circulante"
tab_vars$Definição[6] <- "Dívida de longo prazo, medida pelo passivo exigível em longo prazo"
tab_vars$Definição[7] <- "Vendas, medida pela receita operacional líquida"
tab_vars$Definição[8] <- "Depreciação e amortização"
tab_vars$Definição[9] <- "Caixa disponível e equivalentes de caixa"
tab_vars$Definição[10] <- "Patrimônio Líquido"
tab_vars$Definição[11] <- "Despesas financeiras"
tab_vars$Definição[12] <- "EBIT (lucro antes de juros e impostos)"
tab_vars$Definição[13] <- "Valor de mercado"
tab_vars$Definição[14] <- "Quantidade de ações"
tab_vars$Definição[15] <- "Dividendos pagos"

tab_vars$Abreviatura[16] <- "I"
tab_vars$Abreviatura[17] <- "FC"
tab_vars$Abreviatura[18] <- "D"
tab_vars$Abreviatura[19] <- "CV"
tab_vars$Abreviatura[22] <- "CX"
tab_vars$Abreviatura[23] <- "DIV"
tab_vars <- tab_vars %>% add_row(Abreviatura = "Q", Definição = NA)

tab_vars$Definição[16] <- "Investimento, medido como (AT - AT) / AT"
tab_vars$Definição[17] <- "Fluxo de caixa, medido como (LL + DEP) / AT"
tab_vars$Definição[18] <- "Dívida total, medida como (D_CP + D_LP) / AT"
tab_vars$Definição[19] <- "Crescimendo de vendas, medida como (V - V) / V"
tab_vars$Definição[20] <- "Tamanho da firma, medido como logaritmo de AT"
tab_vars$Definição[21] <- "Alavancagem, medida como (D_CP + D_LP) / (D_CP + D_LP + PL)"
tab_vars$Definição[22] <- "Caixa normalizado pelo ativo total"
tab_vars$Definição[23] <- "Dividendos pagos normalizado pelo ativo total"
tab_vars$Definição[26] <- "Q de tobin, medido como (V_MERC + D_CP + D_LP)/AT"
tab_vars$Definição[24] <- "Cobertura de juros, medido como EBIT/DESP_FIN"
tab_vars$Definição[25] <- "Retorno sobre ativo, medido como LL/AT"

stargazer(tab_vars)
print(xtable(tab_vars, type = "latex"))

