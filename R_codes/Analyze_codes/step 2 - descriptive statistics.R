##################### Starting descriptive statistics #####################
### Load
setwd("G:/Meu Drive/GIT/ECO1")
full_sample <- read.csv("v2_quint_yearly_r_analize.csv", stringsAsFactors = FALSE)

unconstrained <- subset(full_sample, kz_quintile == 0)
constrained <- subset(full_sample, kz_quintile == 1)

## Descriptive table with latex output
stargazer(full_sample[c("PU","INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT","TAM","QTB",
                    "COB_JUROS","DIV_PL","ROA","ROE","ROK")],
          median = TRUE, digits = 4,
          omit.summary.stat = c("p25", "p75", "min", "max"))

stargazer(constrained[c("PU","INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT","TAM","QTB",
                           "COB_JUROS","DIV_PL","ROA","ROE","ROK")],
          median = TRUE, digits = 4,
          omit.summary.stat = c("p25", "p75", "min", "max"))

stargazer(unconstrained[c("PU","INV_AT","FC_AT","D_AT","CV","CAIXA_AT","DIVIDENDOS_AT","TAM","QTB",
                             "COB_JUROS","DIV_PL","ROA","ROE","ROK")],
          median = TRUE, digits = 4,
          omit.summary.stat = c("p25", "p75", "min", "max"))

# Paired sample test to check for mean differences
t.test(PU ~ kz_quintile, data = full_sample[-c(2),], paired = TRUE)
t.test(INV_AT ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(FC_AT ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(D_AT ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(CV ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(CAIXA_AT ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(DIVIDENDOS_AT ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(QTB ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(COB_JUROS ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(DIV_PL ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(ROA ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(ROE ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)
t.test(ROK ~ kz_dum, data = full_sample[-c(2),], paired = TRUE)

       