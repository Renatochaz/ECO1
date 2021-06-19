### Load
setwd("G:/Meu Drive/GIT/ECO1")
full <- read.csv("v2_quint_yearly_r_analize.csv", stringsAsFactors = FALSE)

# Adjust datasets
full$SETOR_ECONOMATICA <- as.factor(full$SETOR_ECONOMATICA)
full$SQinv_atL1 <- full$INV_AT_L1 * full$INV_AT_L1

unconstrained <- subset(full, kz_quintile == 0)
constrained <- subset(full, kz_quintile == 1)
### Base line OLS and FE estimations with latex baseline output

## OLS incomplete model
ols_full_semi <- lm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                    data = full)
ols_const_semi <- lm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                     data = constrained)
ols_unconst_semi <- lm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                       data = unconstrained)

## OLS FULL model
ols_full <- lm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
               data = full)
ols_const <- lm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                data = constrained)
ols_unconst<- lm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                 data = unconstrained)


## FE incomplete model
# Note: coeftest for true summary (robust stds's)
# coeftest(fe_full_semi, vcov = vcovHC, type = "HC1")
fe_full_semi <- plm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                    data = full, index = c("Código","year"), model = "within")

fe_const_semi <- plm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                     data = constrained, index = c("Código","year"), model = "within")

fe_unconst_semi <- plm(INV_AT ~ LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                       data = unconstrained, index = c("Código","year"), model = "within")


## FE FULL model
fe_full <- plm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
               data = full, index = c("Código","year"), model = "within")

fe_const <- plm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                data = constrained, index = c("Código","year"), model = "within")

fe_unconst <- plm(INV_AT ~ INV_AT_L1 + SQinv_atL1 + LN_PU + FC_AT + CV + D_AT + TAM + SETOR_ECONOMATICA,
                  data = unconstrained, index = c("Código","year"), model = "within")


########## Stargazer baseline for latex output
stargazer(ols_full_semi, ols_full, ols_const_semi, ols_const,
          ols_unconst_semi, ols_unconst,
          align = TRUE, no.space = TRUE, digits = 4,
          covariate.labels = c("$INV_{i,t-1}$",
                               "$INV_{i,t-1}^{2}$",
                               "$log PU_{i}$",
                               "$FC_{i,t}$",
                               "$CV_{it}$",
                               "$D_{it}$",
                               "$TAM_{it}$"),
          column.labels = c("Amostra total","Restritas","Não restritas"),
          column.separate = c(2,2,2),
          dep.var.caption = "",
          dep.var.labels.include = FALSE, flip = TRUE, float = FALSE,
          header = FALSE,
          keep = c("INV_AT_L1","SQinv_atL1","LN_PU","FC_AT","CV","D_AT","TAM"),
          model.names = TRUE,
          omit.stat=c("f","ser"),
          omit.table.layout = "n"
)

stargazer((coeftest(fe_full_semi, vcov. = vcovHC, type = "HC1")),
          (coeftest(fe_full, vcov. = vcovHC, type = "HC1")),
          (coeftest(fe_const_semi, vcov. = vcovHC, type = "HC1")),
          (coeftest(fe_const, vcov. = vcovHC, type = "HC1")),
          (coeftest(fe_unconst_semi, vcov. = vcovHC, type = "HC1")),
          (coeftest(fe_unconst, vcov. = vcovHC, type = "HC1")),
          align = TRUE, no.space = TRUE, digits = 4,
          covariate.labels = c("$INV_{i,t-1}$",
                               "$INV_{i,t-1}^{2}$",
                               "$log PU_{i}$",
                               "$FC_{i,t}$",
                               "$CV_{it}$",
                               "$D_{it}$",
                               "$TAM_{it}$"),
          column.labels = c("Amostra total","Restritas","Não restritas"),
          column.separate = c(2,2,2),
          dep.var.caption = "",
          dep.var.labels.include = FALSE, flip = TRUE, float = FALSE,
          header = FALSE,
          keep = c("INV_AT_L1","SQinv_atL1","LN_PU","FC_AT","CV","D_AT","TAM","Constant"),
          model.names = TRUE,
          omit.stat=c("f","ser"),
          omit.table.layout = "n"
)


### BASE for gmm latex
stargazer(ols_full_semi, ols_full, ols_const_semi,
          align = TRUE, no.space = TRUE, digits = 4,
          covariate.labels = c("$\\left ( INV_{i,t-1}/AT_{i,t-2} \\right )$",
                               "$\\left ( INV_{i,t-1}/AT_{i,t-2} \\right )^{2}$",
                               "$\\log PU_{i}$",
                               "$\\left ( FC/AT_{i,t-1} \\right )_{it}$",
                               "$CV_{it}$",
                               "$\\left ( D/AT_{i,t-1} \\right )_{it}$",
                               "$TAM_{it}$"),
          column.labels = c("Amostra total","Restritas","Não restritas"),
          column.separate = c(1,1,1),
          dep.var.caption = "",
          dep.var.labels.include = FALSE, flip = TRUE, float = FALSE,
          header = FALSE,
          keep = c("INV_AT_L1","INV_AT_L1_QUAD","LN_PU","FC_AT","CV","D_AT","TAM","Constant"),
          model.names = TRUE,
          omit.stat=c("f","ser"),
          omit.table.layout = "n"
)
