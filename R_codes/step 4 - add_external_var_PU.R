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
