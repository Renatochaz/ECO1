## Adding PU information (variable)
fc_add_pu <- function (x) {
  
  PU <- read_excel("Brazil_Policy_Uncertainty_Data.xlsx")
  
  PU <- PU[109:348, ]
  EPU <- PU$`Brazil News-Based EPU`
  
  t <- split(EPU, ceiling(seq_along(EPU)/12))
  
  EPU_medio_year <- 0
  
  for (i in 1:20) {
    EPU_medio_year <- c(EPU_medio_year, sum(unlist(t[i]))/12)
  }
  
  EPU_medio_year <- EPU_medio_year[-c(1)]
  return(EPU_medio_year)
  
  
}
