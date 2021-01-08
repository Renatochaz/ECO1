### Melting datasets to integrated panel data and cheking integrity
### of information

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

# Check correct melt length
 702*80 == nrow(df_main)
  
# Check name orders, outcome should be 0
fc_check_name_order <- function (x) {
    
  tmp_trues <- unique(dataset1$Código) == unique(df_main$Código)
  length(tmp_trues[tmp_trues == FALSE])
}
  
fc_check_name_order(x)

