# Select end quarters representing yearly data
quarters_keep <- seq(4,80, by = 4)
df_main <- df_main[df_main$Quarter %in% quarters_keep,]

# add PU information
setwd("G:/Meu Drive/GIT/ECO1/raw_data")
df_main$PU <- 0
df_main$PU <- fc_add_pu(x)
rm (list = setdiff(ls(),c("df_main")))
