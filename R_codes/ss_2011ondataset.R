### load data
setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("full_sample.csv", stringsAsFactors = FALSE)

# SUbset from 2011 onwards

year_to_keep = seq(2011,2019,by=1)
df <- df[df$year %in% year_to_keep,]

length(unique(df$Código))

write.csv(df, "2011on_full_sample.csv", row.names = FALSE)
