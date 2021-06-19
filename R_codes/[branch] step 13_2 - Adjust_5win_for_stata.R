setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("quint_pre_yearly_full_sample.csv", stringsAsFactors = FALSE)


t <- df[,c("INV_AT","FC_AT","D_AT","CV","TAM")]
t <- lapply(t, Winsorize, probs = c (0.05, 0.95))

df_after <- df
df_after[c("INV_AT","FC_AT","D_AT","CV","TAM")] <- t[1:5]
rownames(df_after) <- 1:nrow(df_after)

boxplot(df$INV_AT)
boxplot(df_after$INV_AT)
boxplot(df$FC_AT)
boxplot(df_after$FC_AT)
boxplot(df$D_AT)
boxplot(df_after$D_AT)
boxplot(df$CV)
boxplot(df_after$CV)

## Save for stata
write.csv(df_after, "v2_wins_quint_yearly_full_sample.csv", row.names = FALSE)

