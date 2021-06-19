##################### Starting descriptive statistics #####################
### Load
setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("v2_quint_yearly_r_analize.csv", stringsAsFactors = FALSE)

## Count number of observations per year and build a frequency table
freq_year <- count(df$year)
freq_year$perc <- round_any((freq_year$freq/sum(freq_year$freq))*100,0.0001,f = ceiling)

freq_year$accumulated_freq[1] <- freq_year$perc[1]
for (i in 2:nrow(freq_year)) {
  freq_year$accumulated_freq[i] <- freq_year$accumulated_freq[i-1] + freq_year$perc[i]
}
freq_year$perc <- round(freq_year$perc, digits = 2)
freq_year$accumulated_freq <- round(freq_year$accumulated_freq, digits = 2)

## Count number of observations per year and build a frequency table
freq_sector <- count(df$SETOR_ECONOMATICA)
freq_sector$perc <- round_any((freq_sector$freq/sum(freq_sector$freq))*100,0.0001,f = ceiling)

freq_sector$accumulated_freq[1] <- freq_sector$perc[1]
for (i in 2:nrow(freq_sector)) {
  freq_sector$accumulated_freq[i] <- freq_sector$accumulated_freq[i-1] + freq_sector$perc[i]
}
freq_sector$perc <- round(freq_sector$perc, digits = 2)
freq_sector$accumulated_freq <- round(freq_sector$accumulated_freq, digits = 2)


# Latex output
print(xtable(freq_year), type="latex")
print(xtable(freq_sector), type="latex")
