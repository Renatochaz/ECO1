### Starting analyze of dataset
### Plot evolution of investment
setwd("G:/Meu Drive/GIT/ECO1")
full_sample <- read.csv("full_sample.csv", stringsAsFactors = FALSE)
tri_constrained_sample <- read.csv("tri_constrained_sample.csv", stringsAsFactors = FALSE)
tri_unconstrained_sample <- read.csv("tri_unconstrained_sample.csv", stringsAsFactors = FALSE)


### Looking at normalized INV median evolution through years for groups
## Creating descriptive function for Dfs of INV, with median, SD, lower and upper bound
fun_descriptive <- function(dataset) {
  
  temp_df <- dataset[,c("INV_AT","year")]
  
  temp_meandf <- temp_df %>% 
    group_by(year) %>%
    summarise_each(funs(median(., na.rm = TRUE),sd(., na.rm = TRUE)))
  
  return(temp_meandf)
}

descriptive_full <- fun_descriptive(full_sample)
descriptive_tri_constrained <- fun_descriptive(tri_constrained_sample)
descriptive_tri_unconstrained <- fun_descriptive(tri_unconstrained_sample)

## Plot structure
# setting type for labeling
descriptive_full$amostra <- "Amostra total"
descriptive_tri_constrained$amostra <- "Firmas restritas"
descriptive_tri_unconstrained$amostra <- "Não restritas"

plot_tri <- rbind(descriptive_tri_constrained,descriptive_tri_unconstrained)

# Make plot
ggplot(data=plot_tri, aes(x=year, y=median, fill=amostra, linetype=amostra, color=amostra)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks = c(seq(2006,2019, by = 2))) + 
  scale_y_continuous(limits = c(-0.03,0.07)) +
  xlab("Ano") + 
  ylab("Taxa de Investimento") +
  theme_dark() +
  scale_color_viridis(discrete = TRUE, option = "cividis")

rm(list = setdiff(ls(),c("full_sample","tri_constrained_sample","tri_unconstrained_sample")))
