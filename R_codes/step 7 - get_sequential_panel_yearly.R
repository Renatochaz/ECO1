###### Checking sequence of quarters for each firm
### At this point, its important for the methodology used in the research (GMM)
### To have a minimum sequence of observations for each firm in the panel
### this step analyzes and sets the minimum to 8 quarters

# Creating vector with name of remaining firms
vec_names_firms <- unique(df$Código)

df_count <- aggregate(data.frame(count = df$Código), list(value = df$Código), length)

## if the cut is 2 years of observations minimum, loses 223 observations and 56 firms
sum(df_count[which(df_count$count < 2),][2])
nrow(df_count[which(df_count$count < 2),][1])

## if the cut is 3 years of observations minimum, loses 506 observations and 86 firms
sum(df_count[which(df_count$count < 3),][2])
nrow(df_count[which(df_count$count < 3),][1])

## if the cut is 4 years of observations minimum, loses 765 observations and 106 firms
sum(df_count[which(df_count$count < 4),][2])
nrow(df_count[which(df_count$count < 4),][1])

## if the cut is 5 years of observations minimum, loses 975 observations and 118 firms
sum(df_count[which(df_count$count < 5),][2])
nrow(df_count[which(df_count$count <5),][1])

## if the cut is 5 years of observations minimum, loses 975 observations and 118 firms
sum(df_count[which(df_count$count < 6),][2])
nrow(df_count[which(df_count$count <6),][1])

vec_names_remove <- df_count[which(df_count$count < 4),][1]
vec_names_remove <- vec_names_remove$value

# Removing very small observations for firms (less than 4 years)
for (i in 1:length(vec_names_remove)) {
  
  df <- subset(df, !Código == vec_names_remove[i])
}

## 10926 observations to 303 firms
length(unique(df$Código))

