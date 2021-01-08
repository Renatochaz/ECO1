###### Checking sequence of quarters for each firm
### At this point, its important for the methodology used in the research (GMM)
### To have a minimum sequence of observations for each firm in the panel
### this step analyzes and sets the minimum to 8 quarters

# Creating vector with name of remaining firms
vec_names_firms <- unique(df$Código)

df_count <- aggregate(data.frame(count = df$Código), list(value = df$Código), length)

## if the cut is 2 years of observations minimum, loses 223 observations and 56 firms
sum(df_count[which(df_count$count < 8),][2])
nrow(df_count[which(df_count$count < 8),][1])

## if the cut is 3 years of observations minimum, loses 506 observations and 86 firms
sum(df_count[which(df_count$count < 12),][2])
nrow(df_count[which(df_count$count < 12),][1])

## if the cut is 4 years of observations minimum, loses 765 observations and 106 firms
sum(df_count[which(df_count$count < 16),][2])
nrow(df_count[which(df_count$count < 16),][1])

## if the cut is 5 years of observations minimum, loses 975 observations and 118 firms
sum(df_count[which(df_count$count < 20),][2])
nrow(df_count[which(df_count$count <20),][1])
vec_names_remove <- df_count[which(df_count$count < 8),][1]
vec_names_remove <- vec_names_remove$value

# Removing very small observations for firms (less than 2 years, or 7 quarters)
for (i in 1:length(vec_names_remove)) {
  
  df <- subset(df, !Código == vec_names_remove[i])
}

## 10926 observations to 303 firms
length(unique(df$Código))

vec_names_remove_5 <- df_count[which(df_count$count < 20),][1]
vec_names_remove_5 <- vec_names_remove_5$value

df1 <- df
for (i in 1:length(vec_names_remove_5)) {
  
  df1 <- subset(df1, !Código == vec_names_remove_5[i])
}

## 10174 observations to 241 firms
length(unique(df1$Código))

## Function to get length of sequences
somefunction <- function(x){
  
  if(!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + 1L
  i <- c(which(y|is.na(y)),n)
  
  list(
    lengths = diff(c(0L,i)),
    values = x[head(c(0L,i)+1L,-1L)]
  )
  
}

## Setting visual dataset with lengths and positions of sequences
rle <- somefunction(df$Quarter)
rownames(df) <- 1:nrow(df)

indices <- data.frame(rle$lengths, rle$values)
indices$position <- 0
indices$position[1] <- 1
indices <- indices[,c(2,1,3)]

for (i in 2:nrow(indices)) {
  
  indices$position[i] <- indices$position[i-1] + indices$rle.lengths[i-1]
  
}

## Setting positions of small sequences ( < 8)
indices_small <- subset(indices, rle.lengths < 8)

## Settings rows to be subsetted, should be the sum of rle.lengths from indices_small (847 drops)
# Function to define rows from indices_small
somefunction_2 <- function(x) {
  
  vec_positions <- 0
  
  for (i in 1:nrow(x)) {
    
    vec_positions <- c(vec_positions,seq(from = x$position[i],
                                         to = x$position[i] + x$rle.lengths[i] - 1))
    
  }
  vec_positions <- vec_positions[-c(1)]
  
  return(vec_positions)
  
}

vec_positions_delete <- somefunction_2(indices_small)

df_t <- df

df_t <- df_t[-c(vec_positions_delete), ]

somefunction(df_t$Quarter)


# At this point we have 10079 observations and 284 firms
length(unique(df_t$Código))

df <- df_t
rm (list = setdiff(ls(),c("df")))

setwd("G:/Meu Drive/GIT/ECO1")
write.csv(df, "constructed_quarterly_data_2006-2019.csv", row.names = FALSE)

setwd("G:/Meu Drive/GIT/ECO1/R_codes")
