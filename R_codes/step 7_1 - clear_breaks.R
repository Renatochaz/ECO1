## ADD YEAR

# set years
# setting vector to identify years based on quarters
vec_q1 <- seq(21,77, by = 4)
vec_q2 <- seq(22,78, by = 4)
vec_q3 <- seq(23, 79, by = 4)
vec_q4 <- seq(24, 80, by = 4)
vec_names <- c("vec_q1","vec_q2","vec_q3","vec_q4")
vec_years <- seq(2005,2019, by = 1)
df$year <- 0


# Function to add year
fc_add_year <- function (dataset) {
  
  for (i in 1:4) {
    
    tmp_q <- get(vec_names[i])
    
    for (j in 1:15) {
      
      dataset$year[dataset$Quarter == tmp_q[j]] <- vec_years[j]
    }
  }
  return(dataset$year)
}
df$year <- fc_add_year(df)

# Subset years below 2005
df <- df[,c(1,27,2:26)]
df <- subset(df, year != 0)

####################

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

somefunction(df$year)


## Setting visual dataset with lengths and positions of sequences
rle <- somefunction(df$year)
rownames(df) <- 1:nrow(df)

indices <- data.frame(rle$lengths, rle$values)
indices$position <- 0
indices$position[1] <- 1
indices <- indices[,c(2,1,3)]

for (i in 2:nrow(indices)) {
  
  indices$position[i] <- indices$position[i-1] + indices$rle.lengths[i-1]
  
}

## Setting positions of small sequences ( < 4)
indices_small <- subset(indices, rle.lengths < 4)

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

somefunction(df_t$year)

#################### No sequences below 4

# At this point we have 2947 observations and 276 firms
length(unique(df_t$Código))

df <- df_t

setwd("G:/Meu Drive/GIT/ECO1")
write.csv(df, "constructed_yearly_data_2005-2019.csv", row.names = FALSE)

rm (list = setdiff(ls(),c("df")))
