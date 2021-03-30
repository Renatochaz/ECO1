########## CREATE YEARLY DATASET #########

### READ
setwd("G:/Meu Drive/GIT/ECO1")
df <- read.csv("pos_win_1_constructed_quarterly_data_2006-2019.csv", stringsAsFactors = FALSE)

### Identifying quarter of the year
{setting vector to identify quarters of the year (1 ~ 4)
vec_q1 <- seq(1,77, by = 4)
vec_q2 <- seq(2,78, by = 4)
vec_q3 <- seq(3, 79, by = 4)
vec_q4 <- seq(4, 80, by = 4)

vec_names <- c("vec_q1","vec_q2","vec_q3","vec_q4")

# Initializing variable
df$Quarter_index <- 0

# Function to add categorical quarter
fc_add_qindex <- function (dataset) {
  
  for (i in 1:4) {
    
    tmp_q <- get(vec_names[i])
    dataset$Quarter_index[which(dataset$Quarter %in% tmp_q)] <- i
  }
  return(dataset$Quarter_index)
}

df$Quarter_index <- fc_add_qindex(df)

# Re ordering
df <- df[,c(1:2,47,3:46)]

# Transforming categorical quarter and sector to factor
str(df)

df$Quarter_index <- as.factor(df$Quarter_index)
df$SETOR_ECON_BOVESPA <- as.factor(df$SETOR_ECON_BOVESPA)

str(df)

# Checking contrast matrix encoded by R to those categorical vars
head(model.matrix(~Quarter_index, data = df))
head(model.matrix(~SETOR_ECON_BOVESPA, data = df))

rm (list = setdiff(ls(),c("df")))
}

### Adding year
{
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

# Re ordering
df <- df[,c(1:2,48,3:47)]
rm (list = setdiff(ls(),c("df")))
}

### Subsetting annual data
df_year <- subset(df, Quarter_index == 4)
