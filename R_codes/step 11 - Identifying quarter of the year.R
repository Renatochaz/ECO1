### Setting categorical variables for quarters and sectors

# setting vector to identify quarters of the year (1 ~ 4)
vec_q1 <- seq(1,77, by = 4)
vec_q2 <- seq(2,78, by = 4)
vec_q3 <- seq(3, 79, by = 4)
vec_q4 <- seq(4, 80, by = 4)

vec_names <- c("vec_q1","vec_q2","vec_q3","vec_q4")

# Initializing variable
df_pos1_lag$Quarter_index <- 0

# Function to add categorical quarter
fc_add_qindex <- function (dataset) {
  
  for (i in 1:4) {
    
    tmp_q <- get(vec_names[i])
    dataset$Quarter_index[which(dataset$Quarter %in% tmp_q)] <- i
  }
  return(dataset$Quarter_index)
}

df_pos1_lag$Quarter_index <- fc_add_qindex(df_pos1_lag)

# Re ordering
df_pos1_lag <- df_pos1_lag[,c(1:2,53,3:52)]

# Transforming categorical quarter and sector to factor
str(df_pos1_lag)

df_pos1_lag$Quarter_index <- as.factor(df_pos1_lag$Quarter_index)
df_pos1_lag$SETOR_ECON_BOVESPA <- as.factor(df_pos1_lag$SETOR_ECON_BOVESPA)

str(df_pos1_lag)

# Checking contrast matrix encoded by R to those categorical vars
head(model.matrix(~Quarter_index, data = df_pos1_lag))
head(model.matrix(~SETOR_ECON_BOVESPA, data = df_pos1_lag))

full_sample <- df_pos1_lag
rm (list = setdiff(ls(),c("full_sample")))
