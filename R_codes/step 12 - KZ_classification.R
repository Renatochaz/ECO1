## Construct KZ separated databases (Constrained and unconstrained firms)
## Construct two adittional variables

# Function to get length of sequences
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
somefunction(full_sample$Quarter)

### This sections creates KZ indexes quarterly, but this make the data 
### gapped, analyzes needs to be made with caution by advanced users
{
  
  # set years
  # setting vector to identify years based on quarters
  vec_q1 <- seq(21,77, by = 4)
  vec_q2 <- seq(22,78, by = 4)
  vec_q3 <- seq(23, 79, by = 4)
  vec_q4 <- seq(24, 80, by = 4)
  vec_names <- c("vec_q1","vec_q2","vec_q3","vec_q4")
  vec_years <- seq(2005,2019, by = 1)
  full_sample$year <- 0
  
  
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
  full_sample$year <- fc_add_year(full_sample)
  
  # set kz vector and construct
  full_sample$kz <- 0
  
  full_sample$kz <- -(1.001909 * full_sample$FC_AT) + (0.2826389 * full_sample$QTB) + (3.139193 * full_sample$ALAVANC) - (39.3678 * full_sample$DIVIDENDOS_AT) - (1.314759 * full_sample$CAIXA_AT)
  
  # set cut point (median) and dummy to indicate financial constrain
  # 1 = financial constrained, 0 otherwise
  summary(full_sample$kz)
  kz_median <- 1.6053 
  
  full_sample$kz_dum <- 0
  
  full_sample$kz_dum <- ifelse(full_sample$kz >= kz_median, 1, 0) 
}
# Add sector index number, adjust order of data 
full_sample$SETOR_NUM <- as.numeric(as.factor(full_sample$SETOR_ECONOMATICA)) 
full_sample <- full_sample[,c(1,48,2:3,50,51,4:12,49,13:47)]

# subset sample in constrained and unconstrained
constrained_sample <- subset(full_sample, kz_dum == 1)
unconstrained_sample <- subset(full_sample, kz_dum == 0)
  
### Now we need to check the consistency of sequences of observations
### problem of inconsistency > lots of gaps in time observations
somefunction(full_sample$Quarter)
somefunction(constrained_sample$Quarter)
somefunction(unconstrained_sample$Quarter)



write.csv(full_sample, "full_sample.csv", row.names = FALSE)
write.csv(constrained_sample, "tri_constrained_sample.csv", row.names = FALSE)
write.csv(unconstrained_sample, "tri_unconstrained_sample.csv", row.names = FALSE)
  
rm(list = ls()) 
