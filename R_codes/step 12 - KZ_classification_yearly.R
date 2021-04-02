## Construct KZ separated databases (Constrained and unconstrained firms)
## Construct two adittional variables

setwd("G:/Meu Drive/GIT/ECO1")
full_sample <- read.csv("pos_win_1_constructed_yearly_data_2005-2019.csv", stringsAsFactors = FALSE)

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
somefunction(full_sample$year)

  # set kz vector and construct
  full_sample$kz <- 0
  
  full_sample$kz <- -(1.001909 * full_sample$FC_AT) + (0.2826389 * full_sample$QTB) + (3.139193 * full_sample$ALAVANC) - (39.3678 * full_sample$DIVIDENDOS_AT) - (1.314759 * full_sample$CAIXA_AT)
  
  # set cut point (median) and dummy to indicate financial constrain
  # 1 = financial constrained, 0 otherwise
  summary(full_sample$kz)
  kz_median <- 1.333 
  
  full_sample$kz_dum <- 0
  
  full_sample$kz_dum <- ifelse(full_sample$kz >= kz_median, 1, 0) 

# Add sector index number, adjust order of data 
full_sample$SETOR_NUM <- as.numeric(as.factor(full_sample$SETOR_ECONOMATICA)) 
full_sample <- full_sample[,c(1:3,49:50,4:12,48,13:47)]

########### Only if needed
# subset sample in constrained and unconstrained
#constrained_sample <- subset(full_sample, kz_dum == 1)
#unconstrained_sample <- subset(full_sample, kz_dum == 0)
### Now we need to check the consistency of sequences of observations
### problem of inconsistency > lots of gaps in time observations
#somefunction(full_sample$Quarter)
#somefunction(constrained_sample$Quarter)
#somefunction(unconstrained_sample$Quarter)
#write.csv(constrained_sample, "tri_constrained_sample.csv", row.names = FALSE)
#write.csv(unconstrained_sample, "tri_unconstrained_sample.csv", row.names = FALSE)
######


write.csv(full_sample, "yearly_full_sample.csv", row.names = FALSE)
  
rm(list = ls()) 
