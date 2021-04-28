#Load
setwd("G:/Meu Drive/GIT/ECO1")
pre <- read.csv("pre_yearly_full_sample.csv", stringsAsFactors = FALSE)

# Generate quintiles
quantile(pre$kz, probs = seq(0.2, 1, 0.2))

# We dont want the middile quntile, so we need to drop every obs
# that have a KZ between 1.0418 ~ 1.5835 as undefinied (2)
# We will classify everything below 1.0418 as unconstrained (0) and everything
# greater than 1.5835 as constrained (1)

pre$kz_quintile <- 2

kz_lower <- 1.0418
kz_upper <- 1.5835


for (i in 1:nrow(pre)) {
  
  if (pre$kz[i] < kz_lower) {
  
  pre$kz_quintile[i] <- 0
  
  } else if (pre$kz[i] > kz_upper) {
    
    pre$kz_quintile[i] <- 1
    
  } else {
    
    next
    
  }
}

# Adjust column orders
pre <- pre[,c(1,30,2:3,15,51,4,5:14,16:29,31:50)]

# Count number of drops (undefinied constraint) 483 drops
length(which(pre$kz_quintile == 2))

write.csv(pre, "quint_pre_yearly_full_sample.csv", row.names = FALSE)
