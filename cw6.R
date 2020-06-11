#PRZEDZIAŁY UFNOŚCI

# zadanie 1 ---------------------------------------------------------------
load("Centrala.RData")

#rozkład Poissona
#estymator
library(EnvStats)
p_est <- EnvStats::epois(Centrala$Liczba)$parameters

#przedział ufności
b_conf_int <- function(x, conf_level = 0.95) {
  u <- EnvStats::epois(
    x, 
    ci = TRUE, 
    ci.method = "pearson",
    conf.level = conf_level
  )$interval$limits
  
  return(c(u))
}
b_conf_int(Centrala$Liczba)
