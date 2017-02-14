#Ns = [ (Np) * P * (1 – P) ] / [ (Np-1) * (B/C)^2 + P * (1 – P) ]
#P = the proportion of the population expected to choose one of the two response categories
#Ns = the completed sample size
#Np = the size of the population you’re sampling from
#B = your margin of error (e.g. .03 is equal to plus or minus 3%)
#C = Z score associated with with the CI you want (1.96 for 95% confidence)



Ns.sample_size <-900
Np.population_size <-  100000
P.proportion_size <-  .5
C.confidence.interval <-  1.96


numer <- (((P.proportion_size * C.confidence.interval)^2)*(1 - P.proportion_size)*((Np.population_size/ Ns.sample_size) - 1))
denom <- (Np.population_size - 1)
B.margin_of_error <- sqrt(numer/denom)
  
