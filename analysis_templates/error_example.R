#Calculating Error 



# Population Tables
# Number of Life science faculty in the US total
# based on NCES 2016-014 data
n.faculty.total <- 100000


# Number of life science faculty in the US by sex
# based on NCES 2016-014 data

#70% male
#30% Female
n.faculty.male <- 0.70 * n.faculty.total
n.faculty.female <- 0.30 * n.faculty.total


#Ns = [ (Np) * P * (1 – P) ] / [ (Np-1) * (B/C)^2 + P * (1 – P) ]
#P = the proportion of the population expected to choose one of the two response categories
#Ns = the completed sample size
#Np = the size of the population you’re sampling from
#B = your margin of error (e.g. .03 is equal to plus or minus 3%)
#C = Z score associated with with the CI you want (1.96 for 95% confidence)


########## Question 1A
# Assume in question 1 I have faculty at Small, Medium, and Large institutions. I have no idea what 
# The population distribution is for that demographic. Can I just report some 'Overall value'?

# Number of faculty who identified in being in Small, Medium, or Large institutions 
n.small.medium.large <- 998

#calculate overall error for question 1 (small, medium, large)

#Ns = [ (Np) * P * (1 – P) ] / [ (Np-1) * (B/C)^2 + P * (1 – P) ]
#P = the proportion of the population expected to choose one of the two response categories
#Ns = the completed sample size
#Np = the size of the population you’re sampling from
#B = your margin of error (e.g. .03 is equal to plus or minus 3%)
#C = Z score associated with with the CI you want (1.96 for 95% confidence)

Ns.sample_size <-n.small.medium.large
Np.population_size <-  n.faculty.total
P.proportion_size <-  .5
C.confidence.interval <-  1.96


# Solve for B (margin of error)
numer <- (((P.proportion_size * C.confidence.interval)^2)*(1 - P.proportion_size)*((Np.population_size/ Ns.sample_size) - 1))
denom <- (Np.population_size - 1)
q1.margin_of_error <- sqrt(numer/denom)

########## Question 1B
# From the above, I assume my  margin of error is +/0 0.0218
# Does P change based on how may possible options the repondant had... i.e. should it be .333 since
# They could have identified (in my assumption) Small, Medium, or Large with equal probability?



########## Question 2 
#how many males/females answer a question where I report by Small, Medium, Large institution sizes
# Here I know what proportion of the population should be male and female, so should I report. 

# assuming I have the following

Q2.male.respondants <- 650
Q2.female.respondants <- 450


# Male Error

Ns.sample_size <-Q2.male.respondants
Np.population_size <-  n.faculty.male
P.proportion_size <-  .5
C.confidence.interval <-  1.96

numer <- (((P.proportion_size * C.confidence.interval)^2)*(1 - P.proportion_size)*((Np.population_size/ Ns.sample_size) - 1))
denom <- (Np.population_size - 1)

# ~ 0.027
q1.male.margin_of_error <- sqrt(numer/denom)

# Female Error

Ns.sample_size <-Q2.female.respondants
Np.population_size <-  n.faculty.female
P.proportion_size <-  .5
C.confidence.interval <-  1.96

numer <- (((P.proportion_size * C.confidence.interval)^2)*(1 - P.proportion_size)*((Np.population_size/ Ns.sample_size) - 1))
denom <- (Np.population_size - 1)

# ~ 0.032
q1.female.margin_of_error <- sqrt(numer/denom)


