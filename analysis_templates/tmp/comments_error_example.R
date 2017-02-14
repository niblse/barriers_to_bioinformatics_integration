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


########## Question 1A
# Assume in question 1 I have faculty at Small, Medium, and Large institutions. I have no idea what 
# The population distribution is for that demographic. Can I just report some 'Overall value'?


# If your sampling fraction (sample/population) is less than 5%, you can ignore the finite sample adjustment
# ME_unadjusted = (z * sqrt(p(1-p))/sqrt(n)
# ME_adjusted = (z * sqrt(p(1-p))/sqrt(((N-1)*n)/(N-n))

# p = the proportion of the population expected to choose one of the two response categories
# n = the completed sample size
# N = the size of the population you're sampling from
# z = Z score associated with with the CI you want (1.96 for 95% confidence)

# Number of faculty who identified in being in Small, Medium, or Large institutions 
n.small.medium.large <- 998

n.sample_size <-n.small.medium.large
N.population_size <-  n.faculty.total
P.proportion_size <-  .5
z.confidence.interval <-  1.96

# unadjusted margin of error
numer <- z.confidence.interval * sqrt(P.proportion_size*(1-P.proportion_size))
denom <- sqrt(n.sample_size)
q1.margin_of_error <- numer/denom


########## Question 1B
# You can use the P you find in the actual survey if you have a binary response. Using .5 for p will give
# you the maximum margin of error. If you have multiple proportions, you can use the one closest to .5 to get
# the maximal margin for the question (e.g. a candidate A, candidate B, no preference series of options would
# use the highest of the the 3 percentages for the margin of error calculation)

# If you're calculating the margin of error for something that has an average, rather than proportional response
# then it's just z * SE of the mean of that variable.



########## Question 2 
# how exactly are you wanting to break this up? If you have the response percentage to a question
# and then want to break down those responses by subgroup, then yes, each of those gets it's own
# margin of error. However, if you want to show the proportion of males/females at a small instituion
# then small institution gender gets one margin of error, with p being the likelihood closest to .5 of 
# saying a particular gender

#how many males/females answer a question where I report by Small, Medium, Large institution sizes
# Here I know what proportion of the population should be male and female, so should I report. 

# assuming I have the following

Q2.male.respondants <- 650
Q2.female.respondants <- 450


# Male Error
n.sample_size <-Q2.male.respondants
N.population_size <-  n.faculty.male
P.proportion_size <-  .5
z.confidence.interval <-  1.96

numer <- z.confidence.interval * sqrt(P.proportion_size*(1-P.proportion_size))
denom <- sqrt(((N.population_size - 1)*n.sample_size)/(N.population_size - n.sample_size)
q1.margin_of_error <- numer/denom

3326.554/(3326.54 + 14506.123)

