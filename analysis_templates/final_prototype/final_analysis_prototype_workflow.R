require(ggplot2)
require(tidyverse)
require(reshape2)
require(pwr)


# Analysis workflow...

#0 Load a dataframes which already contains summarized data. 
#a. Load summary table from analysis of Question 38 stratafied by Question 21 demographic
df.summary <- read_csv("./sum_table_Q38_barriers_to_inclusion_by_Q21_carnegie_classification.csv")


#1 Error calculation #######################################################################################################

# If your sampling fraction (sample/population) is less than 5%, you can ignore the finite sample adjustment
# ME_unadjusted = (z * sqrt(p(1-p))/sqrt(n)
# ME_adjusted = (z * sqrt(p(1-p))/sqrt(((N-1)*n)/(N-n))

# p = the proportion of the population expected to choose one of the two response categories
# n = the completed sample size
# N = the size of the population you're sampling from
# z = Z score associated with with the CI you want (1.96 for 95% confidence)

#a. Add n to dataframe -value of n (number of people completing the response)
df.summary <- df.summary%>%
  group_by(Var2)%>%
  mutate(n.sample = sum(responses))

#b get the overall sample size
n.sample <- as.numeric(levels(as.factor(df.summary$n.sample)))

#c set N (population sample size) to a pre-determined value (total number of life science faculty in US), proportion size, and confidence interval. 
N.population <- 100000
P.proportion_size <-  .5 
z.confidence.interval <-  1.96


#calculate error as a percentage
numer <- z.confidence.interval * sqrt(P.proportion_size*(1-P.proportion_size))
denom <- sqrt(((N.population - 1)*n.sample)/(N.population - n.sample))
error.percentage <- numer/denom


#d add error percentages to dataframe
df.summary <- df.summary%>%
  mutate(ymax = proportion + (proportion * error.percentage))%>%
  mutate(ymin = proportion - (proportion * error.percentage))

#plot s subset for fun
head8.df.summary <- df.summary%>%
  head(8)

limits <- aes(ymax = head8.df.summary$ymax, ymin = head8.df.summary$ymin)
error.dodge <- position_dodge(width=0.9)
df.summary%>%
  head(8)%>%
ggplot()+
  aes(x=Var2, y=proportion, fill=nice_names)+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Barrier", 
       y = "proportion of respondants", 
       title = "testplot",
       subtitle = paste("Shown as proportion of users within each",
                        "Institution Type Q21",
                        "n=",n.sample ))+
  geom_errorbar(limits,position = error.dodge, width = .2)+
  theme_minimal()

# Error bards on plot are not very interesting, but I will use the proceedue when I have a facet plot that has differing sample sizes. 



########## ERROR TESTING ON PROPORTIONS

df.summary.error <- df.summary%>%
  mutate(proportion_error = as.numeric(sqrt((proportion * (1 - proportion)/responses))*qnorm(.975)))%>%
  mutate(ymax = proportion + (proportion * proportion_error))%>%
  mutate(ymin = proportion - (proportion * proportion_error))

#plot s subset for fun
head8.df.summary <- df.summary.error%>%
  head(8)

limits <- aes(ymax = head8.df.summary$ymax, ymin = head8.df.summary$ymin)
error.dodge <- position_dodge(width=0.9)
df.summary%>%
  head(8)%>%
  ggplot()+
  aes(x=Var2, y=proportion, fill=nice_names)+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Barrier", 
       y = "proportion of respondants", 
       title = "testplot",
       subtitle = paste("Shown as proportion of users within each",
                        "Institution Type Q21",
                        "n=",n.sample ))+
  geom_errorbar(limits,position = error.dodge, width = .2)+
  theme_minimal()



#2 Proportion Testing  #######################################################################################################

#a contigency table is the number of respondants who we scored as reporting a particular barrier (positive_cored_response) and those who provided
# no response, and total number of responses ($responses)

#Create contigency table and save in a temporary variable
subset.proportion.test <- df.summary %>%
  select(nice_names, responses, positive_scored_response, null_scored_response)%>%
  head(4)

View(subset.proportion.test)

#b Proportion test is calculated as:
proportion.test.results <- prop.test(subset.proportion.test$positive_scored_response, subset.proportion.test$responses)

# view p values
proportion.test.results$p.value

#This test has already been done and is represented in the data frame as
df.summary$err.prop_test_chi_pvalue


#Corrrect


#3 Power testing #######################################################################################################
# Certiantly need help here!
# Stepping back here to state my context and assumptions
# 966 people (n.sample) told us their carnegie classification
# At least 160 people answered a free text question on what their 'barrier' was, education, class size, etc. 
# Some people decided not to answer the question because they experienced no barrier, or they quit the survey (Rather hard to tell this)
# df.summary$summed_score shows the total number of individuals* who responded and were catagroized into a particular barrier
# * An individual may have been coded in more than one barrier, but no individual was counted twice in the same barrier
# The numbers in df.summary$summed_score are a lower-bound of my completed sample size
# The upper-bound of my completed sample size is 966, which assumes that everyone answered this question (where null is a valid answer)
# The upper-bound is not conservative, so I propose the following, that we assume the higest-number is the conservative-upper-bound, in this case 160 individuals
# Using the conservative-upper-bound, reject barrier codings which have insufficent power to detect a medium effect (0.5) at power p = 0.95

#sample power calculations


n.power.sample <- 41
pwr.2p.test(power = 0.95,
            n = n.power.sample, 
            sig.level = 0.05)

# Assuming my understanding is correct, we need at least 40 individuals in our sample before we report that we had power to detect at least a large (0.8) effect. So, 

#run power analysis

n.completed.barriers <- max(df.summary$summed_score)

df.summary.power <- df.summary%>%
  group_by(Var2)%>%
  do(power = pwr.2p.test(power =.95, n = .$summed_score[[1]])$h)%>%
  mutate(power = round(power, 2))

# sufficently powered categories (.8 or less)
df.summary.power <- df.summary.power%>%
  filter(power <= .8)

# Therefore, I should keep proportion tests where I had sufficent power to detect at least large effects, and chi-squared p values of 0.05 or less
results <- df.summary%>%
  filter (Var2 %in% df.summary.power$Var2)%>%
  filter (prop_test_chi_pvalue <= 0.05)

