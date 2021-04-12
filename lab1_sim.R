
#############################################
# PS 207
# Week 2: Simulation
# Date: April 6, 2021
#############################################

library(tidyverse)
library(ggthemes)

# The goal of this simulation is to examine two statistical properties
# of the difference-in-means estimator: unbiasedness (with reference to
# the Law of Large Numbers), and consistency.


# (1) The first step is to create a hypothetical "population" of units
# for which we know both the potential outcome under treatment (Y_1i)
# and the potential outcome under control (Y_0i).

set.seed(93106) # set seed to 'replicate' randomization results
N <- 5e3        # set population size

# For this exercise, we create our population using a random draw
# from a normal distribution with mean and standard deviation as
# specified.
Y_1i <- rnorm(N, mean = 3, sd = 1.5) # vector of potential outcomes under treatment
Y_0i <- rnorm(N, mean = 1, sd = 1.5) # vector of potential outcomes under control

# By construction, the true average treatment effect (ATE) should be equal to 2,
# allowing for variability due to random number generation. We can verify this 
# as follows:
ate <- mean(Y_1i) - mean(Y_0i)
ate

# The next step is to assign a binary treatment vector to indicate treatment
# status. This can be achieved with random draws from a binomial distribution:
D_i <- rbinom(N, size = 1, prob = 0.5)

# By construction, there should be roughly the same number of treated
# and control units. We can verify this as follows:
sum(D_i)



# With the treatment status and the potential outcomes, we can now calculate
# the observed outcome for each unit in the population.
Y_obs <- D_i * Y_1i + (1 - D_i) * Y_0i # recall formula from lecture


# We are now ready to combine the treatment status and the observed outcome
# into a population dataset, along with an index number for each unit.
df_pop <- cbind.data.frame(seq(1:N), Y_obs, D_i)
names(df_pop) <- c("index", "Y", "D")



# (2) Now we are ready to conduct our simulation. 
# Let's start with a sample size of n = 5.

samp_index <- sample.int(N, 5, replace = FALSE) # choose 5 index numbers at random

df_samp <- df_pop[samp_index, ]   # sample rows from population

diffmeans <- mean(df_samp$Y[df_samp$D==1]) - mean(df_samp$Y[df_samp$D==0]) # calculate sample difference in means


# We can now repeat this process for our simulation using a for loop,
# randomly drawing 5 units each time.

sims <- 100 # set number of draws
diffmean_sims <- rep(NA, sims) # create container object to store results


for(i in 1:sims){
        # while loop to redo any draws with no treated or no control units (not unlikely in samll)
        while(is.na(diffmean_sims[i])) {
                samp_index <- sample.int(N, 5, replace = FALSE)
                df_samp <- df_pop[samp_index,]
                diffmean_sims[i] <- mean(df_samp$Y[df_samp$D==1]) - mean(df_samp$Y[df_samp$D==0])
        }
}
# execute for loop


# We can now plot the distribution of the sample difference-in-means,
# with the true ATE also labeled for reference.
plot(density(diffmean_sims), xlab = "Estimated ATE", ylab = "",
     main = "Distribution of Difference in Means Estimate \n(N = 5, T = 100)",
     col = "dodgerblue")
abline(v = ate, lty = 2, col = "firebrick")
abline(v = 0, lty = 2, col = "gray40")
abline(v = mean(diffmean_sims), lty = 2, col = "darkreen")


# For reference: the same plot using ggplot2 (tidyverse package).


# Consistency -------------------------------------------------------------

# (3) We are now ready to repeat step (2) with a variety of sample sizes.
samp_sizes <- c(5, 10, 25, 50, 75, 100, 200, 500, 1000) # vector of sample sizes
sims <- 100

        # create object to store estimates

# use nested for loop, with the outer loop running through vector of sample sizes


# show density plots all together in a grid (one per sample size)


# Note that the sampling distribution of the difference-in-means estimator
# is roughly centered on the true ATE, even at relatively small sample sizes.
# This illustrates the estimator's unbiasedness.

# Moreover, note that the sampling distribution "collapses" around the true
# ATE as the sample size increases. This illustrates the estimator's second
# property: consistency.

