
#############################################
# PS 207
# Randomized Experiments ATE Simulation
# Date: April 15, 2021
#############################################

library(MASS)

# The goal of this simulation is to examine statistical properties
# of the sample average treatment effect (SATE) and the estimator
# for the standard error of the SATE.

# The first step is to generate a hypothetical sample, for which
# we know both the potential outcome under treatment (Y_1i)
# and the potential outcome under control (Y_0i) for all units.

set.seed(93106) # set seed to 'replicate' randomization results
N <- 500        # set sample size


# The potential outcomes are generated so that they have 
# a high positive correlation. (Don't worry about the code!)
V <- matrix(ncol = 2, nrow = 2)
V[1,] <- c(1, 0.6)
V[2,] <- c(0.6, 1)
PO_sample <- mvrnorm(N, mu = c(1.5, 1), Sigma = V)

Y_1i <- PO_sample[,1] # treated potential outcome
Y_0i <- PO_sample[,2] # control potential outcome

plot(Y_0i, Y_1i) # visual check of positive correlation


# By construction, the true SATE should be approximately 0.5,
# allowing for variability due to random number generation. 
# We can verify this as follows:
tau_i <- Y_1i - Y_0i
ate <- mean(tau_i)
ate

# For later comparison, we can also calculate the "true"
# standard error of the SATE. (See Field Experiments, p. 57)
V_Y0i <- 1/N * sum((Y_0i - mean(Y_0i))^2)
V_Y1i <- 1/N * sum((Y_1i - mean(Y_1i))^2)

se_ate <- sqrt(1/(N-1) * (V_Y0i + V_Y1i + 2*cov(Y_0i, Y_1i)))
se_ate


# The first step in each trial of the simulation is to generate 
# and assign a vector of treatment status.
D_i <- rbinom(N, size = 1, prob = 0.5)


# With the treatment status and the potential outcomes, we
# can now calculate the observed outcome for each unit.
Y_obs <- D_i * Y_1i + (1 - D_i) * Y_0i


# We are now ready to combine the treatment status and
# the observed outcome into a specific observed sample.
df_samp <- cbind.data.frame(Y_obs, D_i)
names(df_samp) <- c("Y", "D")
head(df_samp)


# We can first calculate the estimated sample ATE.
mu_Y1 <- mean(df_samp$Y[df_samp$D==1])
mu_Y0 <- mean(df_samp$Y[df_samp$D==0])

sample_ate_est <- mu_Y1 - mu_Y0
sample_ate_est

ate # compare to true value


# We can also estimate the standard error of the sample ATE.
# (See Field Experiments, p. 61)
N_1 <- sum(df_samp$D)
N_0 <- N - N_1       

V_1_est <- sum((df_samp$Y[df_samp$D==1] - mu_Y1)^2) / (N_1-1)
V_0_est <- sum((df_samp$Y[df_samp$D==0] - mu_Y0)^2) / (N_0-1)

se_ate_est <- sqrt(V_1_est/N_1 + V_0_est/N_0)
se_ate_est

se_ate # compare to true value



# Now we are ready to conduct our simulation by repeating
# the above steps.
sims <- 1000

sample_ate_estimates <- rep(NA, sims) # create containers
se_ate_estimates <- rep(NA, sims)

for(i in 1:sims) {
  D_i <- rbinom(N, size = 1, prob = 0.5)
  Y_obs <- D_i * Y_1i + (1 - D_i) * Y_0i
  
  df_samp <- cbind.data.frame(Y_obs, D_i)
  names(df_samp) <- c("Y", "D")
  
  mu_Y1 <- mean(df_samp$Y[df_samp$D==1])
  mu_Y0 <- mean(df_samp$Y[df_samp$D==0])
  sample_ate_estimates[i] <- mu_Y1 - mu_Y0
  
  N_1 <- sum(df_samp$D)
  N_0 <- N - N_1
  V_1_est <- sum((df_samp$Y[df_samp$D==1] - mu_Y1)^2) / (N_1-1)
  V_0_est <- sum((df_samp$Y[df_samp$D==0] - mu_Y0)^2) / (N_0-1)
  se_ate_estimates[i] <- sqrt(V_1_est/N_1 + V_0_est/N_0)
}


# Plotting the results
par(mfrow=c(1,2))

plot(density(sample_ate_estimates), col = "cornflowerblue",
     xlab = "Estimate of Sample ATE", main = "Distribution of Sample ATE Estimates")
abline(v = mean(sample_ate_estimates), lty = 2, col = "cornflowerblue")
abline(v = ate, lty = 2, col = "firebrick1")

plot(density(se_ate_estimates), col = "cornflowerblue", xlim = c(0.075, 0.1),
     xlab = "Estimate of SE(Sample ATE)", main = "Distribution of SE(Sample ATE) Estimates")
abline(v = mean(se_ate_estimates), lty = 2, col = "cornflowerblue")
abline(v = se_ate, lty = 2, col = "firebrick1")


# As expected, our estimator of the sample ATE is unbiased.
# However, note that our estimator of the SE of the sample ATE
# has a strong positive bias. This illustrates what we mean
# when we say that the estimator we use yields a "conservative"
# estimate of the standard error.

