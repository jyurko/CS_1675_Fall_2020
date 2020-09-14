### week 05 -- practice working with likelihoods and priors

library(tidyverse)

### generate an event probability from the prior

### specify the prior -- a beta prior distribution
### what's the probability of someone winning a chess match
a_prior <- 5
b_prior <- 7

a_prior + b_prior

### look at our prior distribution
tibble::tibble(
  mu = seq(0.0001, 0.999, length.out = 1001)
) %>% 
  mutate(beta_pdf = dbeta(x = mu, shape1=a_prior, shape2=b_prior)) %>% 
  ggplot(mapping = aes(x = mu, y = beta_pdf)) +
  geom_line(size = 1.15) +
  theme_bw()

### a-prior expected value
a_prior / (a_prior + b_prior)

### middle 80% uncertainty - a priori
qbeta(0.1, a_prior, b_prior)
qbeta(0.9, a_prior, b_prior)

tibble::tibble(
  mu = seq(0.0001, 0.999, length.out = 1001)
) %>% 
  mutate(beta_pdf = dbeta(x = mu, shape1=a_prior, shape2=b_prior)) %>% 
  ggplot(mapping = aes(x = mu, y = beta_pdf)) +
  geom_line(size = 1.15) +
  geom_vline(xintercept = c(qbeta(0.25, a_prior, b_prior),
                            qbeta(0.75, a_prior, b_prior)),
             color = "grey50",
             size = 1.55) +
  geom_vline(xintercept = c(qbeta(0.1, a_prior, b_prior),
                            qbeta(0.9, a_prior, b_prior)),
             color = "steelblue",
             size = 1.35) +
  theme_bw()

### generate a random event probability
set.seed(19811)
event_prob <- rbeta(n = 1, shape1 = a_prior, shape2 = b_prior)

event_prob

### let's use this particular value as the TRUE event probability
tibble::tibble(
  mu = seq(0.0001, 0.999, length.out = 1001)
) %>% 
  mutate(beta_pdf = dbeta(x = mu, shape1=a_prior, shape2=b_prior)) %>% 
  ggplot(mapping = aes(x = mu, y = beta_pdf)) +
  geom_line(size = 1.15) +
  geom_vline(xintercept = c(qbeta(0.25, a_prior, b_prior),
                            qbeta(0.75, a_prior, b_prior)),
             color = "grey50",
             size = 1.55) +
  geom_vline(xintercept = c(qbeta(0.1, a_prior, b_prior),
                            qbeta(0.9, a_prior, b_prior)),
             color = "steelblue",
             size = 1.35) +
  geom_vline(xintercept = event_prob,
             color = "red", size = 1.55, linetype = "dashed") +
  theme_bw()

### what am I going to do with `event_prob` ?????

### generate events based on this event probability!!

### specify the number of trials with the `size` argument and `n = 1` then generate
### one value with rbinom() to correspond to teh number of events out of
### that trial size -> BINOMIAL option

### OR we could specify the number of trials as the `n` argument with `size = 1`
### in the rbinom() call -> BERNOULLI option: a sequence of independent
### BERNOULLI TRIALS

### example of the BINOMIAL formulation

set.seed(81231)
rbinom(n = 1, size = 55, prob = event_prob)

set.seed(81411)
rbinom(n = 16, size = 55, prob = event_prob)

### we will use the BERNOULLI option here

set.seed(81231)
x <- rbinom(n = 55, size = 1, prob = event_prob)

x

length(x)

sum(x)

### estimate the unknown event probability -- likelihood and the prior

### the observations are CONDITIONALLY INDEPENDENT given the model parameters
### our complete log-likelihood is therefore the SUM of the separate or 
### individual log-likelihoods

my_log_lik <- function(mu, x)
{
  sum(dbinom(x = x, size = 1, prob = mu, log = TRUE))
}

length(my_log_lik(0.5, x))

my_log_lik(0.5, x)
my_log_lik(0.4, x)
my_log_lik(0.7, x)

### define a function for the log-prior
my_log_prior <- function(mu, a, b)
{
  dbeta(mu, shape1 = a, shape2 = b, log = TRUE)
}

my_log_prior(0.7, a_prior, b_prior)

### the un-normalized log-posterior is the sum of hte
### log-likelihood and the log-prior

my_logpost <- function(mu, my_info)
{
  my_log_lik(mu, my_info$x) + my_log_prior(mu, my_info$a, my_info$b)
}

### create the list of required information
info_use <- list(
  x = x,
  a = a_prior,
  b = b_prior
)

### what's the actual posterior distribution since we have a 
### conjugate beta prior to the binomial likelihood
a_new <- a_prior + sum(x)
b_new <- b_prior + (length(x) - sum(x))

(a_new + b_new)

a_new / (a_new + b_new)

tibble::tibble(
  mu = seq(0.0001, 0.999, length.out = 1001)
) %>% 
  mutate(beta_prior = dbeta(x = mu, shape1=a_prior, shape2=b_prior),
         beta_post = dbeta(x = mu, shape1=a_new, shape2=b_new)) %>% 
  ggplot(mapping = aes(x = mu)) +
  geom_line(size = 1.15,
            mapping = aes(y = beta_prior,
                          color = "PRIOR")) +
  geom_line(size = 1.15,
            mapping = aes(y = beta_post,
                          color = "POSTERIOR")) +
  geom_vline(xintercept = event_prob,
             color = "red", size = 1.55, linetype = "dashed") +
  scale_color_manual("",
                     values = c("PRIOR" = "grey50",
                                "POSTERIOR" = "steelblue")) +
  theme_bw()

### what's the probability that the unknown event probability
### is less than the truth?
pbeta(event_prob, a_new, b_new)

qbeta(0.1, a_new, b_new)
qbeta(0.9, a_new, b_new)

### iterate to evaluate the log-posterior with our user defined function
### iterate over MANY values of mu

my_logpost(0.4, info_use)

### purrr library for funtional programming to APPLY a function
### over a sequence of values

my_logpost(c(0.25, 0.44, 0.55), info_use) ### is this right???? 

### let's check!!!
my_logpost(0.25, info_use)
my_logpost(0.44, info_use)
my_logpost(0.55, info_use)

### notice that although we get 3 values printed to screen in teh call
### in line 185, those 3 values are NOT the same as the three values
### we get by running my_logpost separatly! 
### this has to do with broadcasting and how R tries to figure out how to
### align the number of values in `info_use$x` with the number of values of
### `mu` when we call the log-likelihood function. 

### so it's safer to iterate over the values one by one!

### test this out with the 3 values above
mu_small_grid <- c(0.25, 0.44, 0.55)

### use purrr to apply my_logpost() to all element in mu_small_grid
purrr::map_dbl(mu_small_grid, my_logpost, my_info = info_use)

### we should see that the displayed values are the correct values!!!

### Now let's iterate over many values of the unknown mu

mu_grid <- seq(0.001, 0.999, length.out = 1001)

log_post_grid <- purrr::map_dbl(mu_grid,
                                my_logpost,
                                my_info = info_use)

length(log_post_grid)

### so purrr::map_dbl() accomplished the for-loop for us

### visualize the log-posterior
tibble::tibble(
  mu = mu_grid,
  log_post = log_post_grid
) %>% 
  ggplot(mapping = aes(x = mu, y = log_post)) +
  geom_line(size = 1.15, color = "navyblue") +
  theme_bw()

### plot the un-normalized posterior
tibble::tibble(
  mu = mu_grid,
  log_post = log_post_grid
) %>% 
  mutate(log_post_2 = log_post - max(log_post)) %>% 
  ggplot(mapping = aes(x = mu, y = exp(log_post_2))) +
  geom_line(size = 1.15, color = "navyblue") +
  theme_bw()

