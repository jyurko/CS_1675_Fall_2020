### learn an unknown mean and unknown noise given some data

library(tidyverse)

library(Lahman)

### CONSTANT MEAN from data

Master %>% glimpse()

### learn the average HEIGHT of a Major League Baseball Player

Master %>% 
  ggplot(mapping = aes(x = height)) +
  geom_histogram(bins = 42) +
  theme_bw()

Master %>% select(playerID, weight, height) %>% summary()

Master %>% 
  ggplot(mapping = aes(x = height)) +
  geom_histogram(binwidth = 1) +
  theme_bw()

Master %>% 
  ggplot(mapping = aes(x = height)) +
  geom_histogram(binwidth = 1,
                 mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1.15) +
  theme_bw()

### we can control how "smooth" the kernel density estimate is
Master %>% 
  ggplot(mapping = aes(x = height)) +
  geom_histogram(binwidth = 1,
                 mapping = aes(y = stat(density))) +
  geom_density(color = "red", size = 1.15,
               adjust = 0.1) +
  theme_bw()

Master %>% count(height)

### we could model the HEIGHT of a baseball player with a Gaussian likelihood 

### but we also do not know the noise -- the standard deviation of the HEIGHT

### define our log-posterior function on the unkonwn mean mu and the unkonwn
### standard deviation (noise) sigma

my_logpost <- function(unknowns, my_info)
{
  # unpack the parameters
  mu <- unknowns[1]
  sigma <- unknowns[2]
  
  # calculate the log-likelihood
  log_lik <- sum(dnorm(x = my_info$xobs, mean = mu, sd = sigma, log = TRUE))
  
  # use a gaussian on the unknown mean
  log_prior_mu <- dnorm(x = mu,
                        mean = my_info$mu_0,
                        sd = my_info$tau_0,
                        log = TRUE)
  
  # use the Exponential prior on the unknown noise
  log_prior_sigma <- dexp(x = sigma, rate = my_info$sigma_rate, log = TRUE)
  
  # the un-normalized log-posterior is then the sum
  log_lik + log_prior_mu + log_prior_sigma
}

### randomly sample 100 observations from the Master list

set.seed(8123123)
train_id <- sample(1:nrow(Master), 100, replace = FALSE)

### subset the Master list of players

train_df <- Master %>% 
  select(playerID, birthYear, deathYear, weight, height) %>% 
  slice(train_id)

### look at the distribution of height based on our 100 samples
train_df %>% 
  ggplot(mapping = aes(x = height)) +
  geom_histogram(binwidth = 2) +
  geom_rug(alpha = 0.2, color = 'red') +
  theme_bw()

### clean the data set and remove the missings
train_df %>% nrow()

### if we want ONLY complete cases we can use `na.omit()`
train_df %>% na.omit() %>% nrow()

### if we know the specific variable to focus on we can use filter()
clean_df <- train_df %>% 
  filter(!is.na(height))

clean_df %>% nrow()

### specify my prior belief on the unkonwn mean and the unknonw noise

### we will set the prior mean and sd on MU based on the average US male height
mu_0 <- 69
tau_0 <- 3

### the variation across all the observations around the unknown is SIGMA

tibble::tibble(
  s = seq(1e-3, 10, length.out = 1001)
) %>% 
  mutate(Exp_pdf = dexp(x = s, rate = 1)) %>% 
  ggplot(mapping = aes(x = s, y = Exp_pdf)) +
  geom_line(size = 1.15) +
  labs(x = "noise, sigma") +
  theme_bw()

### what if we vary the rate?
tibble::tibble(
  s = seq(1e-3, 10, length.out = 1001)
) %>% 
  mutate(Exp_pdf = dexp(x = s, rate = 3)) %>% 
  ggplot(mapping = aes(x = s, y = Exp_pdf)) +
  geom_line(size = 1.15) +
  labs(x = "noise, sigma") +
  theme_bw()

tibble::tibble(
  s = seq(1e-3, 10, length.out = 1001)
) %>% 
  mutate(Exp_pdf = dexp(x = s, rate = 0.3)) %>% 
  ggplot(mapping = aes(x = s, y = Exp_pdf)) +
  geom_line(size = 1.15) +
  labs(x = "noise, sigma") +
  theme_bw()

### directly compare 3 values for the rate
tibble::tibble(
  s = seq(1e-3, 10, length.out = 1001)
) %>% 
  mutate(pdf_rate_0.3 = dexp(x = s, rate = 0.3),
         pdf_rate_1.0 = dexp(x = s, rate = 1.0),
         pdf_rate_3.0 = dexp(x = s, rate = 3.0))

### ggplot requires us to reshape from wide to long format
### in order to appropriately map the aesthetics
tibble::tibble(
  s = seq(1e-3, 10, length.out = 1001)
) %>% 
  mutate(pdf_rate_0.3 = dexp(x = s, rate = 0.3),
         pdf_rate_1.0 = dexp(x = s, rate = 1.0),
         pdf_rate_3.0 = dexp(x = s, rate = 3.0)) %>% 
  tidyr::gather(key = "key", value = "value", -s) %>% 
  tidyr::separate(key,
                  c("pdf_word", "rate_word", "rate_value"),
                  sep = "_") %>% 
  ggplot(mapping = aes(x = s, y = value)) +
  geom_line(mapping = aes(group = rate_value,
                          color = rate_value),
            size = 1.15) +
  ggthemes::scale_color_calc() +
  theme_bw()

### set the rate hyperparameter to 
sigma_rate <- 0.2

### package the list of required information

info_use <- list(
  xobs = clean_df$height,
  mu_0 = mu_0,
  tau_0 = tau_0,
  sigma_rate = sigma_rate
)

### visualize the log-posterior surface

param_grid <- expand.grid(mu = seq(mu_0 - 3 * tau_0,
                                   mu_0 + 3 * tau_0,
                                   length.out = 251),
                          sigma = seq(0.5,
                                      9,
                                      length.out = 251),
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

param_grid %>% glimpse()

length(param_grid$mu)

### define a wrapper function
eval_logpost <- function(mu_val, sigma_val, logpost_func, logpost_info)
{
  logpost_func(c(mu_val, sigma_val), logpost_info)
}

### calculate the logposterior over the parameter grid
log_post_result <- purrr::map2_dbl(param_grid$mu,
                                   param_grid$sigma,
                                   eval_logpost,
                                   logpost_func = my_logpost,
                                   logpost_info = info_use)

length(log_post_result)

param_grid %>% 
  mutate(log_post = log_post_result,
         log_post_2 = log_post - max(log_post)) %>% 
  ggplot(mapping = aes(x = mu, y = sigma)) +
  geom_raster(mapping = aes(fill = log_post_2)) +
  stat_contour(mapping = aes(z = log_post_2),
               breaks = log(c(0.01/100, 0.01, 0.1, 0.5, 0.9)),
               size = 2.2,
               color = "black") +
  # include the sample average and the sample standard deviation
  geom_point(data = tibble::tibble(xbar = mean(info_use$xobs),
                                   xsd = sd(info_use$xobs)),
             mapping = aes(x = xbar, y = xsd),
             shape = 22,
             size = 4.5, fill = "orange", color = "steelblue") +
  scale_fill_viridis_c(guide = FALSE, option = "viridis",
                       limits = log(c(0.01/100, 1.0))) +
  labs(x = expression(mu), y = expression(sigma)) +
  theme_bw()


### zoom in
param_grid %>% 
  mutate(log_post = log_post_result,
         log_post_2 = log_post - max(log_post)) %>% 
  ggplot(mapping = aes(x = mu, y = sigma)) +
  geom_raster(mapping = aes(fill = log_post_2)) +
  stat_contour(mapping = aes(z = log_post_2),
               breaks = log(c(0.01/100, 0.01, 0.1, 0.5, 0.9)),
               size = 2.2,
               color = "black") +
  # include the sample average and the sample standard deviation
  geom_point(data = tibble::tibble(xbar = mean(info_use$xobs),
                                   xsd = sd(info_use$xobs)),
             mapping = aes(x = xbar, y = xsd),
             shape = 22,
             size = 4.5, fill = "orange", color = "steelblue") +
  # include the prior mean on mu and the prior mean on sigma
  geom_vline(xintercept = mu_0,
             color = "white", size = 2, linetype = "dotted") +
  scale_fill_viridis_c(guide = FALSE, option = "viridis",
                       limits = log(c(0.01/100, 1.0))) +
  coord_cartesian(xlim = c(67, 75), ylim = c(1.5, 3.2)) +
  labs(x = expression(mu), y = expression(sigma)) +
  theme_bw()

### apply the laplace approximation -- perform the change of variables

my_logpost_cv <- function(unknowns, my_info)
{
  # unpack the parameters
  mu <- unknowns[1]
  varphi <- unknowns[2]
  sigma <- exp(varphi)
  
  # calculate the log-likelihood
  log_lik <- sum(dnorm(x = my_info$xobs, mean = mu, sd = sigma, log = TRUE))
  
  # use a gaussian on the unknown mean
  log_prior_mu <- dnorm(x = mu,
                        mean = my_info$mu_0,
                        sd = my_info$tau_0,
                        log = TRUE)
  
  # use the Exponential prior on the unknown noise
  log_prior_sigma <- dexp(x = sigma, rate = my_info$sigma_rate, log = TRUE)
  
  # the un-normalized log-posterior is then the sum
  ### account for the log derviative adjustment
  log_lik + log_prior_mu + log_prior_sigma + varphi
}


my_laplace <- function(start_guess, logpost_func, ...)
{
  # code adapted from the `LearnBayes`` function `laplace()`
  fit <- optim(start_guess,
               logpost_func,
               gr = NULL,
               ...,
               method = "BFGS",
               hessian = TRUE,
               control = list(fnscale = -1, maxit = 1001))
  
  mode <- fit$par
  post_var_matrix <- -solve(fit$hessian)
  p <- length(mode)
  # we will discuss what int means in a few weeks...
  int <- p/2 * log(2 * pi) + 0.5 * log(det(post_var_matrix)) + logpost_func(mode, ...)
  # package all of the results into a list
  list(mode = mode,
       var_matrix = post_var_matrix,
       log_evidence = int,
       converge = ifelse(fit$convergence == 0,
                         "YES", 
                         "NO"),
       iter_counts = as.numeric(fit$counts[1]))
}

laplace_result <- my_laplace(c(72, log(3)), my_logpost_cv, info_use)

laplace_result

### convert a covariance matrix to a corrleation
cov2cor(laplace_result$var_matrix)

### to back transform from varphi to sigma, we can use random posterior samples

generate_post_samples <- function(mvn_object, num_samples)
{
  MASS::mvrnorm(n = num_samples,
                mu = mvn_object$mode,
                Sigma = mvn_object$var_matrix) %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(c("mu", "varphi")) %>% 
    mutate(sigma = exp(varphi))
}

### generate 1000 samples
set.seed(123123)
post_samples <- generate_post_samples(laplace_result, 1000)

post_samples %>% 
  tibble::rowid_to_column("post_id") %>% 
  tidyr::gather(key = "key", value = "value", -post_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 45) +
  facet_wrap(~key, scales = "free") +
  theme_bw()
