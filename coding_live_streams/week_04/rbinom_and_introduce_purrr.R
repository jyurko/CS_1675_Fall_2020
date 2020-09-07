### practice generating random numbers with rbinom()
### introducing purrr to functionally iterate instead of
### using a for-loop

library(dplyr)
library(ggplot2)

### generate some random values
set.seed(12345)
# set.seed(724412)

xa <- rbinom(n = 11, size = 1, prob = 0.25)

xa

xb <- rbinom(n = 5, size = 1, prob = 0.25)

xb

### respecify the seed and call the random number
### generator again
set.seed(12345)
# set.seed(724412)
xa_2 <- rbinom(n = 11, size = 1, prob = 0.25)

xa_2
xa

xb_2 <- rbinom(n = 5, size = 1, prob = 0.25)

xb_2

xb

### other values for the seeds
# set.seed(1)
# set.seed(2)
# set.seed(202002)

### can we estimate the probability of the EVENT giving our
### random sequences?

xa

### how many times did we see the event?

num_events <- sum(xa)
num_trials <- length(xa)

### what's the probability of observing exactly `num_events`
### out of `num_trials` ???????

dbinom(num_events, num_trials, prob = 0.25)

dbinom(0:num_trials, num_trials, prob = 0.25)

### plot the results in ggplot2

tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  ggplot(mapping = aes(x = x_events, y = prob_observe)) +
  geom_bar(stat = "identity") +
  theme_bw()

### modify the axis ticks
tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  ggplot(mapping = aes(x = x_events, y = prob_observe)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 0:num_trials) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### add in a panel strip legend for the probabiliyt of the event
tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  mutate(true_prob = 0.25) %>% 
  ggplot(mapping = aes(x = x_events, y = prob_observe)) +
  geom_bar(stat = "identity") +
  facet_wrap(~true_prob, labeller = "label_both") +
  scale_x_continuous(breaks = 0:num_trials) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### the number of trials
tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  mutate(true_prob = 0.25,
         N = num_trials) %>% 
  ggplot(mapping = aes(x = x_events, y = prob_observe)) +
  geom_bar(stat = "identity") +
  facet_grid(N ~ true_prob, labeller = "label_both") +
  scale_x_continuous(breaks = 0:num_trials) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### change the axis labels to be more descriptive
tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  mutate(true_prob = 0.25,
         N = num_trials) %>% 
  ggplot(mapping = aes(x = x_events, y = prob_observe)) +
  geom_bar(stat = "identity") +
  facet_grid(N ~ true_prob, labeller = "label_both") +
  scale_x_continuous(breaks = 0:num_trials) +
  labs(x = "number of events",
       y = "Probability of observing number of events out of number of trials") +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### look at our data frame
my_df <- tibble::tibble(
  x_events = 0:num_trials
) %>% 
  mutate(prob_observe = dbinom(x_events, num_trials, prob = 0.25)) %>% 
  mutate(true_prob = 0.25,
         N = num_trials)

my_df

### sequence of the coin flips?
### one possible sequence...
xa

### can we look at the result of each coin flip?

### define a function which handles book keeping

flip_coin_simulate <- function(rep_id, prob_heads, num_flips)
{
  x <- rbinom(n = num_flips, size = 1, prob = prob_heads)
  
  # book keeping to help with plots
  tibble::tibble(
    x = x
  ) %>% 
    mutate(rep_id = rep_id,
           prob_heads = prob_heads,
           num_flips = num_flips)
}

set.seed(12345)
flip_coin_simulate(1, 0.25, 11)

flip_coin_simulate(2, 0.25, 11)

### iterate and run the simulation 50 times

### use a for-loop

set.seed(12345)
my_sim_result <- flip_coin_simulate(1, 0.25, 11)

for(nr in 2:50){
  my_sim_result <- bind_rows(my_sim_result, 
                             flip_coin_simulate(nr, 0.25, 11))
}

my_sim_result %>% count(rep_id)

### I like use the purrr package to iterate functionally

# purrr::map() # returns a list

# purrr::map_dfr() # returns a data frame
set.seed(12345)
my_sim <- purrr::map_dfr(1:50,
                         flip_coin_simulate,
                         prob_heads = 0.25,
                         num_flips = num_trials)

my_sim

my_sim %>% count(rep_id)

### what if I wanted flip the coin 25 times per sequence?

set.seed(12345)
my_sim_25 <- purrr::map_dfr(1:50,
                            flip_coin_simulate,
                            prob_heads = 0.25,
                            num_flips = 25)

my_sim_25 %>% count(rep_id)

### look at the coin flips as a heat map

### first put in a sequential counter
my_sim %>% 
  mutate(coin = ifelse(x == 1, "HEADS", "TAILS")) %>% 
  group_by(rep_id) %>% 
  mutate(sequence_id = 1:n()) %>% 
  ungroup() %>% 
  count(sequence_id)

# my_sim %>% 
#   mutate(coin = ifelse(x == 1, "HEADS", "TAILS")) %>% 
#   group_by(rep_id) %>% 
#   mutate(sequence_id = 1:n()) %>% 
#   ungroup() %>% 
#   View()

### visualize the heat map
my_sim %>% 
  mutate(coin = ifelse(x == 1, "HEADS", "TAILS")) %>% 
  group_by(rep_id) %>% 
  mutate(sequence_id = 1:n()) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = as.factor(sequence_id),
                       y = as.factor(rep_id))) +
  geom_tile(mapping = aes(fill = coin),
            color = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### rerun the simulation with a probability of heads being .75

### run the simulation with 0.25 probability followed by 0.75 probability
### for each desired number of replications

study_grid <- expand.grid(rep_id = 1:50,
                          prob_heads = c(0.25, 0.75),
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

study_grid

study_grid %>% count(rep_id)

study_grid %>% count(prob_heads)

### functionally loop over the combinations of red_id and prob_heads

set.seed(12345)
my_sim_2_probs <- purrr::map2_dfr(study_grid$rep_id,
                                  study_grid$prob_heads,
                                  flip_coin_simulate,
                                  num_flips = num_trials)

### i can look at 2 heat maps at once
my_sim_2_probs %>% 
  mutate(coin = ifelse(x == 1, "HEADS", "TAILS")) %>% 
  group_by(rep_id, prob_heads) %>% 
  mutate(sequence_id = 1:n()) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = as.factor(sequence_id),
                       y = as.factor(rep_id))) +
  geom_tile(mapping = aes(fill = coin),
            color = "black") +
  facet_wrap(~prob_heads, labeller = "label_both") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### how can we use the simulation results?

### what's the distribution of the number of heads for a given
### probability and number of trials?

my_sim_2_probs %>% 
  group_by(prob_heads, num_flips, rep_id) %>% 
  summarise(num_heads = sum(x)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = num_heads)) +
  geom_bar() +
  facet_wrap(~prob_heads, labeller = "label_both") +
  scale_x_continuous(breaks = 0:num_trials) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### what if we used 1000 replications instead of 50?

big_study_grid <- expand.grid(rep_id = 1:1000,
                              prob_heads = c(0.25, 0.75),
                              KEEP.OUT.ATTRS = FALSE,
                              stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

set.seed(12345)
my_big_sim <- purrr::map2_dfr(big_study_grid$rep_id,
                              big_study_grid$prob_heads,
                              flip_coin_simulate,
                              num_flips = num_trials)

my_big_sim %>% 
  group_by(prob_heads, num_flips, rep_id) %>% 
  summarise(num_heads = sum(x)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x = num_heads)) +
  geom_bar() +
  facet_wrap(~prob_heads, labeller = "label_both") +
  scale_x_continuous(breaks = 0:num_trials) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

### what do you think the LEFT hand side facet is starting to look like?
