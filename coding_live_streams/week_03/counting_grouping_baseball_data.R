### week 03 live coding session 
### more practice with count() and heatmaps
### use the baseball data set to introduce group_by()
### and summarize()
###
### introduce wide vs tall data sets and reshaping with 
### tidyr::gather()

library(dplyr)
library(ggplot2)

### start off with a question about outliers

### look at the diamonds data set

diamonds %>% 
  ggplot(mapping = aes(x = price)) +
  geom_histogram() +
  theme_bw()

diamonds %>% summary()

diamonds %>% 
  ggplot(mapping = aes(x = color, y = price)) +
  geom_boxplot() +
  theme_bw()

### generate random numbers, to visualize outliers
### from known distributions
set.seed(101)

### we will learn more about random number generators
### in the next few weeks
my_df <- tibble::tibble(
  x1 = rnorm(n = 5000),
  x2 = rnorm(n = 5000),
  x3 = rnorm(n = 5000, mean = 2, sd = 0.5)
)

my_df %>% glimpse()

my_df %>% summary()

### reshaping operations -- WIDE VS TALL data

my_df %>% head()

### use the tidyr::gather() function

### basic syntax is:
### tidyr::gather(key = "key", value = "value")

my_df %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "my_variables",
                value = "variable_values",
                -rowid)

### what are the first few rows of the original data
my_df %>% slice(1:4)

### look at the rows associated with the first 4 observations
### for each key, to show tidyr::gather() stacked the columns
### on top of each other
my_df %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "my_variables",
                value = "variable_values",
                -rowid) %>% 
  filter(rowid < 5)

### check the number of rows per `my_variables`
my_df %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "my_variables",
                value = "variable_values",
                -rowid) %>% 
  count(my_variables)

### store the long or tall data set
my_tall_df <- my_df %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "my_variables",
                value = "variable_values",
                -rowid)

### create a boxplot for each variable in the dataset
### of my rnorm() calls
my_tall_df %>% 
  ggplot(mapping = aes(x = my_variables, y = variable_values)) +
  geom_boxplot() +
  theme_bw()

### use separate facets per variable
my_tall_df %>% 
  ggplot(mapping = aes(x = variable_values)) +
  geom_histogram(bins = 55,
                 mapping = aes(y = stat(density))) +
  geom_density(color = "red") +
  facet_wrap(~my_variables, scales = "free_y") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### load in the lahman package

library(Lahman)

Batting %>% glimpse()

### start with count()

Batting %>% 
  count(yearID, lgID, teamID) %>% 
  tibble::as_tibble()

### how many unique leagues are in the data set?

Batting %>% 
  count(lgID)

### create a heat map between yearID and lgID

Batting %>% 
  count(yearID, lgID) %>% 
  tibble::as_tibble() %>% 
  ggplot(mapping = aes(x = yearID, y = lgID)) +
  geom_tile(mapping = aes(group = interaction(yearID, lgID)),
            fill = "grey30", color = "black") +
  theme_bw()

### filter to years after 1899 and just the AL and NL

### practice with %in%

"a" == c("a")

"a" == c("a", "b", "c", "d")

c("a", "b") == c("a", "b", "c", "d")

c("a", "b", "c", "d") %in% c("a", "b")

my_batting <- Batting %>% 
  filter(yearID > 1899) %>% 
  filter(lgID %in% c("AL", "NL"))

my_batting %>% 
  glimpse()

### how many unique teams are there in the leagues?

my_batting %>% 
  count(teamID)

### heat map showing teams in the leagues

my_batting %>% 
  count(yearID, lgID, teamID) %>% 
  ggplot(mapping = aes(x = yearID, y = teamID)) +
  geom_tile(mapping = aes(group = interaction(yearID, teamID),
                          fill = lgID),
            color = "black") +
  facet_grid( lgID ~ ., scales = "free_y") +
  scale_fill_brewer(guide = FALSE, palette = "Set1") +
  theme_bw()

### more advanced group and summarizing

### group_by() %>% summarize()

### the number of unique players per TEAM per YEAR

my_batting %>% tibble::as_tibble()

my_batting %>% count(yearID, lgID, teamID) %>% 
  tibble::as_tibble()

my_batting %>% 
  group_by(yearID, lgID, teamID) %>% 
  summarise(num_rows = n(),
            num_unique_players = n_distinct(playerID)) %>% 
  ungroup() %>% 
  filter(num_rows != num_unique_players)

### arrange by yearID decreasing
my_batting %>% 
  group_by(yearID, lgID, teamID) %>% 
  summarise(num_rows = n(),
            num_unique_players = n_distinct(playerID)) %>% 
  ungroup() %>% 
  filter(num_rows != num_unique_players) %>% 
  arrange(desc(yearID))

my_batting %>% 
  select(playerID, yearID, lgID, teamID, stint) %>% 
  tibble::as_tibble()

### check the values of stint

my_batting %>% 
  count(stint)

### look at the indiviauals that played on 5 times in a year
players_with_5_stints <- my_batting %>% 
  filter(stint == 5) %>% 
  tibble::as_tibble() %>% 
  pull(playerID)

my_batting %>% 
  filter(playerID %in% players_with_5_stints) %>% 
  tibble::as_tibble()
