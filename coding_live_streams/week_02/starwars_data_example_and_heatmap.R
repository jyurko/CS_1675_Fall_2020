###  practice with the tidyverse

library(tidyverse)

?starwars

starwars %>% class()

starwars %>% View()

glimpse(starwars)

starwars %>% glimpse()

starwars %>% summary()

starwars$height %>% class()

starwars$mass %>% class()

starwars %>% purrr::keep(is.numeric) %>% glimpse()

starwars %>% purrr::keep(is.integer) %>% glimpse()

starwars %>% purrr::keep(is.character) %>% glimpse()

starwars %>% purrr::discard(is.character) %>% glimpse()

starwars %>% purrr::discard(is.character) %>% 
  purrr::discard(is.list) %>% 
  glimpse()

starwars %>% 
  purrr::discard(is.character) %>% 
  purrr::discard(is.list) %>% 
  summary()

starwars %>% 
  purrr::discard(is.character) %>% 
  purrr::discard(is.list) %>% 
  na.omit() %>% 
  summary()

starwars %>% 
  purrr::discard(is.character) %>% 
  purrr::discard(is.list) %>% 
  na.omit() %>% 
  glimpse()

### aggregrate by grouping

starwars %>% 
  purrr::keep(is.character) %>% 
  names()

starwars %>% 
  purrr::keep(is.character) %>% 
  names() %>% 
  class()

starwars %>% 
  count(homeworld)

starwars %>% 
  dplyr::count(homeworld)

starwars %>% 
  filter(homeworld == "Alderaan")

starwars %>% 
  count(homeworld) %>% 
  filter(n > 1)

starwars %>% 
  count(homeworld) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

starwars %>% 
  count(homeworld, sex)

starwars %>% 
  count(homeworld, sex, gender, species)

starwars %>% 
  count(gender, sex, species, homeworld)

starwars %>% 
  count(homeworld, species)

starwars %>% 
  count(species, homeworld)

### create a graphic with ggplot2
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile()

### which species is the most common?
starwars %>% 
  count(species) %>% 
  arrange(desc(n))

starwars %>% 
  count(species, homeworld) %>% 
  arrange(desc(n))

### fix the x-axis
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_blank())

### rotate the x axis labels
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

### the count associated with each combination
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile(mapping = aes(fill = n)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile(mapping = aes(fill = n)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        legend.position = "top")

### change the color palette
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile(mapping = aes(fill = n)) +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

### include the count as text
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile(mapping = aes(fill = n)) +
  geom_text(mapping = aes(label = n)) +
  scale_fill_viridis_c() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

### make the text color depend on n
starwars %>% 
  count(species, homeworld) %>% 
  ggplot(mapping = aes(x = species, y = homeworld)) +
  geom_tile(mapping = aes(fill = n)) +
  geom_text(mapping = aes(label = n,
                          color = n > 3)) +
  scale_fill_viridis_c() +
  scale_color_manual(guide = FALSE,
                     values = c("TRUE" = "black",
                                "FALSE" = "white")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
