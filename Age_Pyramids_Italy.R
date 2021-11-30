# this code creates population pyramids for the Italian population
# in the past and projected into the future
# the code was developed together with Daniele De Rocchi

###### libraries ########

library(tidyverse)
library(wpp2019)

##### data ########

# calling data
data(popF)
data(popM)
data(popFprojMed)
data(popMprojMed)

# historical data
popF <- popF %>% filter(country_code == 380)
popM <- popM %>% filter(country_code == 380)

# projected data
popFprojMed <- popFprojMed %>% filter(country_code == 380)
popMprojMed <- popMprojMed %>% filter(country_code == 380)


### data preparation
df0 <- popF %>% 
  left_join(popFprojMed) %>%
  mutate(sex = 'female')

df1 <- popF %>% 
  left_join(popFprojMed) %>%
  mutate(sex = 'male') %>%
  bind_rows(df0) %>%
  mutate(age = factor(age, levels = unique(age)))

df2 <- df1 %>%
  pivot_longer(cols = -c("country_code", "name", "age", "sex"),
               names_to = "year",
               values_to = "pop") %>% 
  filter(year %in% c(1950, 2000, 2050, 2100)) # filtering for interested years


### plotting data 
plot_pyr <- df2 %>%
  ggplot(aes(x = age, 
             y = ifelse(sex == "male", -pop, pop), # producing negative values for men
             pop, fill = sex)) +
  geom_bar(stat = 'identity') +
  scale_y_continuous(limits = c(-2500, 2500), 
                     breaks = seq(-2000, 2000, 1000),
                     labels = c(2,1,0,1,2)) + # manual labeling 
  scale_fill_manual(values = c('female' = '#01665E', 'male' = '#DABF7F'), 
                    labels = c('Female', 'Male'),
                    name='Sex') +
  coord_flip() +
  labs(x = 'Age',
       y = 'Population in million')+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = 'black'),
        axis.title = element_text(size=10, color = 'grey20')) +
  facet_grid(. ~ year) +
  theme(strip.text.x = element_text(size = 15))
plot_pyr

