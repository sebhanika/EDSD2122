
# This code attempts to recreate some of the Graphs from the Paper:
# Evidence for a limit to human lifespan (X. Dong, B. Milholland and J. Vijg
# Nature, 2017) Health & Population: WHO and Schools of Public Health

# The visualizations were used for a presentation in the EDSD 2021/2022 Class
# Mortality Causes by Cosmo Strozza in November 2021.

###### library and set up ######
library(tidyverse)
library(HMDHFDplus)
library(broom)
library(forestmangr)

options(scipen = 999)

###### loading data ###### 
myHMDusername <- "Your username" 
myHMDpassword <- "Your Password"

# france life expectancy 
france_e0 <-readHMDweb("FRATNP","E0per", 
                       username = myHMDusername,
                       password = myHMDpassword,
                       fixup = T)

# france female life tables
france_mltper <- readHMDweb("FRATNP","mltper_1x1", 
                            username = myHMDusername,
                            password = myHMDpassword,
                            fixup = T)

# france male life tables
france_fltper <- readHMDweb("FRATNP","fltper_1x1", 
                            username = myHMDusername,
                            password = myHMDpassword,
                            fixup = T)


###### initial data prep ###### 

# male life expectancy 
france_lf_m <- france_mltper %>%
  filter(Year > 1899,     # years and ages not necessary
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(age2 = as.factor(Age), # for graph
         lx = lx + 1) # imputation to avoid log of zero

# france male life expactancy
france_lf_f <- france_fltper %>%
  filter(Year > 1899,
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(age2 = as.factor(Age),
         lx = lx + 1)



###### plots 1a to 1c ###### 

### figure 1 a - life expectancy

#specifying colors for graphs
col <- c("Females" = "coral3", "Males" = "cornflowerblue")

fig1_a <- france_e0 %>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=Female, color = "Females"), size = 2) +
  geom_line(aes(y=Male, color = "Males"), size = 2) +
  scale_color_manual(values = col) +
  labs(x = "Year",
       y = "Life expectancy at birth (years)", 
       color = "", 
       title = "Life expectancy in France 1900 to 2018") +
  scale_y_continuous(limits = c(25, 88)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=22, color = "grey30"), #sizes for slide presentation
        axis.text = element_text(size=20, color = "grey30"),
        legend.text = element_text(size=30, color = "grey30"),
        legend.position = c(0.88, 0.15)) 

fig1_a


### fig1_b - regression old age

fig1_b_m <- france_lf_m %>%
  ggplot(aes(x = Year, y = lx, color = age2)) +
  geom_point() +
  scale_y_log10(breaks = c(1, 100, 10000), #manually setting axis for comparison with paper
                labels = c(1, 100, 10000)) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  theme_bw()+
  labs(y ="Survivors per 100,000",
       x = "Years",
       title = "Regressions of males surviving to old age")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=22, color = "grey30"),
        axis.text = element_text(size=20, color = "grey30"))

# removing point layer to only have regression lines
fig1_b_m$layers[[1]] <- NULL # not the most elegant solution
fig1_b_m


fig1_b_f <- france_lf_f %>%
  ggplot(aes(x = Year, y = lx, color = age2)) +
  geom_point() +
  scale_y_log10(breaks = c(1, 100, 10000),
                labels = c(1, 100, 10000)) +
  geom_smooth(method = "lm", se = FALSE, show.legend = F) +
  theme_bw()+
  labs(y ="Survivors per 100,000",
       x = "Years",
       title = "Regressions of males surviving to old age")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=22, color = "grey30"),
        axis.text = element_text(size=20, color = "grey30"))


fig1_b_f$layers[[1]] <- NULL # again removing point layer
fig1_b_f



### fig2_c rate of change

# this requires to run multiple linear models
### prep data for lm

# males
data_lm_m <- france_mltper %>%
  filter(Year > 1899,
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(lx = lx + 1) # adding one to avoid taking log(0). This is discussed in the paper

#females
data_lm_f <- france_fltper %>%
  filter(Year > 1899, 
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(lx = lx + 1) 


#linear models
linear_mod_m <- france_lf_m  %>% 
  group_by(Age) %>%  # running model for each age
  lm_table(log(lx) ~  Year)

linear_mod_f <- data_lm_f  %>% 
  group_by(Age) %>%  # running model for each age
  lm_table(log(lx) ~  Year)

# getting data in right format for plot
data_lm_plot <- linear_mod_m %>%
  select(c("Age", "b1")) %>%
  left_join(select(linear_mod_f, c("Age", "b1")),
            by = "Age") %>%
  rename("Male_b1" = b1.x,
         "Female_b1" = b1.y) %>%
  pivot_longer(cols = c("Male_b1", "Female_b1"),
               names_to = c("sex", "b1"),
               names_sep = "_")


# plotting data
fig1_c <- data_lm_plot %>%
  ggplot(aes(x = Age, y = value, color = sex, shape = sex))+
  geom_point(size = 4, stroke = 2) +
  geom_line(size = 2) +
  scale_shape_manual(values = c(1,0))+
  theme_bw()+
  labs(y ="Rate of change since 1900",
       x = "Age (Years)",
       title = "Rate of change for old ages")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=22, color = "grey30"),
        axis.text = element_text(size=20, color = "grey30"),
        plot.title = element_text(size = 30),
        legend.text = element_text(size=30, color = "grey30"),
        legend.title=element_blank(),
        legend.position = c(0.1, 0.90)) 
fig1_c


###### Analysis after 1950 ###### 

# This code reruns the analysis but from 1950 onwords as suggested by
# Lenart and Vaupel in: Questionable evidence for a limit to human lifespan 
# (A. Lenart and J.W. Vaupel Nature, 2017)


### preping  data

# males
data_lm_m_1950 <- france_mltper %>%
  filter(Year > 1949,
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(lx = lx + 1)

#females
data_lm_f_1950 <- france_fltper %>%
  filter(Year > 1949,
         Age > 69) %>%
  select(-c("mx", "Tx", "ax", "dx", "Lx", "ex")) %>%
  mutate(lx = lx + 1)

# rerunning linear models
linear_mod_f_1950 <- data_lm_f_1950  %>% 
  group_by(Age) %>% 
  lm_table(log(lx) ~  Year)

linear_mod_m_1950 <- data_lm_m_1950  %>% 
  group_by(Age) %>% 
  lm_table(log(lx) ~  Year)

# getting data in right format for plot
data_lm_plot_1950 <- linear_mod_m_1950 %>%
  select(c("Age", "b1")) %>%
  left_join(select(linear_mod_f_1950, c("Age", "b1")), by = "Age") %>%
  rename("Male_b1" = b1.x,
         "Female_b1" = b1.y) %>%
  pivot_longer(cols = c("Male_b1", "Female_b1"),
               names_to = c("sex", "b1"),
               names_sep = "_")

# plotting data
fig1_c_1950 <- data_lm_plot_1950 %>%
  ggplot(aes(x = Age, y = value, color = sex, shape = sex) )+
  geom_point(size = 4, stroke = 2) +
  geom_line(size = 2) +
  scale_shape_manual(values = c(1,0))+
  theme_bw()+
  labs(y ="Rate of change since 1950",
       x = "Age (Years)",
       title = "Rate of change for old ages")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(size = 30),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=22, color = "grey30"),
        axis.text = element_text(size=20, color = "grey30"),
        legend.text = element_text(size=30, color = "grey30"),
        legend.title=element_blank(),
        legend.position = c(0.1, 0.90)) 

fig1_c_1950
