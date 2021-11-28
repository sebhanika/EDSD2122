### Assignment ####

# This is my assignment for the course Basic statistical programming
# as part of the EDSD prep courses. The course was given by Christian Dudel in
# October 2021

####### loading R packages #########

library(readstata13)
library(foreign)
library(tidyverse)
library(HMDHFDplus) 
library(downloader)

####### Question 1 #######

### loading data

url_dta <- "https://www.diw.de/documents/dokumentenarchiv/
            17/diw_01.c.412698.de/soep_lebensz_en.zip" # url for download
download(url = url_dta, dest="dataset.zip", mode="wb") # downloads zip folder into current directory
unzip ("dataset.zip", exdir = ".") # unzips file into current working directory

dt <- read.dta(file ="soep_lebensz_en.dta",  # reads dta file from current directory
               convert.factors = F) 

# Question 1b
# number of unique individuals in data set according to id

u_ind <- length(unique(x=dt$id))
print(paste0("There are: ", u_ind, " unique individuals in the data set."))

# Question1c,

# number of observations per year
tab_1c <- dplyr::count(dt, year)

# Question 1d
dt_04 <- dt %>% 
  filter(year==max(year)) # finding most recent year

dt_prop <- dt_04 %>% # this is a dplyr solution
  group_by(sex) %>% 
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n), 4),
         prop_per = prop*100)

#pull data for female proportions
female_props <- dt_prop %>%
  filter(sex == 1) %>% # filter for females
  pull(prop_per)       #pulls value from prop_per column

#pull data for male proportion
male_props <- dt_prop %>%
  filter(sex == 0) %>% # filter for males
  pull(prop_per)      # pulls value from prop_per column

# in case we want answer sentences :D 
print(paste0("Proportion of females is: ", female_props, "%"))

print(paste0("Proportion of males is: ", male_props, "%"))

# calculating average health
dt_health <- dt_04 %>%
  select(sex, health_org) %>% # selecting relevant columns
  group_by(sex) %>%
  summarise(avg_health = round(mean(health_org), digits = 2)) #calculating mean

#data for female health
female_health <- dt_health %>%
  filter(sex == 1) %>% # filter for females
  pull(avg_health)     # pulls value from dataframe

#data for male health
male_health <- dt_health %>%
  filter(sex == 0) %>% # filter for males
  pull(avg_health)     # pulls value from dataframe

print(paste0("Averge subjective health of females is: ", female_health))

print(paste0("Averge subjective health of males is: ", male_health))

######## Question 2 #######

# loading data from Human Mortality Database 
# Japan,life expectancy at birth by sex 

myHMDusername <- "Enter Username" 
myHMDpassword <- "Enter password"

jpn <-readHMDweb("JPN","E0per", 
                    username = myHMDusername,
                    password = myHMDpassword,
                    fixup = T)

# visualization of the trend in life expectancy at birth

#specifying colors for graphs
col <- c("Females" = "coral3", "Males" = "cornflowerblue")
  
fig1 <- jpn %>%
  ggplot(aes(x=Year)) +
  geom_line(aes(y=Female, color = "Females"), size = 1, linetype = 3) +
  geom_line(aes(y=Male, color = "Males"), size = 1) +
  scale_color_manual(values = col) +
  labs(title = "Life expectancy at birth, both sexes Japan, 1947-2019",
       x = "Year",
       y = "e0 (years)", 
       color = "") +
  scale_y_continuous(limits = c(49, 90)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=11, color = "grey30"),
        legend.position = "bottom",
        plot.title = element_text(size = 13, hjust = 0.5)) 

fig1
  
# calculating the gender gap

jpn <- jpn %>%
  mutate(diff_e0 = Female - Male)

# visualization of the gender gap in life expectancy at birth

fig2 <- jpn %>%
  ggplot(aes(x = Year, y = diff_e0)) +
  geom_line(color = "darkcyan", size = 1.25) + 
  labs(title = "Gender gap in life expectancy at birth Japan, 1947-2019",
       x = "Year", 
       y = " Difference (years)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=11, color = "grey30"),
        legend.position = "bottom",
        plot.title = element_text(size = 13, hjust = 0.5)) 
  

fig2
