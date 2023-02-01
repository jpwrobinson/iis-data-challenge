library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(janitor)

ff<-read.csv('elec-fossil-nuclear-renewables.csv') %>% 
    clean_names() %>% 
    filter(entity == 'World') %>% 
    select(year:electricity_from_renewables_t_wh) %>% 
    pivot_longer(-c(year), names_to = 'source', values_to = 'energy') %>% 
    group_by(year) %>% 
    mutate(cumsum = cumsum(energy),
           label_ypos=cumsum(energy) - 0.5*energy)

# ggplot(ff, aes(year, energy, col=source)) + geom_line()

# ggplot(ff, aes(year, energy, col=source, fill=source)) + geom_area()


ren<-read.csv('modern-renewable-energy-consumption.csv') %>% clean_names() %>% 
    filter(entity=='World') %>% 
    select(year:hydro_generation_t_wh) %>% 
    pivot_longer(-c(year), names_to = 'source', values_to = 'energy') %>% 
    group_by(year) %>% 
    mutate(label_ypos=cumsum(energy) - 0.5*energy)
    

# ggplot(ren, aes(year, energy, col=source)) + geom_line()

## check units
ff %>% filter(year == 2021 & source == 'electricity_from_renewables_t_wh') %>% summarise(sum(energy))
ren %>% filter(year == 2021) %>% summarise(sum(energy))


subs<-read.csv('global_fossil_fuel_subsidies.csv') %>% clean_names() %>% 
    pivot_longer(-year, names_to = 'fuel', values_to = 'billion_usd') 
