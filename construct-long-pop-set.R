library(tidyverse)
library(haven)
library(ggthemes)
library(ggrepel)

zones <- read_dta('data/zonamet_expanded.dta') %>%
    mutate(ZM = ifelse(ZM == "San Luis Potos\xed-Soledad", "San Luis Potos-Soledad", ZM))

top_zones <- read_csv('data/top-zones.csv')

muni_old <- read_csv('data/muni1970-1980.csv')

capital_pops <- read_csv('data/state-capital-pops.csv')

caps_zonas <- read_csv('data/mex_caps_zonas.csv')



zones %>%
    filter(grepl("Valle", ZM))

theme_set(theme_few())

# extract the number part out of entidad federativa and municipio
zonas_oldpop <- muni_old %>%
    mutate(
           entidad_n = str_extract(`Entidad federativa`, '\\d+'),
           muni_n = str_extract(Municipio, '\\d+'),
           geocode = paste0(entidad_n, muni_n),
           `1970` = as.numeric(gsub(',', '', `1970`)),
           `1980` = as.numeric(gsub(',', '', `1980`))
    ) %>%
    filter(!is.na(entidad_n)) %>%
    filter(!is.na(muni_n)) %>%
    select(`Entidad-clean`, `Municipio-clean`, `1980`, `1970`, geocode) %>%
    left_join(zones) %>%
    group_by(codeZM, ZM) %>%
    summarise(
              pop_1980 = sum(`1980`, na.rm=T),
              pop_1970 = sum(`1970`, na.rm=T)
    ) 

# the hermosillo population numbers add up

zone_pops <- read_csv('data/zone_pops.csv')

zone_final <- zone_pops %>%
    select(codeZM, ZM, año, pop) %>%
    rename(
           year = año
    )


zone_final <- zonas_oldpop %>%
    pivot_longer(c('pop_1980', 'pop_1970'), names_to='year', values_to='pop') %>%
    mutate(
           year = as.numeric(gsub('pop_', '', year))
    ) %>%
    rbind(zone_final) %>% 
    arrange(codeZM, year) 


write_csv(zone_final, 'data/zonas_1970_pops.csv')


combined_state_capitals <- capital_pops %>%
    pivot_longer(-State, names_to='year', values_to='pop') %>%
    left_join(caps_zonas, by=c('State')) %>%
    select(codeZM, year, pop) %>%
    left_join(zones %>%
              select(codeZM, ZM) %>% 
              unique(), 
          by='codeZM') %>%
    rbind(zone_final) 


write_csv(combined_state_capitals, 'data/long_state_capitals.csv')

