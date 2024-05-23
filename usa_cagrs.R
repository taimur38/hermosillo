library(tidyverse)
library(readxl)
library(ggrepel)

# take all the \n out of the column names
dat1 <- read_excel("data/usa-pop/tab01a.xls", skip = 10) %>%
    filter(!is.na(`Metro/\nMicro Area\nCode` )) %>%
    filter(`Legal/Statistical\nArea Description` == "Metropolitan Statistical Area") %>%
    rename_with(~str_replace_all(., "\n", " "), everything())  %>%
    rename(msa_name =  `United States Puerto Rico Metropolitan Statistical Area Micropolitan Statistical Area`) %>%
    rename(
           pop_2000 = `Population`,
           pop_1990 = `...7`
    )


dat1

names(dat1)

dat2 <- read_csv("data/usa-pop/cbsa-est2022.csv") %>%
    filter(LSAD == "Metropolitan Statistical Area")


dat1 %>%
    select(`Metro/ Micro Area Code`, msa_name, pop_1990, pop_2000) %>%
    left_join(
        dat2 %>%
            select(CBSA, NAME, POPESTIMATE2020) %>%
            mutate(CBSA = as.character(CBSA)),
        by = c("Metro/ Micro Area Code" = "CBSA")
    ) %>%
    rename(pop_2020 = POPESTIMATE2020) %>%
    mutate(
           pop_1990 = as.numeric(pop_1990),
           pop_2000 = as.numeric(pop_2000),
           
           cagr = (pop_2020 / pop_1990)^(1/30) - 1,
    )  %>%
    write_csv("data/usa-pop/msa_cagrs.csv")



    ggplot(aes(x=pop_1990, y=cagr, label=msa_name)) +
    geom_point() +
    geom_text_repel() 



