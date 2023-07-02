## code to prepare `bidens_speeches` dataset goes here
library(lubridate)
library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)

# read files
charlotte <- "Charlotte_Sep23_2020_Racial_Equity_Discussion.txt"
cleveland <- "Cleveland_Sep30_2020_Whistle_Stop_Tour.txt"
milwaukee <- "Milwaukee_Aug20_2020_Democratic_National_Convention.txt"
philly <- "Philadelphia_Sep20_2020_SCOTUS.txt"
washington <- "Washington_Sep26_2020_US_Conference_of_Mayors.txt"
wilmington <- "Wilmington_Nov25_2020_Thanksgiving.txt"

bidens_speeches_data <- read_delim(file=c(charlotte,cleveland,
                        milwaukee,philly,
                        washington,wilmington),
                 delim=' ', col_types = 'c')
#change the column names
colnames(bidens_speeches_data) <- c('speech', 'part')

#add the location
library(stringr)

#extract the first word into a new column
bidens_speeches_data$location <- str_extract(bidens_speeches_data$part,"(\\w+)")

#split the new location column
bidens_speeches_data[c('location', 'other')] <- str_split_fixed(bidens_speeches_data$location, '_', 2)

#delete the other column
bidens_speeches_data <- bidens_speeches_data %>% select(-other)


#add the event column
bidens_speeches_data<- bidens_speeches_data %>%
  mutate(event = case_when(location == 'Charlotte' ~ 'Racial Equity Discussion',
                           location == 'Cleveland'  ~ 'Whistle Stop Tour',
                           location == 'Milwaukee'  ~ 'Democratic National Convention',
                           location == 'Philadelphia' ~ 'SCOTUS',
                           location == 'Washington' ~ 'US Conference of Mayors',
                           TRUE ~ 'Thanksgiving'))

#add the date column
bidens_speeches_data<- bidens_speeches_data %>%
  mutate(date = case_when(location == 'Charlotte' ~ as_date('2020-09-23'),
                          location == 'Cleveland'  ~ as_date('2020-09-30'),
                          location == 'Milwaukee'  ~ as_date('2020-08-20'),
                          location == 'Philadelphia' ~ as_date('2020-09-20'),
                          location == 'Washington' ~ as_date('2020-09-26'),
                          TRUE ~ as_date('2020-11-25')))


#bidens_speeches_data$speech <- str_replace_all(bidens_speeches_data$speech, "['’]", ".")
#bidens_speeches_data$speech <- str_replace_all(bidens_speeches_data$speech, "…", "")
bidens_speeches_data$speech <- gsub("[^[:ascii:]]", "", bidens_speeches_data$speech, perl = TRUE)
bidens_speeches_data <- write_excel_csv(bidens_speeches_data, 'C:\\Users\\Yahdii\\OneDrive\\Desktop\\513 Assignment\\bidens_speeches_data.csv')

#install.packages("usethis")
usethis::use_data(bidens_speeches_data, overwrite = TRUE)
usethis::use_pipe()
