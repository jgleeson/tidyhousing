# This code shows how to use tidyxl and unpivotr to turn a messy Excel table into tidy data

library(tidyverse)
library(tidyxl)
library(readxl)
library(unpivotr)
library(rio)
library(lubridate)

## This script seeks to convert MHCLG's table 253a into tidy data
# The table shows quarterly housebuilding by LA and is on multiple sheets
# First, download the data and import the cells
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/743685/LiveTable_253a.xlsx"
destfile <- "LiveTable_253a.xlsx"
curl::curl_download(url,destfile)
data <- xlsx_cells(path = "LiveTable_253a.xlsx")

# Through trial and error I found that this dataset needed some work before tidying
data$character[data$sheet > "2010 Q3" & data$row == 3 & data$col == 8] <- "Dwellings started"
data$data_type[data$sheet > "2010 Q3" & data$row == 3 & data$col == 8] <- "character"

# The structure of the data changes subtly in 2014, and I found the only way to deal with this
# was to split it and apply separate filters
data_pre2014Q2 <- data %>% 
  filter(col >3 & row > 2) %>% 
  filter(data_type != "blank") %>% 
  filter(sheet < "2014 Q2") %>%
  select(1,3:4,6,9,11,21)

data_post2014Q2 <- data %>% 
  filter(col >3 & row > 3) %>% 
  filter(data_type != "blank") %>% 
  filter(sheet > "2014 Q1") %>%
  select(1,3:4,6,9,11,21)

dataf <- rbind(data_pre2014Q2, data_post2014Q2)

# Describe the beheading function 
unpivot <- function(cells){
  cells %>% 
    behead("NNW", "Type") %>% # Type heading is above and to the left
    behead("N", "Provider") %>% # The provider heading is above
    behead("W", "LA code")
}

# Apply that to each sheet.
updata.sheet <- dataf %>%
  nest(-sheet) %>%
  mutate(data = map(data, unpivot)) %>%
  unnest()

# We now have a dataframe to work with
# again this has to be done using judgement, trial and error.
# Here we filter to only those rows that have data for supply, provider type and LA code.
final <- updata.sheet %>%
  filter_at(vars(numeric, Provider, `LA code`), all_vars(!is.na(.)))

# Join on LA and region names
lookup <- import("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/userinformationenglandandwaleslocalauthoritytoregionlookup/june2017/lookuplasregionew2017.xlsx", skip=4)
final <- left_join(final, lookup, by="LA code")

# Rename some variables, remove others and tidy up
final <- final %>%
  select(sheet, Type, Provider, `LA code`, `LA name`, `Region code`, `Region name`, `numeric`) %>%
  rename(Quarter = sheet,
         LA.code = `LA code`,
         LA.name = `LA name`,
         Region.code = `Region code`,
         Region.name = `Region name`,
         Number = numeric) 

# Turn the quarter variable into the date of the end of each quarter
# This helps with ggplotting 
final$Date <- parse_date_time(final$Quarter, orders = "Yq") %>%
  ceiling_date("quarter", change_on_boundary = T) - days(1)

# Tidy up some of the text values
final$Provider <- gsub("\r?\n|\r", " ", final$Provider)
final$Type <- trimws(final$Type, "both") # this fails to remove the trailing space, strangely
final$Type <- str_trim(final$Type) # this works

# Let's try plotting an annualised total
final %>%
  group_by(Region.name) %>%
  filter(Type == "Dwellings started" & Provider == "Housing Associations") %>%
  filter(!is.na(Region.name)) %>%
  group_by(Region.name, Date) %>%
  summarise(Total = sum(Number)) %>% # calculate regional sums
  mutate(annual_total = zoo::rollapplyr(Total, width=4, FUN=sum, partial=TRUE)) %>% # annualised totals
  slice(4:n()) %>% # skip the first three rows when plotting annualised data
  ggplot(aes(x = Date, y = annual_total, colour = Region.name, group = Region.name)) +
  geom_line() +
  facet_wrap(~Region.name) +
  guides(colour = FALSE) # drop legend

