# This code shows how to use tidyxl and unpivotr to turn a messy Excel sheet into tidy data

library(tidyverse)
library(tidyxl)
library(readxl)
library(unpivotr)
library(rio)
library(lubridate)
library(geofacet)

## The first example is MHCLG's table NB3 on Energy Performance Certificates in new build homes
# NB tidyxl only works with xlsx files, so if it's an xls you'll have to resave it
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/752442/NB3_-_Domestic_EPCs.xlsx"
destfile <- "NB3.xlsx"
curl::curl_download(url,destfile)
data <- xlsx_cells(path = "NB3.xlsx", sheets = "NB3 by LA")

# this creates a table with a row for each cell, summarising its characteristics
# In many cases you'll want to filter the original data to a subset of columns and rows
# and you'll also want to select certain cell characteristics. 
# The last line of the below code selects the row, col, data_type, numeric, character & local_id characteristics
# Because of a clash with function names (I think in purrr), you have to select these by numbers
dataf <- data %>% filter(col < 4 & row > 1) %>% # we're only interested in the first four columns
  select(3:4,6,9,11,21) 

# Does the data need to be partitioned into a list of tables, e.g. one for each local authority?
# Then you'll need to identify the 'corners' of each partition, which in this case is where the LA names are
# This needs to be done by manually inspecting the data to find unique formats / position of partition labels
corners <- filter(dataf, local_format_id == 18 & col == 1) 
partition <- partition(dataf, corners)

# From scrutinising the results we can identify the procedure for 'beheading' our data
# write this into a function
unpivot <- function(x){
  x %>% 
    behead("NNW", "LA") %>% # The LA heading is up and to the left
    behead("W", "Year") %>% # the Year heading is to the far left
    behead("W", "Quarter") # The Quarter headings is to the left (before you get to Year)
}

# map that function onto each of our partitions
end <- map(partition$cells, unpivot)

# we now have a partitioned list of dataframes which we'd like to filter and select from
# so create a function to do that for one dataframe
nb3filter <- function(x){
  x %>%
    filter(!Quarter %in% c("Year Total", "Year Quarter")) %>%
    select(LA, Quarter, numeric)
}

# and map that to each one in the list
final <- map(end,nb3filter) %>% bind_rows

# Join on data on regions
lookup <- import("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/userinformationenglandandwaleslocalauthoritytoregionlookup/june2017/lookuplasregionew2017.xlsx", skip=4)
final <- left_join(final, lookup, by=c("LA" = "LA name"))

# rolling annualised totals
final <- final %>%
  group_by(LA) %>%
  mutate(annual_total = zoo::rollapplyr(numeric, width=4, FUN=sum, partial=TRUE))

# make names syntactically valid
names(final) <- make.names(names(final))

# parse the quarter variable into date format
final$date <- yq(final$Quarter)

# regional summaries from 2012 Q4 on, excluding Wales
regions <- final %>%
  filter(Quarter > "2012/3", Region.name != "Wales") %>%
  group_by(Region.name, Quarter, date) %>%
  summarise(total=sum(annual_total)) %>%
  mutate(year=as.numeric(substr(Quarter, 1, 4)))

ggplot(data=regions, aes(x=date, y=total, group=Region.name, colour=Region.name)) +
  geom_line() +
  facet_wrap(~Region.name) 

final %>% filter(Quarter > "2012/3", Region.name=="London") %>%
  ggplot(mapping=aes(x=date, y=annual_total)) +
  geom_area() + 
  facet_geo(~LA, grid="london_boroughs_grid", label="name") + 
  scale_y_continuous(breaks = c(0, 2000, 4000), labels = c("0","2k","4k")) +
  labs(title = "Housing supply trends trends in London boroughs 2012-2018",
       caption = "Annualised EPC new dwelling totals. Chart by @geographyjim",
       x = "Year",
       y = "Annualised number of new dwellings") +
  theme(strip.text.x = element_text(size = 10, margin=margin(0.1,0,0.1,0,"cm")),
        strip.background = element_rect(fill = "slategray2"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Write this to a CSV
write.csv(final, "EPC_LA.csv")