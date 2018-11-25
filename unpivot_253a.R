# This code shows how to use tidyxl and unpivotr to turn a messy Excel table into tidy data

library(tidyverse)
library(tidyxl)
library(readxl)
library(unpivotr)
library(rio)

## This script seeks to convert MHCLG's table 253a into tidy data
# The table shows quarterly housebuilding by LA and is on multiple sheets
# First, download the data and import the cells
url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/743685/LiveTable_253a.xlsx"
destfile <- "LiveTable_253a.xlsx"
curl::curl_download(url,destfile)
data <- xlsx_cells(path = "LiveTable_253a.xlsx")

# Filter to relevant cells and descriptor columns
dataf <- data %>% filter(col >3 & row > 2) %>% 
  select(1,3:6,9,11,21) 

# Through trying various approaches it seems that we need to limit ourselves to 
# only the relevant rows, so let's get the list of rows where 
# columns 8 to 10 are blank (i.e. no headings or data).
# This filter was chosen by trial and error, made more difficult by the fact that
# the table layout shifts subtly at various points
blanks <- dataf %>% 
  select(sheet, row, col, is_blank) %>%
  filter(col > 7 & col <11) %>%
  spread(col, is_blank) %>%
  filter_at(vars(`8`,`9`,`10`), all_vars(. == TRUE))

# now use that list to filter out these rows with an `anti-join`
dataf <- anti_join(dataf, blanks, by = c("sheet", "row"))

# Describe the beheading function 
unpivot <- function(cells){
  cells %>% 
    behead("NNW", "Type") %>% # Type heading is above and to the left
    behead("N", "Provider") %>% # The provider heading is above
    behead("W", "LA code")
}

# Apply that to each sheet.
# Note, I found that this step fails if you don't strip out blank rows as above
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

# Looking at the data, there's a problem: missing type for completed private units from 2010 Q4 on
# I'm completely sure there's a better way of dealing with this upfront, but for now
# I'll just do a manual post-hoc correction. This wouldn't work in more complex or random cases however.
final <- final %>% 
  replace_na(list(Type = "Dwellings completed"))

# Now we just need to tidy up some textual irregularities
final$Provider <- gsub("[\r\n]", " ", final$Provider)
final$Type <- trimws(final$Type)

# Let's look at the London trend
London <- final %>%
  filter(Region.name == "London" &
           Provider == "All") %>%
  group_by(Region.name, Type, Quarter) %>%
  tally(Number)

# Hmm, for some reason it stops after 2014 Q1, and I can't work out why.
# Presumably something to do with MHCLG cunningly changing the table layouts every couple of years ...

# Another approach would be to strip out the numeric data, then manually identify the
# cells containing the headers, then `enhead` the data. But I couldn't work out how to 
# do this with multiple cells.