tidyhousing: MHCLG table 253 (annual housebuilding)
================
James Gleeson
May 2019

#### Introduction

This notebook shows how to use R code to turn a (very) messy official statistics spreadsheet into [tidy data](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html), which makes it easier to analyse. This is the first of what will hopefully be many scripts to tidy up messy or complex official statistics on housing, which I'll publish under the `tidyhousing` heading.

The code here uses Duncan Garmonsway's brilliant [tidyxl](https://github.com/nacnudus/tidyxl) and [unpivotr](https://nacnudus.github.io/unpivotr/) packages, as well as many of the tips in his [guide to tidying complex spreadsheets](https://nacnudus.github.io/spreadsheet-munging-strategies/). If you're not familiar with these approaches Duncan's [guide](https://nacnudus.github.io/unpivotr/) to using `unpivotr` is a good introduction.

The table we'll be tidying is MHCLG's [live table 253](https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building), which shows 'permanent dwellings started and completed, by tenure and district' in England by financial year back to 1980/81. This data is valuable for historical purposes (even if it less accurate in recent years, as discussed by [Neal Hudson](http://resi-analysts.com/wp-content/uploads/2019/01/Counting-Houses-Rightmove-2018.11.20.pdf)), but it's quite difficult to construct any trends from the published spreadsheet as each year's data is on a different sheet.

As it turns out, this is a doozy of a messy spreadsheet, due to a combination of hidden columns, variables that appear and disappear from year to year, header cells that merge and unmerge, numbers formatted as characters and lots of missing data. So tidying it requires a fairly complex procedure, which took quite a while to develop. No doubt this procedure could also be improved - comments and suggestions are welcomed!

#### Getting started

Let's start by loading the packages we'll be using, which you'll need to install from [CRAN](https://cran.r-project.org/web/packages/index.html) if you don't already have them.

``` r
library(tidyverse)
library(tidyxl)
library(readxl)
library(unpivotr)
library(rvest)
```

#### Download and import the spreadsheet

Now we need to download and import the right file. This function scans the page of tables, find the right link based on a search string you import (you might need to check the page source to find this) and downloads the file to your working directory.

``` r
get_file <- function(link, string, destfile){
  page <- read_html(link)
  page_links <- html_attr(html_nodes(page, "a"), "href")
  download_url <- page_links[str_which(page_links, string)][1]
  curl::curl_download(download_url,destfile)
}

get_file(link = "https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building",
         string = "LiveTable253",
         destfile = "LiveTable_253.xlsx")
```

Now we can import the data using the `xlsx_cells` function from `tidyxl`, which returns a row for each cell in the original spreadsheet with a number of variables describing its location, contents and format. After visual inspection of the spreadsheet I've decided to skip the first two rows of every sheet, filter out blank cells, filter out the cell containing the extraneous "Number of dwellings" heading, and do some formatting to make sure all the data is ready for analysis.

``` r
data <- xlsx_cells(path = "LiveTable_253.xlsx") %>%
  filter(row>2) %>% # skip the first two rows
  filter(!is_blank) %>% # exclude blank cells
  filter(!str_detect(character, "Number of dwellings") | is.na(character)) %>% # exclude this random heading
  mutate(character = gsub("\r?\n|\r", " ", character)) %>% # convert text characters to spaces
  mutate(numeric = case_when(
    str_detect(character, "\\d{2,}") == TRUE ~ as.numeric(gsub(",", "", character)), # convert text numbers to numeric
    TRUE ~ numeric
  ))
```

#### Split into chunks and tidy

One significant complication here is that there's a format change at 1998-99 when national and regional totals are introduced, then another at 2011-12 when they are taken away again but area type labels added in. The best way I could find to deal with this was to split the imported data into three periods.

``` r
data.first<- data %>%
  filter(sheet %in% c("8081":"9798")) 

data.second <- data %>%
  filter(sheet %in% c("1998-99", "1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
                      "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11")) 

data.third <- data %>%
  filter(!sheet %in% c("8081":"9798", "1998-99", "1999-00", 
                       "2000-01", "2001-02", "2002-03", "2003-04", "2004-05",
                       "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11"))
```

Now we create a function to tidy each sheet in each chunk of data. Each function includes a number of steps:

-   [Justifying](https://nacnudus.github.io/spreadsheet-munging-strategies/pivot-complex.html#centre-aligned-headers) the column headers where they have been unmerged so that they can be matched to the relevant numeric cells
-   Using [enhead](https://nacnudus.github.io/unpivotr/reference/enhead.html) to ensure these justified headings are recognised as such
-   Using [behead](https://nacnudus.github.io/unpivotr/reference/behead.html) to identify headings that are already in the right positions in the spreadsheet.

First chunk first:

``` r
unpivot.first <- function(cells){
  first_header <- cells %>% filter(str_detect(character, "Dwellings ")) %>%
    select(row, col, Type = character)
  first_header_corners <- cells %>% filter(str_detect(character, "Private Enterprise")) %>%
    select(row, col, Type = character) %>% mutate(row = row -1)
  first_header<- justify(first_header, first_header_corners)
  second_header <- cells %>%
    filter(character %in% c("Private Enterprise",   "Housing Associations", "Local Authority",  "All")) %>%
    select(row, col, Provider = character)
  cells %>% 
    behead("W", DCLG_code) %>%
    behead("W", Former_ONS_code) %>%
    behead("W", Current_ONS_code) %>%
    behead("W", Upper_area_total) %>%
    behead("W", Area_name) %>%
    enhead(first_header, "NNW") %>%
    enhead(second_header, "N") %>%
    filter(!is.na(numeric))
}

output.first <- data.first %>%
  nest(-sheet) %>%
  mutate(data = map(data, unpivot.first)) %>%
  unnest() %>%
  select(Year = sheet, Type, Provider, DCLG_code, Former_ONS_code, Current_ONS_code, Upper_area_total, Area_name, "Number" = numeric)

# Fix year labels in the first chunk
output.first$Year <- str_c("19", str_sub(output.first$Year, start = 1L, end = 2L), "-",  
                            str_sub(output.first$Year, start = 3L, end = 4L)) 

knitr::kable(head(output.first)) # Let's take a look at the result
```

| Year    | Type              | Provider             | DCLG\_code | Former\_ONS\_code | Current\_ONS\_code | Upper\_area\_total | Area\_name |  Number|
|:--------|:------------------|:---------------------|:-----------|:------------------|:-------------------|:-------------------|:-----------|-------:|
| 1980-81 | Dwellings started | Private Enterprise   | NA         | NA                | NA                 | England            | NA         |   86510|
| 1980-81 | Dwellings started | Housing Associations | NA         | NA                | NA                 | England            | NA         |   11050|
| 1980-81 | Dwellings started | Local Authority      | NA         | NA                | NA                 | England            | NA         |   29400|
| 1980-81 | Dwellings started | All                  | NA         | NA                | NA                 | England            | NA         |  126960|
| 1980-81 | Dwellings started | Private Enterprise   | NA         | NA                | NA                 | Avon               | NA         |    1970|
| 1980-81 | Dwellings started | Housing Associations | NA         | NA                | NA                 | Avon               | NA         |     200|

Nowe we can do the same for the second and third chunks.

``` r
unpivot.second <- function(cells){
  first_header <- cells %>% filter(str_detect(character, "Dwellings ")) %>%
    select(row, col, Type = character)
  first_header_corners <- cells %>% filter(str_detect(character, "Private Enterprise")) %>%
    select(row, col, Type = character) %>% mutate(row = row -1)
  first_header<- justify(first_header, first_header_corners)
  second_header <- cells %>%
    filter(character %in% c("Private Enterprise",   "Housing Associations", "Local Authority",  "All")) %>%
    select(row, col, Provider = character)
  cells %>% 
    behead("W", DCLG_code) %>%
    behead("W", Former_ONS_code) %>%
    behead("W", Current_ONS_code) %>%
    behead("W", Nat_reg_total) %>%
    behead("W", Upper_area_total) %>%
    behead("W", Area_name) %>%
    enhead(first_header, "NNW") %>%
    enhead(second_header, "N") %>%
    filter(!is.na(numeric))
}

output.second <- data.second %>%
  nest(-sheet) %>%
  mutate(data = map(data, unpivot.second)) %>%
  unnest() %>%
  select(Year = sheet, Type, Provider, DCLG_code, Former_ONS_code, Current_ONS_code, 
         Nat_reg_total, Upper_area_total, Area_name, "Number" = numeric)

unpivot.third <- function(cells){
  first_header <- cells %>% filter(str_detect(character, "Dwellings ")) %>%
    select(row, col, Type = character)
  first_header_corners <- cells %>% filter(str_detect(character, "Private Enterprise")) %>%
    select(row, col, Type = character) %>% mutate(row = row -1)
  first_header<- justify(first_header, first_header_corners)
  second_header <- cells %>%
    filter(character %in% c("Private Enterprise",   "Housing Associations", "Local Authority",  "All")) %>%
    select(row, col, Provider = character)
  cells %>% 
    behead("W", Area_type_total) %>%
    behead("W", DCLG_code) %>%
    behead("W", Former_ONS_code) %>%
    behead("W", Current_ONS_code) %>%
    behead("W", Upper_area_total) %>%
    behead("W", Area_name) %>%
    enhead(first_header, "NNW") %>%
    enhead(second_header, "N") %>%
    filter(!is.na(numeric))
}

output.third <- data.third %>%
  nest(-sheet) %>%
  mutate(data = map(data, unpivot.third)) %>%
  unnest() %>%
  select(Year = sheet, Type, Provider, Area_type_total, DCLG_code, Former_ONS_code, 
         Current_ONS_code, Upper_area_total, Area_name, "Number" = numeric)
```

#### Combine chunks and fill out empty cells

Now we need to combine those three chunks into one.

``` r
# Function to standardise the chunks
process_output <- function(chunk) {
  chunk %>%
    select(Year, Type, Provider, Upper_area_total, Current_ONS_code, Area_name, Number)
}

# Create a combined output dataset by applying that function to each chunk
combined.output <- map_dfr(outputs <- list(output.first, output.second, output.third), process_output)
```

The procedure above doesn't import any information for cells with missing data, so for completeness and for the purposes of analysis, let's 'fill out' the combination of headers using the [combine](https://dplyr.tidyverse.org/reference/combine.html) function from `dplyr`.

``` r
combined.output <- combined.output %>%
  complete(Year, Type, Provider, nesting(Upper_area_total, Current_ONS_code, Area_name))
```

#### Visualisation

And that's it! Well, it's as far as we're going to take it for now.

Let's finish off with some visualisation. The chart below shows the number of new build homes started in each London borough by year.

``` r
# Tile map of starts by London borough
library(viridis)
combined.output %>%
  filter(str_detect(Current_ONS_code, "E09")) %>%
  filter(str_detect(Type, "started")) %>%
  filter(Provider == "All") %>%
  ggplot(mapping=aes(x = Year, 
                     y = factor(Area_name, levels = rev(levels(factor(Area_name)))))) +
  geom_tile(aes(fill = Number), colour = "white", height = 1, width = 1) +
  labs(title = "New build starts by London borough, 1980/81 to 2017/18",
       x = NULL, y = NULL, subtitle = "Grey cells indicate missing data") +
  scale_fill_viridis_c(option = "plasma", name="Number",
                     guide=guide_colorbar(frame.colour="black", ticks.colour="black")) +
  scale_x_discrete(breaks = c("1980-81", "1990-91", "2000-01", "2010-11"))
```

![](unpivot_253_md_files/figure-markdown_github/tile%20map-1.png)
