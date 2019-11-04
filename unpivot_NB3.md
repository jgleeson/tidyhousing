Tidying EPC statistics
================
James Gleeson
November 2019

#### Introduction

This notebook shows how to use R code to turn a messy official
statistics spreadsheet into [tidy
data](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html),
which makes it easier to analyse. This is the latest in what is so far a
short series of scripts to tidy up messy or complex official statistics
on housing, gathered together under the
[`tidyhousing`](https://github.com/jgleeson/tidyhousing) heading.

As with the other `tidyhousing` scripts, the code here uses Duncan
Garmonsway’s [tidyxl](https://github.com/nacnudus/tidyxl) and
[unpivotr](https://nacnudus.github.io/unpivotr/) packages, as well as
many of the tips in his [guide to tidying complex
spreadsheets](https://nacnudus.github.io/spreadsheet-munging-strategies/).

The table we’ll be tidying this time is MHCLG’s [live table
NB3](https://www.gov.uk/government/statistical-data-sets/live-tables-on-energy-performance-of-buildings-certificates),
which shows the number of new dwellings registered with Energy
Performance Certificates, along with their floor area. This data is
particularly useful for tracking housebuilding as it seems to have much
better coverage than MHCLG’s longer standing [quarterly housebuilding
data](https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building),
but the summary tables published by MHCLG are quite messy. The same
figures could be derived from the record-level EPC data published by
[Open Data Communities](https://epc.opendatacommunities.org/), but that
requires registration and downloading very large amounts of data, so if
you’re just interested in tidy high-level summary figures then the
approach set out below should help.

#### Getting started

Let’s start by loading the packages we’ll be using, which you’ll need to
install if you don’t already have them.

``` r
library(tidyverse)
library(tidyxl)
library(readxl)
library(unpivotr)
library(rvest)
library(rio)
library(geofacet)
```

#### Download and import the spreadsheet

Now we need to download and import the right file. This function scans
the page of tables, finds the right link based on a search string you
input (you might need to check the page source to find this) and
downloads the file to your working directory.

``` r
get_file <- function(link, string, destfile){
  page <- read_html(link)
  page_links <- html_attr(html_nodes(page, "a"), "href")
  download_url <- page_links[str_which(page_links, string)][1]
  curl::curl_download(download_url,destfile)
}

get_file(link = "https://www.gov.uk/government/statistical-data-sets/live-tables-on-energy-performance-of-buildings-certificates",
         string = "NB3",
         destfile = "NB3.xlsx")
```

Now we can import the data using the `xlsx_cells` function from
`tidyxl`, which returns a row for each cell in the original spreadsheet
with a number of variables describing its location, contents and format.

``` r
data <- xlsx_cells(path = "NB3.xlsx", sheets = "NB3 by LA")
```

In many cases you’ll want to filter the original data to a subset of
columns and rows and you’ll also want to select certain cell
characteristics. The last line of the below code selects the row, col,
data\_type, numeric, character & local\_id characteristics. Because of a
clash with function names (I think in purrr), you have to select these
by
numbers.

``` r
dataf <- data %>% filter(col < 4 & row > 1) %>% # we're only interested in the first four columns
  select(3:4,6,9,11,21) 
```

#### Tidy the data by identifying partitions and headings

The data needs to be ‘partitioned’ into a list of tables, one for each
local authority. To do this we’ll need to identify the ‘corners’ of each
partition, which in this case is where the LA names are. This needs to
be done by manually inspecting the data to find unique formats /
position of partition labels. Rather unhelpfully, it also seems to be
unstable from quarter to quarter - sometimes the local\_format\_id you
need is 19, sometimes 14

``` r
corners <- filter(dataf, local_format_id == 14 & col == 1) 
partition <- partition(dataf, corners)
```

From scrutinising the results we can identify the procedure for
‘beheading’ our data. First we write this into a function, then map
that function onto each of our partitions.

``` r
unpivot <- function(x){
  x %>% 
    behead("NNW", "LA") %>% # The LA heading is up and to the left
    behead("W", "Year") %>% # the Year heading is to the far left
    behead("W", "Quarter") # The Quarter headings is to the left (before you get to Year)
}

# map that function onto each of our partitions
end <- map(partition$cells, unpivot)
```

We now have a partitioned list of dataframes which we’d like to filter
and select from, so we create a function to do that for one dataframe
and map that onto each item in our list.

``` r
nb3filter <- function(x){
  x %>%
    filter(!Quarter %in% c("Year Total", "Year Quarter")) %>%
    select(LA, Quarter, numeric)
}

# and map that to each one in the list
final <- map(end,nb3filter) %>% bind_rows
```

#### Join on regional names and calculate annualised totals

Currently this data only has local authority names for identifiers, so
let’s join on the ONS codes and regional
names.

``` r
lookup <- import("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/userinformationenglandandwaleslocalauthoritytoregionlookup/june2017/lookuplasregionew2017.xlsx", skip=4)
final <- left_join(final, lookup, by=c("LA" = "LA name"))
```

Because of the seasonal pattern in the quarterly data, it also makes
sense to calculate rolling annualised totals.

``` r
final <- final %>%
  group_by(LA) %>%
  mutate(annual_total = zoo::rollapplyr(numeric, width=4, FUN=sum, partial=TRUE))
```

Finally we need to make some basic tweaks to the data to get it ready
for analysis and plotting.

``` r
# make names syntactically valid
names(final) <- make.names(names(final))

# parse the quarter variable into date format
final$date <- lubridate::yq(final$Quarter)
```

#### Plotting

Now for some analysis and plotting. Let’s start with regional summaries
from 2012 Q4 on (when data quality improved), excluding Wales.

``` r
regions <- final %>%
  filter(Quarter > "2012/3", Region.name != "Wales") %>%
  group_by(Region.name, Quarter, date) %>%
  summarise(total=sum(annual_total)) %>%
  mutate(year=as.numeric(substr(Quarter, 1, 4)))

ggplot(data=regions, aes(x=date, y=total, group=Region.name, colour=Region.name)) +
  geom_line() +
  facet_wrap(~Region.name)+
  expand_limits(y = 0)
```

![](unpivot_NB3_files/figure-gfm/regional%20totals-1.png)<!-- -->

We can also do a (fairly rough-round-the-edges) chart-map of London
boroughs using the excellent `geofacet` package.

``` r
final %>% filter(Quarter > "2012/3", Region.name=="London") %>%
  ggplot(mapping=aes(x=date, y=annual_total)) +
  geom_area() + 
  facet_geo(~LA, grid="london_boroughs_grid", label="name") + 
  scale_x_date(breaks = '3 years', date_labels = '%Y') +
  scale_y_continuous(breaks = c(0, 2000, 4000), labels = c("0","2k","4k")) +
  labs(title = "Housing supply trends trends in London boroughs 2012-2019",
       caption = "Annualised EPC new dwelling totals",
       x = "Year",
       y = "Annualised number of new dwellings") +
  theme(strip.text.x = element_text(size = 10, margin=margin(0.1,0,0.1,0,"cm")),
        strip.background = element_rect(fill = "slategray2"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
```

![](unpivot_NB3_files/figure-gfm/London%20borough%20plot-1.png)<!-- -->

And the last task is to write the data to a CSV file.

``` r
write.csv(final, "EPC_LA.csv")
```
