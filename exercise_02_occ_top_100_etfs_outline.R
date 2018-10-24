## Load all the libraries that you will need.

library(dplyr)
library(readr)
library(tidyverse)

## Read in the data from CSVs.

df_occ<- readr::read_csv("data_occ_201808.csv")
df_etfs<- readr::read_csv("data_etf_list.csv")


## Create a monthly volume report.
df_monthly_volume <- df_occ %>% group_by(symbol) %>% 
                     summarize(monthly_volume = sum(quantity))




## Look at df_etfs and examine the segment column. 
## Look for clues about whether the fund is volatility related.

df_etfs$segment

#########Those with the label of "volatility" are volatility-related. 



## Convert segment column of df_etfs to all lowercase letters.

df_etfs$segment<-base::tolower(df_etfs$segment)

## Isolate volatility etfs into a dataframe called df_vol_etfs.

df_vol_etfs<-
  dplyr::filter(df_etfs, stringr::str_detect(df_etfs$segment, "volatility"))

##### another method:
df_vol_etfs <- 
  df_etfs %>%  filter(str_detect(segment, "volatility"))


## Create a list of top 100 most liquid non-volatility ETFs.
## Call it df_top_100.

A <-
  df_monthly_volume %>% 
  filter(symbol %in% df_etfs$symbol) %>% 
  filter(! symbol %in% df_vol_etfs$symbol) %>% 
  arrange(desc(monthly_volume)) %>% 
  top_n(100)

   
df_top_100 <-
  df_etfs %>% 
  filter(symbol %in% (A$symbol))
 

  
## Create a dataframe consisting of the distinct by the df_top_100 symbols?

distinct(df_top_100, segment)

df_group <-
  df_top_100 %>% 
  group_by(segment) %>% 
  summarise(count = n()) 


## Come up with a meaningful grouping of these segments.

#### Sorry, I don't know how to set up 5-10 groups. Does it mean I have to filter
### the segments in terms of the strings "equity", "currency", "fixed income",etc?
