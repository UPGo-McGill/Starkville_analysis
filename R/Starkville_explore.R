library(tidyverse)
library(upgo)
library(strr)

upgo_connect()

property_Starkville <- 
  property_all %>% 
  filter(country == "United States", region == "Mississippi", city == "Starkville") %>% 
  collect()

names(property_Starkville)

property_Starkville %>% 
  filter(created <= "2019-08-31", scraped >= "2019-08-31",
         housing == TRUE) %>%  #active on that day
  count(host_ID) %>% 
  arrange(-n)
  
property_Starkville %>%  #look up host with most properties from previous search
  filter(host_ID == "183994724") %>% 
  view()

all_Starkville_hosts <- 
  property_all %>% 
  filter(host_ID %in% !! property_Starkville$host_ID) %>% 
  collect()

all_Starkville_hosts %>% 
  count(city) %>% 
  arrange(-n)
