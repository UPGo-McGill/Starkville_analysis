library(tidyverse)
library(upgo)
library(strr)

upgo_connect()

city_region_valid <-
  c("Starkville, Mississippi",
     "Oxford, Mississippi",
    "Lexington, Kentucky",
    "Nashville, Tennessee",
    "Knoxville, Tennessee",
    "Colombia, South Carolina",
    "Athens, Georgia",
    "Gainesville, Florida",
    "Auburn, Alabama",
    "Tuscaloosa, Alabama",
    "Fayetteville, Arizona",
    "Columbia, Missouri",
    "College Station, Texas",
    "Baton Rouge, Louisiana"
    )

city_names <- c("Starkville", "Oxford", "Lexington", "Nashville", "Knoxville", "Columbia",
                "Athens", "Gainesville", "Auburn", "Tuscaloosa", "Fayetteville", "Columbia", 
                "College Station", "Baton Rouge")    
     
property <- 
  property_all %>% 
  filter(country == "United States",
         city %in% city_names,
         region %in% c("Georgia", "Mississippi", "Kentucky", "Tennessee", "South Carolina",
                       "Alabama", "Arizona", "Missouri", "Texas", "Louisiana")) %>% 
  collect() 

property %>% 
  mutate(city_region = paste(city, region, sep = ", ")) %>% 
           filter(city_region %in% city_region_valid)

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

max(property$scraped)

ML_hosts <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID)

ML_hosts_with_city <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID, city)
  
  
local_hosts <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID, city) %>% 
  filter(city %in% c("Starkville", "Oxford", "Athens"))

property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  mutate(total_local_listings = length())

property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  left_join(ML_hosts) %>% 
  rename(total_host_listings = n) %>% 
  left_join(local_hosts) %>% 
  rename(total_local_listings = n) %>% 
  select(property_ID, host_ID, total_host_listings, total_local_listings, everything()) %>% 
  group_by(city) %>% 
  summarize(active_listings = n(),
            active_hosts          = length(unique(host_ID)),
            listings_per_host     = active_listings / active_hosts,
            EH_pct         = mean(listing_type == "Entire home/apt"),
            ML_host_pct    = mean(total_host_listings > 1, na.rm = TRUE),
            local_host_pct = mean(total_local_listings > 1, na.rm = TRUE),
            only_nonlocal_pct = mean(total_local_listings == 1 & total_host_listings > 1, na.rm = TRUE)) #%>% 
  write_csv("output.csv")





ML_property %>% 
  group_by(region)
  count(region, sort = TRUE)


ML_property %>% 
  filter(region == "Florida") %>% 
  pull(host_ID) %>% 
  unique()



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



property_all %>% 
  filter(country == "United States",
         city %in% c("Starkville", "Oxford"),
         region %in% c("Mississippi")) %>% 
  explain()

property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

