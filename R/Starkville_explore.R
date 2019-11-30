library(tidyverse)
library(upgo)
library(strr)

upgo_connect()

## Name city region combos 

city_region_valid <-
  c("Starkville, Mississippi",
     "Oxford, Mississippi",
    "Lexington, Kentucky",
    "Nashville, Tennessee",
    "Knoxville, Tennessee",
    "Columbia, South Carolina",
    "Athens, Georgia",
    "Gainesville, Florida",
    "Auburn, Alabama",
    "Tuscaloosa, Alabama",
    "Fayetteville, Arkansas",
    "Columbia, Missouri",
    "College Station, Texas",
    "Baton Rouge, Louisiana"
    )

city_names <- c("Starkville", "Oxford", "Lexington", "Nashville", "Knoxville", "Columbia",
                "Athens", "Gainesville", "Auburn", "Tuscaloosa", "Fayetteville", "Columbia", 
                "College Station", "Baton Rouge")    

## Get properties for all 14 cities 
## (with region combos to make sure both Columbias are kept)

property_cities <- 
  property_all %>% 
  filter(country == "United States",
         city %in% !! city_names,
         region %in% c("Georgia", "Florida", "Mississippi", "Kentucky", "Tennessee", "South Carolina",
                       "Alabama", "Arkansas", "Missouri", "Texas", "Louisiana")) %>% 
  collect() 

property_cities <- property_cities %>% 
  mutate(city_region = paste(city, region, sep = ", ")) %>% 
           filter(city_region %in% city_region_valid)

## Get all host IDs

host_IDs <- property_cities$host_ID
save(host_IDs, file="R/host_IDs.Rdata")

## Get different property and host numbers

ML_property
all_hosts_test <- 
  left_join(ML_hosts_with_city, only_local_hosts) %>% 
  rename(all_listings = n) %>% 
  filter(all_listings != total_host_listings)

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property_cities$host_ID) %>% 
  collect()

ML_hosts <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID) %>% 
  rename(total_host_listings = n)

ML_hosts_with_city <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID, city) %>% 
  rename(total_host_listings = n)
  
local_hosts <- 
  ML_property %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  count(host_ID, city, region) %>% 
  mutate(city_region = paste(city, region, sep = ", ")) %>% 
  filter(city_region %in% city_region_valid) %>% 
  rename(total_local_listings = n)

## Function to find local and nonlocal host IDs

nonlocal_active_hosts <- function(cityx){
  
  local_city <- 
    ML_property %>% 
    filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
    count(host_ID, city, region) %>% 
    rename(count_local = n) %>% 
    filter(city %in% cityx)
  
  non_local_city <- 
    ML_property %>% 
    filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
    count(host_ID, city, region) %>% 
    filter(!(city %in% cityx)) %>% 
    rename(count_nonlocal = n, city_nonlocal = city, region_nonlocal = region)
  
  city_local_nonlocal <- 
    left_join(non_local_city, local_city) %>% 
    drop_na() 
  
  city_local_nonlocal
}

nonlocal_hosts <- map_df(city_names, nonlocal_active_hosts)

## Calculate nonlocal vs local host numbers to get percentage
# This is close to giving what we want, but somehow the summarize
# is not doing the trick.
# Not sure if this is the right way to group_by and summarize to get numbers we need.

# Want: total number of hosts per city with count_nonlocal > 1

nonlocal_hosts_gr <- nonlocal_hosts %>% 
  group_by(city, region) %>% 
  summarize(
    sum_nonlocal_hosts = length(unique(host_ID)),
    sum_nonlocal = sum(count_nonlocal > 1)) # %>% 
arrange(host_ID)

# length(unique(nonlocal_hosts_gr$host_ID))

## Calculate only local hosts 
# Something goes wrong here - after the 2nd left_join df has 14991 rows, but
# one implementing the 2nd filter, it becomes almost the same # of rows as 
# local_hosts, and the summarize gives unique host_ID length of one less than
# total_local_listings for each row. So these numbers don't make sense.

# only_local_hosts <- 
#   ML_property %>% 
#   filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
#   count(host_ID, city, region) %>% 
#   left_join(ML_hosts_with_city) %>% 
#   left_join(local_hosts, na.rm = TRUE) %>% 
#   filter(total_host_listings == total_local_listings) %>% 
#   group_by(city, region) %>% 
#   summarize(only_local_hosts = length(unique(host_ID)))

## Property output 

property_output <- 
  property_cities %>% 
  filter(created <= "2019-09-30", scraped >= "2019-09-30") %>% 
  mutate(city_region = paste(city, region, sep = ", ")) %>% 
  filter(city_region %in% city_region_valid) %>% 
  left_join(ML_hosts) %>% 
  left_join(local_hosts) %>% 
  left_join(nonlocal_hosts_gr) %>% 
  select(property_ID, host_ID, sum_nonlocal_hosts, total_host_listings, total_local_listings, everything()) %>% 
  group_by(city, region, sum_nonlocal_hosts) %>% 
  summarize(active_listings = n(),
            active_hosts          = length(unique(host_ID)),
            listings_per_host     = active_listings / active_hosts,
            EH_pct         = mean(listing_type == "Entire home/apt"),
            ML_host_pct    = mean(total_host_listings > 1, na.rm = TRUE),
            local_host_pct = mean(total_local_listings > 1, na.rm = TRUE)) %>% 
  mutate(pct_active_nonlocal = (sum_nonlocal_hosts/active_hosts)) #%>%
  write_csv("output.csv")





