library(tidyverse)
library(pacman)
library(rio)
library(lubridate)


# Import bin data 2016-2018
bin_data <- as.data.frame(import("~/Bring_sites_17.07.20.csv"))
# drop last 2 rows and last 2 columns
bin_data <- bin_data[-c(397, 398),]
bin_data <- bin_data[-c(59, 60)]
# get rid of thousands separator comma
bin_data[23:58] <- apply(bin_data[23:58], 2, gsub, pattern = "([0-9])\\,([0-9])", 
      replacement= "\\1\\2")
# replace spaces by "_" in all column titles
names(bin_data) %>% 
  str_replace_all("\\s","_") %>% 
  tolower

bin_data[bin_data == ""] <- NA

bin_data[,c(23:58)] <- as.numeric(unlist(bin_data[,c(23:58)]))

#Add data for 2019
bin_data_2019 <- import("~/Leeds_glass_recycling/Bring_sites_19_to_20.csv")
names(bin_data_2019) %>% 
  str_replace_all("\\s","_") %>% 
  tolower
bin_data_2019 <- bin_data_2019[-c(399, 400),]
bin_data_2019 <- bin_data_2019[,-c(35:60)]

bin_data_2019[23:34] <- apply(bin_data_2019[23:34], 2, gsub, pattern = "([0-9])\\,([0-9])", 
                         replacement= "\\1\\2")
bin_data_2019[,c(23:34)] <- as.numeric(unlist(bin_data_2019[,c(23:34)]))

#Add data for April 2020 onwards
bin_data_new <- import("~/Leeds_glass_recycling/Bring_sites_from_apr20.csv")
names(bin_data_new) %>% 
  str_replace_all("\\s","_") %>% 
  tolower

# Replace values per cell
bin_data[247, 5] = "Woodhouse"
bin_data[47, 5] = "Beeston and Holbeck"
bin_data[241, 5] = "Otley and Yeadon"
bin_data[367, 5] = "Otley and Yeadon"
bin_data[c(25, 65, 77, 119, 130, 132, 235, 245, 249, 252, 264, 272, 274, 369, 
          389), 5] = "Headingley and Hyde Park"
bin_data[316,2] = "Aberford Road, Garforth"

# Change names to match bin_data_new
bin_data[51,1] <- "Beech Hill Car park"
bin_data[23,1] <- "Yasin Banqueting Suite formerly Beeston Hill Social Club"

bin_data$site_name <- bin_data$site_name %>% str_replace_all(c("Zaccharys Caf\xe9" = "Zaccharys Café",
                                         "The Beech Hotel" = "Beech Hotel",
                                         "The Bramley Villagers" = "Bramley Social Club - Villagers",
                                         "The Brick" = "Brick Hotel",
                                         "Robin Lane Club formerly Royal British Legion Club" = "British Legion Club now Robin Lane Club",
                                         "The Butterbowl" = "Butterbowl",
                                         "Waterside Apartments/ City Island Apartments" = "City Island Apartments / Waterside Apartments",
                                         "The Commercial Pub" = "Commercial Pub",
                                         "Potternewton Depot " = "Potternewton Depot",
                                         "The Red Lion" = "Red Lion",
                                         "Sports Park Weetwood - University of Leeds" = "Leeds University Main Car Park",
                                         "The Gate Hotel" = "Ramshead Hill Shops - LS14 Shops",
                                         "Lord Darcy" = "The Lord Darcy",
                                         "Brownlee" = "The Brownlee",
                                         "33 Town St formerly The New Inn" = "Bar 33, formerly The New Inn"))


bin_data$address <- bin_data$address %>% 
  str_replace_all(c("Calverley Ln, Horsforth, Leeds " = "Calverley Ln, Horsforth Vale, Leeds ",
                    "St Marks Road" = "Eldon Court, St Marks Road"))

# Change bin_data_2019 to match bin_data_new
bin_data_2019[23,1] <- "Yasin Banqueting Suite formerly Beeston Hill Social Club"
bin_data_2019[121,1] <- "Brackenwood Drive/Bus Turning Circle"
bin_data_2019[211,1] <- "North Parade Car Park (formerly  The Licks Car park)"

bin_data_2019$address <- bin_data_2019$address %>% 
  str_replace_all(c("St Marks Road" = "Eldon Court, St Marks Road",
                    "Aberford Road" = "Aberford Road, Garforth",
                    "A58, Weatherby Road Layby, north side" = "Weatherby Road Layby, north side",
                    "A58, Weatherby Road Layby 2, South Side" = "Weatherby Road Layby 2, South Side"))


bin_data_2019$site_name <- bin_data_2019$site_name %>% str_replace_all(c("Zaccharys Caf\xe9" = "Zaccharys Café",
                                                               "The Beech Hotel" = "Beech Hotel",
                                                               "The Bramley Villagers" = "Bramley Social Club - Villagers",
                                                               "The Brick" = "Brick Hotel",
                                                               "British Legion Club" = "British Legion Club now Robin Lane Club",
                                                               "The Butterbowl" = "Butterbowl",
                                                               "Waterside Apartments/ City Island Apartments" = "City Island Apartments / Waterside Apartments",
                                                               "The Commercial Pub" = "Commercial Pub",
                                                               "Potternewton Depot " = "Potternewton Depot",
                                                               "Sports Park Weetwood - University of Leeds" = "Leeds University Main Car Park",
                                                               "The Gate Hotel" = "Ramshead Hill Shops - LS14 Shops",
                                                               "The New Inn now Bar 33" = "Bar 33, formerly The New Inn",
                                                               "Eldon Court" = "Roadside Eldon Court, St Marks Road",
                                                               "Buzz Bingo formerly Gala Bingo" = "Buzz Bingo",
                                                               "Caribbean Cricket Club / Scotthall Road" = "Caribbean Cricket Club",
                                                               "The Red Lion" = "Red Lion",
                                                               "Sweet Basil / China Red" = "Sweet Basil Valley formerly China Red",
                                                               "Whetherby" = "Wetherby",
                                                               "Wyke Ridge Gold Club" = "Leeds Golf Centre (former Wike Ridge Inn)"))


#Add or replace values in rows that are near-duplicate
bin_data_new[167, -c(1:22)] <- bin_data_new[167, -c(1:22)] + bin_data_new[168, -c(1:22)]
bin_data[256,c(46:58)] <- bin_data[270,c(46:58)]


# Concatenate site_name and address for duplicate site_names, both in bin_data
# and in bin_data_new

# 1)    FOR BIN_DATA
# Layby
bin_data[160:162,1] <- paste(bin_data[160:162,1], bin_data[160:162,2])
# Morrisons
bin_data[192:198,1] <- paste(bin_data[192:198,1], bin_data[192:198,2])
# Roadside
bin_data[226:280,1] <- paste(bin_data[226:280,1], bin_data[226:280,2]) 
bin_data[282:285,1] <- paste(bin_data[282:285,1], bin_data[282:285,2])
#Sainsburys
bin_data[295:296,1] <- paste(bin_data[295:296,1], bin_data[295:296,2])
#Tescos
bin_data[316:318,1] <- paste(bin_data[316:318,1], bin_data[316:318,2])
#Wagon and Horses
bin_data[371:372,1] <- paste(bin_data[371:372,1], bin_data[371:372,2])


# 2)     FOR BIN_DATA_NEW
# Layby
bin_data_new[166:169,1] <- paste(bin_data_new[166:169,1], bin_data_new[166:169,2])
# Morrisons
bin_data_new[198:204,1] <- paste(bin_data_new[198:204,1], bin_data_new[198:204,2])
# Roadside
bin_data_new[234:291,1] <- paste(bin_data_new[234:291,1], 
                                  bin_data_new[234:291,2])
#Sainsburys
bin_data_new[300:301,1] <- paste(bin_data_new[300:301,1], bin_data_new[300:301,2])
#Tescos
bin_data_new[322:324,1] <- paste(bin_data_new[322:324,1], bin_data_new[322:324,2])
#Wagon and Horses
bin_data_new[375:376,1] <- paste(bin_data_new[375:376,1], bin_data_new[375:376,2])

# FOR BIN_DATA_2019
# Layby
bin_data_2019[163:165,1] <- paste(bin_data_2019[163:165,1], bin_data_2019[163:165,2])
# Morrisons
bin_data_2019[196:202,1] <- paste(bin_data_2019[196:202,1], bin_data_2019[196:202,2])
# Roadside
bin_data_2019[232:278,1] <- paste(bin_data_2019[232:278,1], bin_data_2019[232:278,2])
#Sainsburys
bin_data_2019[287:288,1] <- paste(bin_data_2019[287:288,1], bin_data_2019[287:288,2])
#Tescos
bin_data_2019[308:310,1] <- paste(bin_data_2019[308:310,1], bin_data_2019[308:310,2])
#Wagon and Horses
bin_data_2019[373:374,1] <- paste(bin_data_2019[373:374,1], bin_data_2019[373:374,2])

# Delete duplicates (ONLY AFTER HAVING MADE ALL ALTERATIONS)
bin_data_new <- bin_data_new[-374,]
bin_data <- bin_data[-c(158, 270),]
bin_data_2019 <- bin_data_2019[-c(48,366,368),]

#Select glass bins only
bin_data <- bin_data %>%
  filter(`recyclables-mixed_glass` == "Y")

bin_data_new <- bin_data_new %>%
  filter(`recyclables-mixed_glass` == "Y")

bin_data_2019 <- bin_data_2019 %>%
  filter(`recyclables-mixed_glass` == "Y")


# Do the merge !!
bin_data <- bin_data %>% 
  full_join(bin_data_2019, by = "site_name")

bin_data[371:387,2:22] <- bin_data[371:387, 59:79]

bin_data <- bin_data %>% 
  full_join(bin_data_new, by = "site_name") %>%
  filter(!is.na(site_name))

bin_data <- bin_data %>%
  select(-c(59:79), -c(92:112), -c(sticker.x, non_lcc_banks.x, tally_of_sites.x, cleaning.x))

names(bin_data) = gsub(pattern = ".x", replacement = "", x = names(bin_data))

# Replace ward names by pattern
bin_data$ward <- bin_data$ward %>%
  str_replace_all(c('Burley' = "Headingley and Hyde Park",
                    'City and Hunslet' = "Hunslet and Riverside",
                    "Weatherby" = "Wetherby",
                    "Halton" = "Burmantofts and Richmond Hill",
                    "Harwood" = "Harewood",
                    "Meanwood" = "Moortown",
                    "Hyde Park and Woodhouse" = "Headingley and Hyde Park",
                    "Woodhouse" = "Little London and Woodhouse",
                    "City Centre" = "Little London and Woodhouse",
                    "&" = "and"))

# Change names of location_type to have less categories:
bin_data$location_type <- bin_data$location_type %>% 
  str_replace_all(c("Pub|Restaurant" = "Pub/Restaurant",
                    "Road Side" = "Roadside",
                    "Leisure Centre|Sports Club" = "Leisure Centre/Sports Club",
                    "Childrens Centre|Educational Facility" = "Childrens Centre/Educational Facility"))


# Reshape and tidy using dates
bin_data_g <- bin_data %>%
  gather(key = "Date", value = "Glass_tonnages_kg", 19:78) %>%
  drop_na(Glass_tonnages_kg)

bin_data_g <- as_tibble(bin_data_g)

bin_data_g$Date <- str_replace_all(bin_data_g$Date, "_glass_tonnages_kg", "")
bin_data_g$Date <- paste(bin_data_g$Date, "_01", sep="")
bin_data_g$Date <- myd(bin_data_g$Date)
bin_data_g$Glass_tonnages_kg <- as.numeric(bin_data_g$Glass_tonnages_kg, na.rm = TRUE)

# Filtering out all rows with Glass_tonnages_kg == NA
bin_data_g <- bin_data_g %>%
  filter(!is.na(Glass_tonnages_kg))
