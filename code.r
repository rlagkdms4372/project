# Bringing out the library
library(ggplot2)
library(maps)
library(ggmap)

# recall all the dataset
global <- read.csv("globalpowerplant.csv")
product <- read.csv("production.source.csv")
consume <- read.csv("consumer.source.csv")
nuclear <- read.csv("nuclear.csv")
co2 <- read.csv("co2.data.csv")

########## Plot 1 ##########
# Plot1 for world energy production trend

# Bring out the data index of world
world <- which(product$Entity == "World")

# Bring out the data index for each energy resource
coal_world <- product$Coal....electricity.[world]
gas_world <- product$Gas....electricity.[world]
hydro_world <- product$Hydro....electricity.[world]
solar_world <- product$Solar....electricity.[world]
wind_world <- product$Wind....electricity.[world]
oil_world <- product$Oil....electricity.[world]
other_world <- product$Other.renewables....electricity.[world]
nuclear_world <- product$Nuclear....electricity.[world]

# Making the data frame for coal from 1985 to 2020
value <- coal_world
coal_world_prod <- data.frame(value)
coal_world_prod$year <- 1985:2020
coal_world_prod$name[1:36] <- "coal"

# Making the data frame for nuclear from 1985 to 2020
value <- nuclear_world
nuclear_world_prod <- data.frame(value)
nuclear_world_prod$year <- 1985:2020
nuclear_world_prod$name[1:36] <- "nuclear"

# Making the data frame for gas from 1985 to 2020
value <- gas_world
gas_world_prod <- data.frame(value)
gas_world_prod$year <- 1985:2020
gas_world_prod$name[1:36] <- "gas"

# Making the data frame for hydro from 1985 to 2020
value <- hydro_world
hydro_world_prod <- data.frame(value)
hydro_world_prod$year <- 1985:2020
hydro_world_prod$name[1:36] <- "hydro"

# Making the data frame for solar from 1985 to 2020
value <- solar_world
solar_world_prod <- data.frame(value)
solar_world_prod$year <- 1985:2020
solar_world_prod$name[1:36] <- "solar"

# Making the data frame for wind from 1985 to 2020
value <- wind_world
wind_world_prod <- data.frame(value)
wind_world_prod$year <- 1985:2020
wind_world_prod$name[1:36] <- "wind"

# Making the data frame for oil from 1985 to 2020
value <- oil_world
oil_world_prod <- data.frame(value)
oil_world_prod$year <- 1985:2020
oil_world_prod$name[1:36] <- "oil"

# Making the data frame for other resources from 1985 to 2020
value <- other_world
other_world_prod <- data.frame(value)
other_world_prod$year <- 1985:2020
other_world_prod$name[1:36] <- "other"

# row-bind all the dataframe made above
new_production <- rbind(coal_world_prod, gas_world_prod, hydro_world_prod,
                        oil_world_prod, other_world_prod, solar_world_prod,
                        wind_world_prod, nuclear_world_prod)

# Plot (1)
# The production percentage of each energy source of world since 1985
ggplot(data = new_production) +
  geom_smooth(mapping = aes(x = year, y = value, color = name),
              method = 'loess', formula = 'y ~ x') +
  labs( x = "Year", y= "Percentage(%)", color = "Energy",
        title = "The Production Percentage of Each Energy Source 
        All Around the World Since 1985")

########## Plot 2 ##########
# Plot 2 for the production trend of each energy source for top five countries
# where produce the most energy all around the world since 1985

# Bring out the data index of each contries (top five countries)
prod_5 <- which(product$Code == "USA" & product$Year >= 1985 & 
                  product$Year <= 2020 | 
                  product$Code == "CHN" & product$Year >= 1985 & 
                  product$Year <= 2020 | 
                  product$Code == "IND" & product$Year >= 1985 & 
                  product$Year <= 2020 | 
                  product$Code == "RUS" & product$Year >= 1985 & 
                  product$Year <= 2020 | 
                  product$Code == "JPN" & product$Year >= 1985 & 
                  product$Year <= 2020)

# Make the data frame for country name
df_prod_5 <- data.frame(product$Code[prod_5])

# Bring out the data for each energy resource from index we figured above
coal_prod_5 <- product$Coal....electricity.[prod_5]
gas_prod_5 <- product$Gas....electricity.[prod_5]
hydro_prod_5 <- product$Hydro....electricity.[prod_5]
solar_prod_5 <- product$Solar....electricity.[prod_5]
wind_prod_5 <- product$Wind....electricity.[prod_5]
oil_prod_5 <- product$Oil....electricity.[prod_5]
nuclear_prod_5 <- product$Nuclear....electricity.[prod_5]
other_prod_5 <- product$Other.renewables....electricity.[prod_5]


# Making the data frame for coal
coal_prod_5 <- data.frame(coal_prod_5)
coal_prod_5$year <- rep(1985:2020)
coal_prod_5$name[1:180] <- "coal"
names(coal_prod_5) <- c("value", "year", "name")

# Making the data frame for gas
gas_prod_5 <- data.frame(gas_prod_5)
gas_prod_5$year <- rep(1985:2020)
gas_prod_5$name[1:180] <- "gas"
names(gas_prod_5) <- c("value", "year", "name")

# Making the data frame for hydro
hydro_prod_5 <- data.frame(hydro_prod_5)
hydro_prod_5$year <- rep(1985:2020)
hydro_prod_5$name[1:180] <- "hydro"
names(hydro_prod_5) <- c("value", "year", "name")

# Making the data frame for solar
solar_prod_5 <- data.frame(solar_prod_5)
solar_prod_5$year <- rep(1985:2020)
solar_prod_5$name[1:180] <- "solar"
names(solar_prod_5) <- c("value", "year", "name")

# Making the data frame for nuclear
nuclr_prod_5 <- data.frame(nuclear_prod_5)
nuclr_prod_5$year <- rep(1985:2020)
nuclr_prod_5$name[1:180] <- "nuclear"
names(nuclr_prod_5) <- c("value", "year", "name")

# Making the data frame for wind
wind_prod_5 <- data.frame(wind_prod_5)
wind_prod_5$year <- rep(1985:2020)
wind_prod_5$name[1:180] <- "wind"
names(wind_prod_5) <- c("value", "year", "name")

# Making the data frame for oil
oil_prod_5 <- data.frame(oil_prod_5)
oil_prod_5$year <- rep(1985:2020)
oil_prod_5$name[1:180] <- "oil"
names(oil_prod_5) <- c("value", "year", "name")

# Making the data frame for other resources
other_prod_5 <- data.frame(other_prod_5)
other_prod_5$year <- rep(1985:2020)
other_prod_5$name[1:180] <- "other"
names(other_prod_5) <- c("value", "year", "name")

# bind all the dataframe made above and make new dataframe we'll use
new_df_prod_5 <- rbind(coal_prod_5, gas_prod_5, hydro_prod_5,
                       oil_prod_5, other_prod_5, solar_prod_5,
                       wind_prod_5, nuclr_prod_5)
new_prod_5 <- cbind(new_df_prod_5, df_prod_5)

# Rename the column's name
names(new_prod_5) <- c("value", "year", "name", "country")

# Plot (2)
# The production percentage of each energy source for the top five countries 
# the most production energy all aroudn the world
ggplot(data = new_prod_5) +
  geom_smooth(mapping = aes(x = year, y = value, color = country),
              method = 'lm', formula = 'y ~ x') +
  facet_wrap(~name, nrow = 2) +
  labs( x = "Year", y= "Percentage(%)", color = "Country",
        title = "The Top Five Energy Production Countries' Energy Trend
        since 1985")


########## Plot 3 ##########
# By using the data frame we figured out above, we compare the differences
# between production and consumption of energy

# bring out the index which is suitable for conditions
# this index for consumer energy rate for top 5 countries 
cons_5 <- which(consume$Code == "USA" &consume$Year == 2019| 
                  consume$Code == "CHN"  &consume$Year == 2019|
                  consume$Code == "IND" &consume$Year == 2019| 
                  consume$Code == "RUS" &consume$Year == 2019|
                  consume$Code == "JPN" &consume$Year == 2019)

# this index for production energy rate for top 5 countries
prod_5 <- which(product$Code == "USA"&product$Year == 2019 | 
                  product$Code == "CHN"&product$Year == 2019|
                  product$Code == "IND"&product$Year == 2019 | 
                  product$Code == "RUS"&product$Year == 2019|
                  product$Code == "JPN"&product$Year == 2019)

# bring out the energy rate by using the index from dataset
prod_value <- product$Code[prod_5]
cons_value <- consume$Code[cons_5]

# make the new data frame with the data we figured out above
df_prod_5 <- data.frame(prod_value)
df_cons_5 <- data.frame(cons_value)

# Bring out the data index for each energy resource for production
coal_prod_5 <- product$Coal....electricity.[prod_5]
gas_prod_5 <- product$Gas....electricity[prod_5]
hydro_prod_5 <- product$Hydro....electricity.[prod_5]
solar_prod_5 <- product$Solar....electricity.[prod_5]
wind_prod_5 <- product$Wind....electricity.[prod_5]
oil_prod_5 <- product$Oil....electricity.[prod_5]
nuclear_prod_5 <- product$Nuclear....electricity.[prod_5]
other_prod_5 <- product$Other.renewables....electricity.[prod_5]

# Bring out the data index for each energy resource for consumption
coal_cons_5 <- consume$Coal....sub.energy.[cons_5]
gas_cons_5 <- consume$Gas....sub.energy.[cons_5]
hydro_cons_5 <- consume$Hydro....sub.energy.[cons_5]
solar_cons_5 <- consume$Solar....sub.energy.[cons_5]
wind_cons_5 <- consume$Wind....sub.energy.[cons_5]
oil_cons_5 <- consume$Oil....sub.energy.[cons_5]
nuclear_cons_5 <- consume$Nuclear....sub.energy.[cons_5]
other_cons_5 <- consume$Other.renewables....sub.energy.[cons_5]

# Production
# Making the data frame for coal
value <- coal_prod_5
coal_prod_5 <- data.frame(value)
coal_prod_5$name <- "coal"
# Combining the new data frame with the the data frame for country name
coal_prod_5 <- cbind(coal_prod_5, df_prod_5)

# Making the data frame for gas
value <- gas_prod_5
gas_prod_5 <- data.frame(value)
gas_prod_5$name <- "gas"
gas_prod_5 <- cbind(gas_prod_5, df_prod_5)

# Making the data frame for hydro
value <- hydro_prod_5
hydro_prod_5 <- data.frame(value)
hydro_prod_5$name <- "hydro"
hydro_prod_5 <- cbind(hydro_prod_5, df_prod_5)

# Making the data frame for solar
value <- solar_prod_5
solar_prod_5 <- data.frame(value)
solar_prod_5$name <- "solar"
solar_prod_5 <- cbind(solar_prod_5, df_prod_5)

# Making the data frame for solar
value <- nuclear_prod_5
nuclear_prod_5 <- data.frame(value)
nuclear_prod_5$name <- "nuclear"
nuclear_prod_5 <- cbind(nuclear_prod_5, df_prod_5)

# Making the data frame for wind
value <- wind_prod_5
wind_prod_5 <- data.frame(value)
wind_prod_5$name <- "wind"
wind_prod_5 <- cbind(wind_prod_5, df_prod_5)

# Making the data frame for oil
value <- oil_prod_5
oil_prod_5 <- data.frame(value)
oil_prod_5$name <- "oil"
oil_prod_5 <- cbind(oil_prod_5, df_prod_5)

# Making the data frame for other resources
value <- other_prod_5
other_prod_5 <- data.frame(value)
other_prod_5$name <- "other"
other_prod_5 <- cbind(other_prod_5, df_prod_5)

# Consumer
# Making the data frame for coal
value <- coal_cons_5
coal_cons_5 <- data.frame(value)
coal_cons_5$name <- "coal"
coal_cons_5 <- cbind(coal_cons_5, df_cons_5)

# Making the data frame for gas
value <- gas_cons_5
gas_cons_5 <- data.frame(value)
gas_cons_5$name <- "gas"
gas_cons_5 <- cbind(gas_cons_5, df_cons_5)

# Making the data frame for hydro
value <- hydro_cons_5
hydro_cons_5 <- data.frame(value)
hydro_cons_5$name <- "hydro"
hydro_cons_5 <- cbind(hydro_cons_5, df_cons_5)

# Making the data frame for solar
value <- solar_cons_5
solar_cons_5 <- data.frame(value)
solar_cons_5$name <- "solar"
solar_cons_5 <- cbind(solar_cons_5, df_cons_5)

# Making the data frame for solar
value <- nuclear_cons_5
nuclear_cons_5 <- data.frame(value)
nuclear_cons_5$name <- "nuclear"
nuclear_cons_5 <- cbind(nuclear_cons_5, df_cons_5)

# Making the data frame for wind
value <- wind_cons_5
wind_cons_5 <- data.frame(value)
wind_cons_5$name <- "wind"
wind_cons_5 <- cbind(wind_cons_5, df_cons_5)

# Making the data frame for oil
value <- oil_cons_5
oil_cons_5 <- data.frame(value)
oil_cons_5$name <- "oil"
oil_cons_5 <- cbind(oil_cons_5, df_cons_5)

# Making the data frame for other resources
value <- other_cons_5
other_cons_5 <- data.frame(value)
other_cons_5$name <- "other"
other_cons_5 <- cbind(other_cons_5, df_cons_5)

# bind all the dataframe made above for production
new_prod_5 <- rbind(coal_prod_5, gas_prod_5, hydro_prod_5, oil_prod_5,
                    other_prod_5, solar_prod_5, wind_prod_5, nuclear_prod_5)

# make the new column to indicate "production"
new_prod_5$names <- rep("production")

# bind all the dataframe made above for consumption
new_cons_5 <- rbind(coal_cons_5, gas_cons_5, hydro_cons_5, oil_cons_5,
                    other_cons_5, solar_cons_5, wind_cons_5, nuclear_cons_5)

# make the new column to indicate "consumption"
new_cons_5$names <- rep("consumer")

# Make the point plot for production
ggplot(data = new_prod_5) +
  geom_point (mapping = aes(x = prod_value, y = value, color = name)) +
  labs(x = "Country", y = "Percentage(%)",
       title = "Energy Production for Top 5 Countries", color = "Energy")

# Make the point plot for consumption
ggplot(data = new_cons_5) +
  geom_point(mapping = aes(x = cons_value, y = value, color = name))+
  labs(x = "Country", y = "Percentage(%)",
       title = "Energy Consumption for Top 5 Countries", color = "Energy")

########## Plot 4 ##########
# Bring out the index where fossil fuel production rate is less than 50 in 2018
below_50 <- which(nuclear$Fossil.fuels....sub.energy. <= 50 &
                    nuclear$Year == 2018)

# Find out the countries name and make the data frame
country <- nuclear$Entity[below_50]
country <- data.frame(country)

# Find out the fossil fuel and make the data frame
fossil_50 <- nuclear$Fossil.fuels....sub.energy.[below_50]
fossil_50 <- data.frame(fossil_50)
# Name it that it is Fossil Fuel
fossil_50$name <- rep("Fossil Fuel")
# Change the column name
names(fossil_50) <- c("value", "name")
# Make the new column for country name
fossil_50 <- cbind(fossil_50, country)

# Find out the renewable and make the data frame
renew_50 <- nuclear$Renewables....sub.energy.[below_50]
renew_50 <- data.frame(renew_50)
renew_50$name <- rep("renewable")
names(renew_50) <- c("value", "name")
# Make the new column for country name
renew_50 <- cbind(renew_50, country)

# Find out the nuclear and make the data frame
nuclear_50 <- nuclear$Nuclear....sub.energy.[below_50]
nuclear_50 <- data.frame(nuclear_50)
nuclear_50$name <- rep("Nuclear")
names(nuclear_50) <- c("value", "name")
# Make the new column for country name
nuclear_50 <- cbind(nuclear_50, country)

# Make the data frame (final version)
new_new <- rbind(fossil_50, renew_50, nuclear_50)

# Make the bar plot which indicates energy production ratio
ggplot()+
  geom_bar(data = new_new, mapping = aes(x = country, y = value, 
                                         fill = as.factor(name)), 
           position = "fill" , stat="identity") +
  labs( x = "Country", y = "Ratio (1=100%)", fill = "Energy", 
        title = "The Ratio of Energy for 4 Countries")

########## Plot 5 ##########
# This plot is made in order to look over the trend of the CO2 amount by year
# We're going to compare the amount of 
# CO2 eco-friendly countries to top five countries

# bring out the index of eco-friendly countries between 1985 and 2020
environ_ctr <- which(co2$year >= 1985 & co2$country == "Iceland" |
                       co2$year >= 1985 & co2$country == "Norway" |
                       co2$year >= 1985 & co2$country == "Sweden" | 
                       co2$year >= 1985 & co2$country == "Switzerland")

# bring out the index of top five energy production countries 
# between 1985 and 2020
top_5_ctr <- which( co2$year >= 1985 & co2$country == "United States" |
                      co2$year >= 1985 & co2$country == "China" |
                      co2$year >= 1985 & co2$country == "India" |
                      co2$year >= 1985 & co2$country == "Japan" |
                      co2$year >= 1985 & co2$country == "Russia")

# Bring out the amount of CO2 by using the eco-friendly index
tb_environ <- co2$co2[environ_ctr]

# Bring out country by using the eco-friendly index
tb_environ_cty <- co2$country[environ_ctr]

# Bring out year by using the eco-friendly index
tb_environ_yr <- co2$year[environ_ctr]

# column bind with the amount of CO2, country, and year
new_tb_environ <- cbind(tb_environ, tb_environ_cty, tb_environ_yr)

# make the data frame
df_environ <- data.frame(new_tb_environ)

# rename the column name
names(df_environ) <- c("co2", "country", "year")

# Bring out the amount of CO2 by using the top 5 production index
tb_top_5 <- co2$co2[top_5_ctr]

# Bring out country by using the top 5 production index
tb_top_cty <- co2$country[top_5_ctr]

# Bring out year by using the top 5 production index
tb_top_yr <- co2$year[top_5_ctr]

# column bind with the amount of CO2, country, and year
new_tb_top <- cbind(tb_top_5, tb_top_cty, tb_top_yr)

# make the data frame
df_top <- data.frame(new_tb_top)

# rename the column name
names(df_top) <- c("co2", "country", "year")

# Make the line plot included point plot for eco-friendly countries
ggplot(data = df_environ) +
  geom_line(mapping = aes(x = as.numeric(year), y = as.numeric(co2),
                          color = country)) +
  geom_point(mapping = aes(x = as.numeric(year), y = as.numeric(co2),
                           color = country), cex = 0.9) +
  labs(x = "Year", y = "CO2 (kt)",
       title = "The Trend of the CO2 Amount by Year")

# Make the line plot included point plot for top five countries
ggplot(data = df_top) +
  geom_line(mapping = aes(x = as.numeric(year), y = as.numeric(co2),
                          color = country)) +
  geom_point(mapping = aes(x = as.numeric(year), y = as.numeric(co2),
                           color = country), cex = 0.9) +
  labs(x = "Year", y = "CO2 (kt)",
       title = "The Trend of the CO2 Amount by Year")

########## Plot 6 (1) ##########
# Plot 6 (1) for the number of power plants all around the world

# by using table we can figure out the number of each plants
plant_number <- table(global$primary_fuel)

# In order to make the plot decreasing sort
plant_number <- sort(plant_number, decreasing = FALSE)

# Making the data frame for it
plant_number <- data.frame(plant_number)

# Making the plots by using ggplot, and it indicates the figures
ggplot(plant_number) +
  geom_bar(mapping = aes(x = Freq, y = Var1, fill = Var1), stat = "identity") +
  labs(x = "Number of Plants", y = "Energy Source", fill = "Energy Source",
       title = "The Number of Plants All Around the World") +
  geom_text(aes(x = Freq, y = Var1, label = Freq))

########## Plot 6 (2) ##########
# Plot 6 (2) for the totoal energy capacity which can generate 
# from each energy plant all around the world

# figure out the sum capacity for each energy plant
sum_solar <- sum(global$capacity_mw[which(global$primary_fuel == "Solar")])
sum_hydro <- sum(global$capacity_mw[which(global$primary_fuel == "Hydro")])
sum_wind <- sum(global$capacity_mw[which(global$primary_fuel == "Wind")])
sum_coal <- sum(global$capacity_mw[which(global$primary_fuel == "Coal")])
sum_oil <- sum(global$capacity_mw[which(global$primary_fuel == "Oil")])
sum_gas <- sum(global$capacity_mw[which(global$primary_fuel == "Gas")])
sum_biomass <- sum(global$capacity_mw[which(global$primary_fuel == "Biomass")])
sum_waste <- sum(global$capacity_mw[which(global$primary_fuel == "Waste")])
sum_nuclear <- sum(global$capacity_mw[which(global$primary_fuel == "Nuclear")])
sum_geother <- sum(global$capacity_mw[which(global$primary_fuel == 
                                              "Geothermal")])
sum_storage <- sum(global$capacity_mw[which(global$primary_fuel == "Storage")])
sum_other <- sum(global$capacity_mw[which(global$primary_fuel == "Other")])
sum_cogen <- sum(global$capacity_mw[which(global$primary_fuel == 
                                            "Cogeneration")])
sum_petcoke <- sum(global$capacity_mw[which(global$primary_fuel == "Petcoke")])
sum_wnt <- sum(global$capacity_mw[which(global$primary_fuel == 
                                          "Wave and Tidal")])


# make the dataframe for the value of capacity
df_sum <- data.frame(c(sum_solar, sum_hydro, sum_wind, sum_coal, sum_oil,
                       sum_gas, sum_biomass, sum_waste, sum_nuclear,
                       sum_geother, sum_storage, sum_other, sum_cogen,
                       sum_petcoke, sum_wnt))

# round up to the second decimal place.
df_sum <- round(df_all, 2)

# make the dataframe for 'energy source name' by the order of the sum dataframe 
df_name <- data.frame(c("Solar", "Hydro", "Wind", "Coal", "Oil", "Gas",
                        "Biomass", "Waste", "Nuclear", "Geothermal",
                        "Storage", "Other", "Cogeneration", "Petcoke",
                        "Wave and Tidal"))

# make the new dataframe by using cbind 
new_sum <- cbind(df_sum, df_name)

# change the column name
names(new_sum) <- c("value", "energy")

# make the barplot for total capacity of each plant all around the world
ggplot(new_sum) +
  geom_bar(mapping = aes(x = value, y = reorder(energy, value), fill = energy),
           stat = "identity") +
  labs(x = "Total Capacity(mw)", y = "Energy Source", fill = "Energy Source",
       title = "The Total Capacity of Each Plant All Around the World") +
  geom_text(aes(x = value, y = energy, label = value), cex = 3)


########## Plot 7 ##########
# Which energy plant produces the most energy?

# barplot for energy plant produces the energy
ggplot(global) +
  geom_boxplot(mapping = aes(x = primary_fuel, y = capacity_mw,
                             color = primary_fuel)) +
  labs(x = "Energy Source", y = "Capacity (mw)",
       title = "The Energy Plant Capacity of World") +  
  scale_y_continuous(limits = c(1, 2000)) +
  theme(axis.text.x = element_text(size= 7))

########## Plot 8 ##########
# This plot is in order to look the spread of energy plants in the States

# Bringing out USA
us_map <- map_data("state")

# Making the dataframe for USA
production_usa <- global[which(global$country == "USA"), ]

# Making the dataframe for longitude and latitude (map plot)
production_usa <- production_usa[which(production_usa$longitude <= -65 &
                                         production_usa$longitude >= -130), ]
production_usa <-  production_usa[which(production_usa$latitude <=50 &
                                          production_usa$latitude >= 23), ]

# Making the dataframe for renewable energy (scatter plot)
production_usa <- production_usa[which(production_usa$primary_fuel == "Biomass"|
                                  production_usa$primary_fuel == "Cogeneration"|
                                  production_usa$primary_fuel == "Geothermal"|
                                  production_usa$primary_fuel == "Hydro"|
                                  production_usa$primary_fuel == "Wind"|
                                  production_usa$primary_fuel == "Solar"), ]


# Making the map plot for the spread of renewable energy power plant.
ggplot(us_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +  
  coord_quickmap() +
  geom_point(data = production_usa,
             mapping = aes(x = longitude, y = latitude, color = primary_fuel),
             cex = 0.9) +
  labs(x = "Longitude", y = "Latitude", color = "Renewable energy",
       title = "The Spread of Renewable Energy Power Plant")

# Making the map plot for the spread of renewable energy power plant
# divided by the energy source
ggplot(us_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) +  
  coord_quickmap() +
  geom_point(data = production_usa,
             mapping = aes(x = longitude, y = latitude, color = primary_fuel),
             cex = 0.7) +
  facet_wrap(~primary_fuel, nrow = 3) + 
  labs(x = "Longitude", y = "Latitude", color = "Renewable energy",
       title = "The Spread of Renewable Energy Power Plant")
