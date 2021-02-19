# I call in the tidyverse library
library(tidyverse)

# I bring in each of the csv files that I saved from blackboard as tibbles, and I view them as I go so I can see what I'm working with
imm = read_csv("ImmigrationData.csv")
imm

region = read_csv("RegionData.csv")
region

# The dataset includes Country, CountryCode, year, population, male population, female population, 
# migrants, male migrants, female migrants, counts of people from each of the continents, refugees, 
#and the region of the country

# I decide to rename the columns in region so that they match the immigration dataset when I merge them
region_2 <- 
  rename(region, 
         CountryCode = countrycode, 
         Region = region)

# I do a left join with immigration first since I want to keep all of these observations. I join by CountryCode
imm_by_region <-
  left_join(imm, region_2, 
            by = "CountryCode")

# Question 1

imm_by_region_2 <- imm_by_region %>%
  filter(year == 2015) %>% # ensure that I am only looking at 2015
  drop_na(Migrants) %>% # get rid of NA values so I can perform analysis
  group_by(Region) %>% # group by Region so that I can look at total migrants for each region
  summarize(Total_Migrants = sum(Migrants)) # create a column that sums migrants

# Answer:
# Region              Total_Migrants
# Africa                    20648953
# Asia                      72781741
# Developed countries      140393231
# Latam                      9024966
# Oceania                     229158
# Territories/Others          622187

# Developed countries have the most amount of migrants, which I am not surprised by
# Oceania has the least amount of migrants

# Question 2

imm_by_region_3 <- imm_by_region %>%
  mutate(prop_imm = Migrants / Pop) %>% # create new column that finds the number of migrants by population
  filter(year == 2015, prop_imm >= .5) %>% # I only want to look at the year 2015 AND when my new column value is greater than .5 or 50%
  summarise(number_countries = n()) # include a new column with number of countries
# run the code and see that there are 16 countries with a share of immigrants that is greater than or equal to 50% of the population in 2015

# Question 3

imm_by_region_4 <- imm_by_region %>%
  group_by(Region) %>% # will look at the above answer of 16 broken down by region
  mutate(prop_imm = Migrants / Pop) %>% # same as above 
  filter(year == 2015, prop_imm >= .5) %>% # same as above
  summarise(number_countries = n()) # same as above
# below is the breakdown of the 16 countries from above by region
# Region              number_countries
# Asia                               5
# Developed countries                4
# Latam                              1
# Territories/Others                 6

# Question 4

imm_by_region_5 <- imm_by_region %>%
  mutate(prop_imm = Migrants / Pop) %>% # same as above
  filter(prop_imm >= .5) %>% # same as above, but we are looking at all years now so do not filter for year
  group_by(Country) %>% # looking at the proportion by country
  summarise(avg_prop_imm = mean(prop_imm)) %>% # create column based prop_imm that averages the proportion by country over all the years
  arrange(desc(avg_prop_imm)) # arrange highest to lowest so that we can see the country with the highest average proportion of immigrants
dim(imm_by_region_5) # check the dimensions to see the number of countries that meet the criteria
# 19 countries have a share of immigrants greater than or equal to 50% of their population as an average over the years
# The country with the highest average across all years is Bonaire, Sint Eustatius and Saba at 2.27. 


# Question 5

# I decide to create datasets to show refugees by region for 1990, then below for 2015
refugees_1990 <- imm_by_region %>% # create new dataset from imm_by_region
  filter(year == 1990) %>% # filter so only 1990
  group_by(Region) %>% # group by region because that is the way I am looking at the data
  summarise(regional_refugees = sum(Refugees, na.rm = T)) # sum the refugees in the group by and adjust for the NA values

# Create another dataset by 2015  
refugees_2015 <- imm_by_region %>%  # create new dataset from imm_by_region
  filter(year == 2015) %>% # filter so only 2015
  group_by(Region) %>% # group by region because that is the way I am looking at the data
  summarise(regional_refugees = sum(Refugees, na.rm = T)) # sum the refugees in the group by and adjust for the NA values

# Join the datasets by region 
change_refugees <-
  full_join(refugees_1990, refugees_2015, # use a full join for my two new datasets 
            by = "Region") # join by region

change_refugees_1 <- change_refugees %>% # do analysis based on the create_refugees dataset
  mutate(Change = regional_refugees.y - regional_refugees.x) %>% # create a new column that shows the change from 1990 to 2015
  rename(
    refugees_1990 = regional_refugees.x, # rename the columns to make them more clear
    refugees_2015 = regional_refugees.y) %>% # rename the columns to make them more clear
  arrange(desc(Change)) # arrange in order
change_refugees_1 # view the dataset to see the changes

# Asia had the largest increase in the number of refugees and Africa had the largest decrease in number of refugees


# Question 6

# Part 1: Changes in the immigration patterns by gender over time 

imm_by_region_7 <-imm_by_region %>% 
  drop_na() %>% # adjust for NA values
  group_by(year) %>% # will assess the changes by year, so I grouped by them
  summarize(total_perc_male = sum(MaleMigrants) / sum(Migrants), # look at consolidated summed proportions by gender
            total_perc_female = sum(FemaleMigrants) / sum(Migrants)) # look at consolidated summed proportions by gender

# Over time, the percentage of male has consistently been slightly higher than the percentage of females 

# year  male female
#  1990 0.511  0.489
#  1995 0.510  0.490
#  2000 0.511  0.489
#  2005 0.514  0.486
#  2010 0.520  0.480
#  2015 0.521  0.479

# Part 2: Lowest female immigration - country
imm_by_region_8 <- imm_by_region %>%  
  group_by(Country) %>%
  summarize(avg_fem_mig = mean(FemaleMigrants)) %>%
  arrange(avg_fem_mig)
# Tuvalu has the lowest average female immigration from 1990 to 2015 out of the countries

# Part 3: Highest female immigration - region
imm_by_region_9 <- imm_by_region %>%
  group_by(Region) %>%
  summarize(avg_fem_mig = mean(FemaleMigrants, na.rm = T)) %>%
  arrange(desc(avg_fem_mig))
# Developed countries have the highest average female immigration from 1990 to 2015 out of the regions


# Missing Values

# Question 1

colSums(is.na(imm_by_region)) # Use the colSums and the is.na function to sum the number of NA values in each column of the imm_by_region dataset
# There are 366 NA values in the Refugee column
# There are 0 NA values in the Population column
# There are 15 NA values in the Migrants column
# MalePop, Female Pop, Migrants, MaleMigrants, FemaleMigrants, Refugees, and all the tot_ columns have NA values

# Question 2

pop_na <- imm_by_region %>%
  filter(is.na(MalePop) | is.na(FemalePop)) %>% # decide to start off by looking at the 192 missing values for MalePop and FemalePop
  group_by(Country, Region) %>% # look at these by Country and Region 
  summarize(number_na = n()) # count by Country and Region 
# Most missing values are in Territories/Others, then Developed countries, then Latam; There are 32 countries that do not have gendered Pop values, and all of them are missing for all 6 years
# It makes sense that the Territories may not have as much information about gendered population as other Regions, so I would classify these values as MNR, 
# # since there is a clear trend of territories being the region with the most missing values by far 
# We should consider why 27 Countries within Territories/Others did not have available gendered population values for all 6 years in our analysis 
# This likely has to do with the fact that they are territories
# There are four Developed Countries and one Latam country missing values for all 6 years as well

migrant_na <- imm_by_region %>%
  filter(is.na(Migrants) | is.na(MaleMigrants) | is.na(FemaleMigrants)) %>% # next I look at the at the 192 missing values for MalePop and FemalePop
  group_by(Country, year) %>% # look at these by Country and year
  select(Country, Region, year) %>% # only want to look at Country and year
  arrange(Country) # arrange so it is easier to assess by Country
# South Sudan, Montenegro, and Curacao are missing values for 1990-2005 and Sint Maarten (Dutch part) is missing values for 1990-2000
# The same values are missing across the three migrant columns, which makes it seem as though migrant values were just not available for the four countries in the missing years
# These values seem to be MAR, and the fact that there are no missing values after 2005 shows that the values became available after that
# We can likely safely eliminate these values, but may want to consider why they are not available
# There are no missing Oceania values

area_na <- imm_by_region %>%
  filter(is.na(tot_africa) | is.na(tot_asia) | is.na(tot_oceania) | is.na(tot_developed) | is.na(tot_latam)) %>% # next I look at the at the 150 missing values for tot_ columns
  group_by(Country, Region) %>% # look at these by Country and Region
  summarize(number_na = n()) %>% 
  arrange(number_na)
# All of the countries are missing values for all 6 years; There are 25 countries with missing values
# There are 8 Latam countries, 3 Developed countries, 4 Asian countries, and 4 African countries
# There are no missing Oceania values
# These seem to be missing at random, there may be a reason Latam is higher than the others but they can likely be safely removed

refugee_na <- imm_by_region %>%
  filter(is.na(Refugees)) %>% # decide to start off by looking at the 366 missing values for Refugees
  group_by(Country, Region) %>% # look at these by Region
  summarize(number_na = n())
# There are the most missing values for Refugees, which makes sense given the variable is a more difficult one to collect data on
# All of the Countries with missing values are missing values for all 6 years
# MNAR, need to consider the many missing territories/other values when analyzing
# Territories has the most missing values with 156, or 26 Countries missing for 6 years
# 6 missing African countries
# 7 missing Asian countries
# 4 missing developed countries
# 8 missing Latam Countries
# 7 missing Oceania countries

#  Question 3

# Oceania has the least amount of missing values by region and Territories/Other has the most.
# # I was not surprised that Territories/Other had the most missing values, but I was surprised that Oceania had the least, because I expected Developed Countries to have the least.
# # For developed countries, Montenegro and San Marino had missing values for more than one category
# # I was surprised that Canada was missing values for the tot_ columns.

# I think that the volume of missing Territories/Other values could affect the dataset, since there are disproportionately high missing values for these
# # The missing Territories/Other values could be a result of their status as territories, which could be meaningful to consider
# # Overall, I think that most of the NA variables could be removed without a significant impact on the dataset. 
# # The exception to this is the missing values for Territories/Others for refugees and male / female population. These missing values should be considered.

# The most missing values were for refugees. Population had no missing values.  
# # Nor did country, countrycode, year, or region.


