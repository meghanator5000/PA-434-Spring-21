# I call in the tidyverse library
library(tidyverse)

# I bring in each of the csv files that I saved from blackboard as tibbles, and I view them as I go so I can see what I'm working with
imm = read_csv("ImmigrationData.csv")
imm

region = read_csv("RegionData.csv")
region

# I check the dimensions to take a deeper look at the datasets
# When I merge, I want to keep all observations from the immigration dataset, which has 1392 rows. Therefore, I expect my merged dataset to have 1392 rows.
dim(imm)
dim(region)

# I decide to rename the columns in region so that they match the immigration dataset when I merge them. I will rename the immigration dataset at the end
region_2 <- 
  rename(region, 
         CountryCode = countrycode, 
         Region = region)

# I do a left join with immigration first since I want to keep all of these observations. I join by CountryCode
imm_by_region <-
  left_join(imm, region_2, 
            by = "CountryCode")
dim(imm_by_region)
# There are 1392 rows, just like I expected
# There are 16 columns which makes sense since the Region column was added to the 15 in immigration

# Find all observations from countries in Africa collected in 1990
africa_1990 <- imm_by_region %>% 
  filter(year == 1990 & Region == "Africa") 
dim(africa_1990) 
# There are 57 observations from countries in Africa collected in 1990

# Find the countries that have a number of female migrants between 1 and 2 million excluded
female_migrants <- imm_by_region %>% 
  filter(FemaleMigrants >= 1000000 & FemaleMigrants < 2000000 & year == 2015) 
female_migrants 
dim(female_migrants) 
# the 15 countries with between 1 and 2 million female migrants in 2015 are: South Africa, Kazakhstan, China, Hong Kong Special Administrative Region, Japan, Singapore, Thailand, Iran (Islamic Republic of), Pakistan, Israel, Jordan, Lebanon, Turkey, Netherlands, Switzerland, Argentina  

# Filter to only countries located in Africa or Oceania
africa_oceania <- imm_by_region %>% 
  filter(Region == "Africa" | Region == "Oceania") %>% 
  distinct(Country) 
africa_oceania
dim(africa_oceania) 
# There are 67 countries.

# Find the country that has the highest number of migrants among “developed” countries in 2010 
highest_mig <- imm_by_region %>% 
  filter(Region == "Developed countries", year == 2010) %>% 
  summarise(Migrants, Country)  %>%  
  arrange(desc(Migrants)) 
highest_mig # The United States

# Reorganize the dataset 
imm_by_region_2 <- imm_by_region %>% 
  relocate(Migrants:Refugees, .after = year) 
imm_by_region_2

# Only look at select countries
several_countries <- imm_by_region_2 %>% 
  filter(Country %in% c("Italy", "Germany", "Spain", "France", "Portugal", "Greece") 
         & Year == 2015) 
several_countries 

# Rename columns for clarity and consistency
imm_by_region_3 <- 
  rename(imm_by_region_2, 
         Year = year, 
         Africa = tot_africa,
         Asia = tot_asia,
         Oceania = tot_oceania,
         Developed = tot_developed,
         Latam = tot_latam)
imm_by_region_3 

