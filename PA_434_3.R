# I bring in the library
library(tidyverse)

# I bring in each of the csv files and view them
migration_flows = read_csv("MigrationFlows.csv")
migration_flows

origin = read_csv("Origin.csv")
origin

population = read_csv("Population.csv")
population

refugees = read_csv("Refugees.csv")
refugees

# First, I am going to make each of the data sets 'tidy'

# I start with tidying up migration_flows, I want to make the set longer and show country, code, year, and values by total, male, and female
# to achieve this, I first make the dataset longer and separate year and gender. This allows for year to be its own column which is the first step of what I wanted
# I am sure to convert year to a numeric value when pivoting
migration_flows_2 <-
  pivot_longer(migration_flows, # the dataset
               c("Tot_1990":"Female_2015"), # The columns I want to pivot
               names_sep = "_", # I want to separate the titles by the underscore
               names_to = c("Gender", "Year"), # The new column names
               values_to = "Migration_Flows_Value", # The column names for the value column that will be created
               names_transform = list(Year = as.numeric)) # make sure that year is numeric

# I don't want gender to be its own column, so I pivot this wider by value, and now each gender category is its own column
migration_flows_3 <-
  pivot_wider(migration_flows_2,
              names_from = c("Gender"), # The column I want to pivot into new columns based on its values
              values_from = c("Migration_Flows_Value")) # where the new columns will get their values from

# I don't like that Country code has a space in the title, so I decide to rename this
migration_flows_4 <-   
  rename(migration_flows_3,
         Country_code = `Country code`) # new name = old name

# I look to see if it looks tidy and it does! The names are not the most helpful, so I will rename this when I create my datasets 
migration_flows_4

# Next I will tidy up origin
# I want the tibble's naming conventions to look more like migration_flows and cleaner
origin_2 <- 
  rename(origin, 
         Country = country_name, # new name to replace old
         Country_code = code,
         Africa = tot_africa, 
         Asia = tot_asia, 
         Oceania = tot_oceania,
         Developed = tot_developed,
         Latam = tot_latam)

# I want to show century, decade, and year as one year column, so I unite these and check that it looks right, and it does
origin_3 <- 
  unite(origin_2, 
        Year, c(century, decade, year), sep="") # I want to unite these three columns, separated by nothing so it looks like a year

# I want Year to be numeric since it is more flexible and should match year type in the other datasets for when we join
origin_4 <- origin_3 %>%
  mutate(Year = as.numeric(Year)) 
origin_4

# Next, I want to tidy population
# I start by matching the names to the other tibbles so that I can follow along with which columns match up
population_2 <- 
  rename(population, 
         Year = year, # new name to replace old
         Country_code = `Country code`,
         Population_Value = population,
         Gender = Poptype) 

# I want each gender type to have its own column in the same way I did it for migration. I pivot wider to achieve this
population_3 <-
  pivot_wider(population_2,
              names_from = c("Gender"), # same methodology as last pivot_wider
              values_from = c("Population_Value"))
population_3

# Lastly, I will tidy up refugees. I want to show country, country code, year, and value 
# I want Year to be numeric since it is more flexible and should match year type in the other datasets for when we join
refugees_2 <-
  pivot_longer(refugees,
               c("REFUGEES_1990":"REFUGEES_2015"), # same as last pivot_longer
               names_to = c("Year"), 
               values_to = "Refugee_Value",
               names_prefix = "REFUGEES_", # I want to get rid of this prefix and only include the year from column names
               names_transform = list(Year = as.numeric))

# I make sure that the names on this tibble match my naming conventions of the others
refugees_3 <- 
  rename(refugees_2, 
         Country_code = `Country code`)
refugees_3

# Below is the first objective: A tidy dataset containing all common observations (e.g., countries) across all four datasets
# I use left join for the first two datasets, joining by "Country", "Country_code", and "Year". I can only join two datasets at a time, so I perform this three times. My last join is an inner join, so that I only keep the common observations, per the requirements

common_observations <- 
  left_join(migration_flows_4, origin_4, # use a left join so that I get all the values I need
            by = c("Country", "Country_code", "Year")) # identifies the columns that I am joining by

common_observations_2 <-
  left_join(common_observations, population_3, 
            by = c("Country", "Country_code", "Year"))

common_observations_3 <-
  inner_join(common_observations_2, refugees_3, #use an inner join here so I only have common observations
             by = c("Country", "Country_code", "Year"))

# Now that there are several data measures in my set, I want to make sure my column names are clear about what I am showing, so I rename them to achieve this
common_observations_4 <- 
  rename(common_observations_3, 
         Total_Migration = Tot,
         Male_Migration = Male,
         Female_Migration = Female,
         Total_Population = Pop,
         Male_Population = MalePop,
         Female_Population = FemalePop) 

# I check the dimensions to compare to my expectations from earlier (expected is 1026)
# there are 1026 rows, just like I expected
dim(common_observations_4)

# Below is the second objective: A tidy dataset contaning as much information as possible across all four datasets. 

# I could only join two tibbles at a time with left_join, so I performed left_join several times until all four were joined
all_columns <-
  left_join(migration_flows_4, origin_4)

all_columns_2 <-
  left_join(all_columns, population_3)

all_columns_3 <-
  left_join(all_columns_2, refugees_3) # keep a left join for this instead of inner like earlier because I do not only want common values

# Similar to above, I need to rename the columns to be as clear as possible
all_columns_4 <- 
  rename(all_columns_3, 
         Total_Migration = Tot,
         Male_Migration = Male,
         Female_Migration = Female,
         Total_Population = Pop,
         Male_Population = MalePop,
         Female_Population = FemalePop) 

# I check the dimensions to compare to my expectations. I expect 1,392 rows and that is how many I have
dim(all_columns_4)

# Create a column to find migration flow by population, and find the mean of this column in 2015
# The average is 133.3353
all_columns_4$Mig_by_Pop <-
  all_columns_4$Total_Migration/all_columns_4$Total_Population

mean(all_columns_4$Mig_by_Pop[all_columns_4$Year == "2015"],  na.rm = T)

# Create a column to find refugees by migration flow, and find the mean of this column in 1990 and in 2015
# The average has decreased to 0.1515877 from 0.1544021
all_columns_4$Ref_by_imm <-
  all_columns_4$Refugee_Value/all_columns_4$Total_Migration

mean(all_columns_4$Ref_by_imm[all_columns_4$Year == "2015"],  na.rm = T)
mean(all_columns_4$Ref_by_imm[all_columns_4$Year == "1990"],  na.rm = T)

# Find the highest and smallest percentages of immigrants in a country in 2010
# I use a max and min formula on the Mig_by_pop column that I created
# the highest percentage is 878.4501 and the lowest is 0.6337663
max(all_columns_4$Mig_by_Pop[all_columns_4$Year == "2010"], na.rm = T)
min(all_columns_4$Mig_by_Pop[all_columns_4$Year == "2010"], na.rm = T)


# Find the median percentage of immigrants from the different continents/geographical areas in 2015
# I create a column that sums the immigrants from different areas and then another column to calculate the percentage of immigrants from different areas over total immigrants in 2015
# The median percentage of immigrants from different areas in 2015 is 0.937598
all_columns_4$Diff_areas <-
  all_columns_4$Africa + all_columns_4$Asia + all_columns_4$Oceania + all_columns_4$Developed + all_columns_4$Latam

all_columns_4$Imm_from_diff_areas <-
  all_columns_4$Diff_areas/all_columns_4$Total_Migration
median(all_columns_4$Imm_from_diff_areas[all_columns_4$Year == "2015"], na.rm = T)
