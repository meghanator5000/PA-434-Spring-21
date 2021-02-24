# I load in the tidyverse library
library(tidyverse)
library(haven) # add in the haven library since I will use this for my regression line later

# I load the api dataset that I downloaded
api = read_csv("api_data.csv") 

# Step 1 -  Before starting your analysis
# 1. Question 1
# The unit of analysis is schools. Characteristics for each school are provided.

# 2. Question 2
# This data is not tidy because each observation does not have its own column 
# since variable_name includes three variables and community_school includes two variables
api_2 <- api %>%
  separate( 
           "community_schooltype", # I separate this column since I want each observation to have its own column
           into = c("community", "schooltype"), # I name the separate columns that I will create
           sep = "_") %>% # I specify that the observations are separated by the "_"
  pivot_wider(
            names_from = "variable_name", # I want to make the observations in the variable_name column into their own columns by pivoting
            values_from = "percentage") # I want the values to come from percentage
api_2 # view the data and it looks tidy now

# 3. Question 3
colSums(is.na(api_2)) # Check to see how many missing values there are by column
# There are no missing values in the dataset


# Step 2 - Exploratory data analysis using plots

# 1. Question 1

ggplot(data = api_2) +
  geom_density(mapping = aes(x = meals)) + # specify that I am looking at meals
  labs(title = "Distribution of percentage of school students eligible for subsidized meals", # give the chart a title
       x = "Percentage of school students eligible for subsidized meals", # rename the x axis
       y = "Density") + # rename the y axis
  geom_vline(xintercept = mean(api_2$meals)) # add a line where the mean of the distribution is
# the chart shows that there is a fairly even distribution of percentage of school students eligible for subsidized meals
# the mean of the meals percentage variable is just under 50%
# Zero and 100 have the lowest density, so the least amount of schools have those values for meals percentage 

ggplot(data = api_2) +
  geom_density(mapping = aes(x = colgrad)) + # specify variable I am looking at
  labs(title = "Distribution of school parents with college degrees", # give the chart a clear title
       x = "Percentage of school parents with college degrees", # rename the x axis
       y = "Density") + # rename the y axis
  geom_vline(xintercept = mean(api_2$colgrad)) # add a line where the mean of the distribution is
# the chart shows that there is a skewed distribution of density for percentage of school parents with college degrees
# Majority of schools have a colgrad percentage at around 10%
# the mean of the colgrad percentage variable is about 21%
# Values over 50% have very low density 

ggplot(data = api_2) +
  geom_density(mapping = aes(x = fullqual)) + # specifiy that I am looking at the fullqual variable
  labs(title = "Distribution of percentage of fully qualified teachers", # give the chart a title
       x = "Percentage of fully qualified teachers", # name the x axis
       y = "Density") + # name the y axis
  geom_vline(xintercept = mean(api_2$fullqual))
# the chart shows that there is left skewed distribution of density for percentage of fully qualified teachers
# Majority of schools have a colgrad percentage at between 95 and 100%
# the mean of the fullqual percentage variable is about 88%
# Values under 75% have very low density 

ggplot(data = api_2) +
  geom_density(mapping = aes(x = api)) + # specify that I am looking at API
  labs(title = "Distribution of Academic Performance Index", # give the chart a title
       x = "Academic Performance Index", # rename the x axis
       y = "Density") + # rename the y axis
  geom_vline(xintercept = mean(api_2$api)) + # add a line to show the mean
  geom_vline(xintercept = min(api_2$api)) + # add a line to show the min value
  geom_vline(xintercept = max(api_2$api))  # add a line to show the max value
# The chart shows that the distribution of API peaks around the mean of 681 and gradually decreases as API moves away from the mean in both directions
# School density shows that distribution of API values falls between 336 and 959

ggplot(data = api_2) +
  geom_bar(mapping = aes(x = county)) + # specify that I am looking at county
  labs(title = "Number of Records by County", # give the chart a title
       x = "County", # rename the x axis
       y = "Count") + # rename the y axis
  scale_x_discrete(guide = guide_axis(angle = 90)) # rotate the x axis labels so that I can see the county name values to assess
# I can see that some of the counties have more records than others
# I can reference this chart if I am curious about how many records there are about each county

ggplot(data = api_2) +
  geom_bar(mapping = aes(x = community)) + # specify that I am looking at community
  labs(title = "Number of Records by Community", # give the chart a title
       x = "Community", # rename the x axis
       y = "Count") # rename the y axis
# suburban communities have the most records by far, followed by urban then rural
  
ggplot(data = api_2) +
  geom_bar(mapping = aes(x = schooltype)) + # specify that I am looking at community
  labs(title = "Number of Records by School Type", # give the chart a title
      x = "School Type", # rename the x axis
      y = "Count") # rename the y axis
# By far, there are the most records for elementary schools, then middle schools, and lastly high schools


# 2. Question 2
ggplot(data = api_2, 
       mapping = aes(x = api, # specify the x value
                     y = colgrad)) + # specify the y value
  geom_point() + # create a scatter plot to see relationship
  labs(title = "Relationship of Performance Index to Parents with College Degrees", # give the chart a title
       x = "Academic Performance Index", # rename the x axis
       y = "Percentage of school parents with college degrees")
# API and colgrad are correlated, as API increases, so does colgrad

# Question 2a. 
# Before drawing the graph, I expected for a higher percentage of school parents with college degrees to correlate to a higher academic performance index
# From the scatter plot, I learned that my expectations were accurate
# While I can see this trend by looking at the plot, there is also a wide distribution range of values
# For instance, an API of about 800 has colgrad values ranging from close to zero to about 80%

# Question 2a continued
lm(data = api_2, colgrad ~ api) # I find the slope and the intercept of colgrad and api
ggplot(data = api_2) +
  geom_point(mapping = aes(x = api, y = colgrad)) + # I create the plot just like I did above
  geom_abline(aes(intercept = -37.3709, slope = 0.0855), color = "pink", size = 2) + # I add a regression line based on the intercept and slope I found above, and make it pink and thick so that I can see it
  labs(title = "Relationship of Performance Index to Parents with College Degrees", # give the chart a title
     x = "Academic Performance Index", # rename the x axis
     y = "Percentage of school parents with college degrees") # rename the y axis

# Question 2b. 
ggplot(data = api_2,
       mapping = aes(x = api, # establish the x axis
                     y = colgrad, # establish the y axis
                     color = community)) + # breakdown community type using color
  geom_point() + # specify scatter plot
  facet_wrap(~schooltype) + # use facet wrap to split the graph by school type
  labs(title = "Performance Index by Parents' Education by School Type", # specify title
       x = "Academic Performance Index", # label the x axis 
       y = "Percentage of School Parents with College Degrees", # label the y axis
       caption = " E stands for elementary school, M stands for middle school, and H stands for high school", # specify what each of the letters stand for in a caption for clarity
       color = "Community Type") # give name to color legend

# Question 2c. 
# The finalized chart shows the same positive relationship between colgrad and api that I saw in earlier graphs for each school and community types
# The chart shows that there are much more points in the elementary graph than in the high school or middle school graph
# The color shows that there are much more suburban schools at all school types than rural or urban

# Question 3

highest <- api_2 %>% 
  arrange(desc(api)) %>% # arrange in order of highest to lowest apis
  slice(1:10) %>% # limit to the top 10 highest apis
  mutate(rank = "Top 10") # add a new row to specify that these are the top 10 values

lowest <- api_2 %>%
  arrange(api) %>% # arrange in order of lowest to highest apis
  slice(1:10) %>% # limit to the top 10 lowest apis
  mutate(rank = "Bottom 10") # add a new row to specify that these are the bottom 10 values

api_3 <-
  bind_rows(highest, lowest) # combine the two datasets I just made, so that the rows are stacked on top of one another
  
ggplot(data = api_3,
       mapping = aes(x = reorder(as.character(uid), -api), # specify that the x axis should be the UID value for the school, ordered it from highest to lowest API
                     y = api, # specify that API is on the y axis
                     fill = rank)) + # use rank to color the top 10 and bottom 10 values differently
  geom_col() + # specify that it is a column graph
  labs(x = "School Unique Identifier",   # change x axis label
       y = "Academic Performance Index", #  change y axis label
       title = "Highest and Lowest Academic Performance by School ID", # give the chart a table 
       fill = "Category") + # rename the legend for the color fill
  geom_hline(yintercept = mean(api_4$avg_api)) + # add a horizontal line at the mean
  scale_fill_discrete(breaks=c("Top 10","Bottom 10")) +  # switch the legend so that Top 10 is above Bottom 10, since it makes more sense that way
  scale_x_discrete(guide = guide_axis(angle = 90)) # rotate the x axis labels so that I can see the uids more clearly


# Question 4
# Question 4a
# Think about a couple of criteria to select priority counties

# Using my analysis from question 3, I know that there is a positive relationship between colgrad and api
cor.test(api_2$api, api_2$colgrad) # the correlation coefficient between the variables is 0.6894666
# Schools with lower colgrad percentages may need higher funding, since they usually have lower APIs

ggplot(data = api_2,
       mapping = aes(x = api, # specify that the x value is API
                     y = meals)) + # look into percentage of school students eligible for subsidized meals
  geom_point() + # use a scatter plot to test the relationship
  facet_wrap(~community) +
  labs(title = "Meals by API by Community",
       x = "Academic Performance Index",
       y = "Percentage of school students eligible for subsidized meals") # give a title and rename axes

cor.test(api_2$api, api_2$meals) # the correlation coefficient between the variables is -0.8936422
# there is a negative relationship between api and meals, a higher percentage of students eligible for meals correlates to a lower api
# Schools with higher meals percentages may need higher funding, since they usually have lower APIs

ggplot(data = api_2,
       mapping = aes(x = api, # specify that the x value is API
                     y = fullqual)) + # look into percentage of fully qualified teachers
  geom_point() + # use a scatter plot to test the relationship
  facet_wrap(~community) +
  labs(title = "Fully Qualified Teachers by API by Community",
       x = "Academic Performance Index",
       y = "Percentage of fully qualified teachers") # give a title and rename axes
# There is a positive relationship between fully qualified teachers and API
# Schools with lower fullqual percentages may need higher funding, since they usually have lower APIs

cor.test(api_2$api, api_2$fullqual) #  the correlation coefficient between the variables is 0.6110846

# The trends established above are consistent across communities, as shown in the faceted plots

# Question 4b
# I decide to focus on the percentage variables of fullqual, colgrad, and meals to determine funding priority, along with API

api_4 <- api_2 %>% # create a new dataset
  group_by(county) %>% # group by county, since this is what I care about
  summarize(avg_api = mean(api), # calculation county mean for api
            avg_fullqual = mean(fullqual), # calculation county mean for fullqual
            avg_colgrad = mean(colgrad), # calculation county mean for colgrad
            avg_meals = mean(meals)) %>% # calculation county mean for meals
  filter(avg_api <= mean(avg_api)) %>% # I want to limit my analysis to only look at counties with APIs that are below average
  # it is my opinion that funding should be directed toward counties with lower APIs than the average
  # a more limited view is easier to assess
  mutate(fund = case_when((avg_fullqual <= mean(avg_fullqual) # create a new column to determine funding priority
                           & avg_meals >= mean(avg_meals)
                           &  avg_colgrad <= mean(avg_colgrad)) ~ "high",
                          # counties are high priority if they have lower than average fullqual, higher than average meals, AND lower than average colgrad
                          # I determined this from analyzing correlation earlier
                          (avg_fullqual >= mean(avg_fullqual) 
                           | avg_meals >= mean(avg_meals)
                           |  avg_colgrad >= mean(avg_colgrad)) ~ "medium",
                          # columns are medium priority if they have lower than average fullqual, higher than average meals, OR lower than average colgrad
                          TRUE ~ "low"))
                          # other columns are low priority

# I present the findings in my new dataset as a column graph
ggplot(data = api_4,
       mapping = aes(x = county, # x axis is county
                     y = avg_api, # y axis is average API
                     fill = fund)) + # color coded by my new column, fund
  geom_col() + # column graph
  scale_x_discrete(guide = guide_axis(angle = 90)) + # rotate x axis labels to look better
  labs(title = "API Detail by County", # give a new title
       x = "County", # label x axis
       y = "Average API", # label y axis
       fill = "Funding Priority") # label color legend



