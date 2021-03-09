# install the package
install.packages(tidytuesdayR)

# load the libraries I will be using
library(tidytuesdayR)
library(tidyverse)
library(haven) # will use this for my regression line

# pull in the dataset I will be visualizing
tuesdata <- tidytuesdayR::tt_load('2021-01-05')
transit_cost <- tuesdata$transit_cost

# examine the dataset to assess variables and determine what questions I have
view(transit_cost)

# clean up dataset
df <- transit_cost %>% 
  drop_na(e) %>% # several unwanted na values in e
  mutate(real_cost = as.double(real_cost), na.rm = T) # change this column to double

# Question: What is the relationship between cost and tunnel length per city in the US and Canada?
# For this question, I need to keep columns for country, city, length, and real_cost

df_1 <- df  %>% # I create a new dataset 
  select(country, city, length, real_cost) %>% # I limit to only the columns I am assessing
  filter(country %in% c("US", "CA")) # I filter so that I only look at the US and Canada

lm(data = df_1, real_cost ~ length) # will use this to find the intercept and slope for my regression line
# the intercept is 3513.58 and the slope is 59.21.
# this line on the graph will give me a better idea of which values have high/low real costs relative to their length

ggplot(data = df_1) +
  geom_point(mapping = aes(x = length, # length is independent, goes on the x axis, I chose to use a scatter plot because I am curious about the relationship between the variables
                           y = real_cost, # cost is likely dependent on length
                           shape = country, # I distinguish country by shape
                           color = city), # I distinguish city by color
             size = 4) + # The points were small so I made them bigger
  theme_linedraw() + # choose a theme, I also liked classic but this one is easier to see values
  labs(title = "Transit Length by Cost in US and Canadian Cities", # Give the plot a title
       x = "Length of proposed line in km", # use more helpful axis name
       y = "Real cost in Millions of USD",  # use more helpful axis name
       color = "City", # capitalize legend title
       shape = "Country")  + # capitalize legend title
  scale_color_manual(values = c("#33FCFF", # the default colors had some similar colors, so I chose my own that are more different
                                "#7C33FF", 
                                "#3BFF33",
                                "#FF5733", # I chose for New York to be red since it is a significant finding in my opinion - high cost, low length
                                "#3387FF", 
                                "#C733FF", 
                                "#FF33D7", 
                                "#FF3388",
                                "#FFBC33",
                                "#33FF92")) +
  theme(
    plot.title = element_text(
      face = "bold", # I want the title to be bold
      size = 12, # I increase the size of the title
      hjust = .5, # I center the title horizontally
      vjust = .5)) + # I center the title vertically
  geom_abline(aes(intercept = 3513.58, slope = 59.21)) # I use my regression analysis from earlier to plot a line. This makes it easier to compare the actual values to what they are predicted to be

# I closed out my graph and came back to it a few days later
# The most significant finding I see is that New York has short lengths and high costs compared to other US and Canadian cities
# I also notice that Toronto has longer transit lengths than the other cities
# It seems that overall, Canadian cities have longer lengths than US cities
# New York, San Jose, and Toronto have the highest costs relative to transit lengths

