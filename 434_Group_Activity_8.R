######################################################
#        IN CLASS ACTIVITY (ASSIGNMENT #8)           #
######################################################
# Contributors: 
# Jake da Silva Passos-Hoioos
# Jane Huber
# Meghan Mokate
# Mia Krout
#
#   
#   Which drug had the largest increase in usage between 2010 and 2019? Given that, 
#   which wards were the top 5 (?) wards of reports of this usage?
#   
#   This is important because we can target city resources to provide harm reduction 
#   strategies, such as needle exchanges or drug addiction treatment programs, in wards 
#   that are the largest increase in specific drug use.
# 

library(tidyverse)
library(ggalt)
library(forcats)

###########################################
#               IMPORT DATA               #
###########################################

crimes_2010 <- read_csv("Crimes_2010.csv")
crimes_2019 <- read_csv("Crimes_2019.csv")

###########################################
#         Clean and Organize Data         #
###########################################

#Remove Categories we don't need for both datasets
narcotics_2010_r <-
  crimes_2010 %>%
  filter(`Primary Type` == "NARCOTICS") %>%
  select(c("Date",
           "Description",
           "Ward"))

narcotics_2010_r
#tibble: [43,393 x 3]

narcotics_2019_r <-
  crimes_2019 %>%
  filter(`Primary Type` == "NARCOTICS") %>%
  select(c("Date",
           "Description",
           "Ward"))

narcotics_2019_r
# A tibble: 12,294 x 3


#Combine datasets
narcotics_2010_2019 <-
  full_join(narcotics_2010_r, narcotics_2019_r)

narcotics_2010_2019
# A tibble: 55,687 x 3

#Edit the class of the date column to be the year
narcotics_2010_2019 <-
  narcotics_2010_2019 %>%
  separate(Date,
           into = c("Month", "Day", "Year")) %>%
  select(Year, Description, Ward)

narcotics_2010_2019
#A tibble: 55,687 x 3

#Since some of these dates are in 2020, filter them out.
narcotics_2010_2019$Year <- as.numeric(narcotics_2010_2019$Year)
class(narcotics_2010_2019$Year) #numeric

narcotics_2010_2019 <-
  narcotics_2010_2019 %>%
  filter(Year != 2020)

narcotics_2010_2019
# A tibble: 52,632 x 3


###########################################
#       Code Type Of Narcotics            #
###########################################

# See what drug types the descriptions can be grouped into
unique(narcotics_2010_2019$Description)

# create new dataset with a new column that labels drug types more broadly for analyzing at the group level
# use str_detect to search for certain words that identify drug
narcotics_2010_2019_1 <- narcotics_2010_2019 %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'CANNABIS'), "CANNABIS",`Description`)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'HEROIN'), "HEROIN", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'CRACK'), "CRACK", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'PCP'), "PCP", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'PARAPHERNALIA'), "PARAPHERNALIA", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'BARBITURATE'), "BARBITURATES", drug_type)) %>% 
  mutate(drug_type = ifelse(str_detect(`Description`, 'NARCOTICS'), "OTHER NARCOTICS", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'HALLUCINOGEN'), "HALLUCINOGENS", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'COCAINE'), "COCAINE", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'AMPHETAMINES'), "AMPHETAMINES", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'METHAMPHETAMINE'), "METHAMPHETAMINE", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'SYNTHETIC DRUGS'), 'SYNTHETIC DRUGS', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'DRUG EQUIPMENT'), 'DRUG EQUIPMENT', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'FORGE PRESCRIPTION'), 'ALTER/FORGE PRESCRIPTION', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'FORFEIT PROPERTY'), 'PROPERTY FORFEITURE', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'SYNTHETIC MARIJUANA'), 'SYNTHETIC MARIJUANA', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'CRIMINAL DRUG CONSPIRACY'), "OTHER DRUG CRIMES", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'CONT SUBS:FAIL TO MAINT RECORD'), "OTHER DRUG CRIMES", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'HYPODERMIC NEEDLE'), 'HYPODERMIC NEEDLE', drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'BARBITUATES'), "BARBITUATES", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, 'LOOK-ALIKE'), "OTHER NARCOTICS", drug_type)) %>%
  mutate(drug_type = ifelse(str_detect(`Description`, '<18'), "OTHER DRUG CRIMES", drug_type))

# Check on the new column groupings to make sure everything is grouped and makes sense
unique(narcotics_2010_2019_1$drug_type)

###########################
# Take a look at the data
narcotics_2010_2019_1

###########################################
#          Plotting the Data              #
###########################################
# Take a quick look at frequency of reported crimes for each drug type
ggplot(data = narcotics_2010_2019_1) +
  geom_bar(mapping = aes(y = drug_type))

# I want to do a dumbbell plot that shows the number of reports for each drug type.
#I want a point for 2010 and for 2019. I need the data to have a column for counts from each year.
narcotics_a <-
  narcotics_2010_2019_1 %>% 
  filter(drug_type != "SYNTHETIC MARIJUANA") %>%
  group_by(drug_type, Year) %>%
  count(drug_type, name = "Reports")

narcotics_a

narcotics <- narcotics_a %>%
  pivot_wider(names_from = "Year",
              values_from = "Reports") %>%
  mutate(per_change = round(((`2019` - `2010`)/`2010`) * 100, digits = 2)) %>%
  mutate(per_change_ordering = abs(round(((`2019` - `2010`)/`2010`) * 100, digits = 2))) %>%
  arrange(desc(per_change_ordering)) %>%
  mutate(drug_type = as.factor(drug_type))
  
  
narcotics$drug_type <-  fct_reorder(.f = narcotics$drug_type, .x = narcotics$per_change_ordering, .desc = F)

# Since we are adding a percent change, this might prove to be a good variable to sort the order of 
                
narcotics #Data looks good

#Now I can plot the data with two points
ggplot() +
  geom_dumbbell(data = narcotics, aes(y = drug_type, x = `2010`, xend = `2019`),
                size = 1.0, color = "black", size_x = 3, size_xend = 3,
                colour_x = "#27A192", colour_xend = "#A12736",
                alpha = 0.5) + #Use this to change the transparency of the dots
  labs(title = "Change in Narcotic Crime Reports from 2010 to 2019",
       x = "Number of Reports",
       y = "Narcotic Crime Reported") 


###########################################################
#               Enhancing the Plot                        #
###########################################################

# From the base plot, we need to add a number of elements for clarity and for practical 
# purposes. First, we need to duplicate the plots of the barbell so we have a mapping
# that we can create a legend for.  This way we can have a legend for the barbells 
# and make it clear which color corresponds to what year. Since our data is about Chicago, I've decided 
# to use colors from the Chicago flag, specifically the shades of red (#FF0000) and blue (#41b7e6). 
# A second element that should be added is something to convey the scale of the change, 
# whether in absolute terms or % change. This is especially important because the initial plot indicates
# we might want to transform the scale of the x-axis, as the large change in cannabis 
# numbers is making it hard to view the changes in the other drug categories that
# have lower counts to begin with. One possible solution may be to use a log transformation, 
# you just need to be sure this is clearly conveyed since log scales are not commonly used. 
# 
# Since I plan on including the % difference as a geom_text, I also plan on using the 
# reordered factor so I can display the drug_types in descending order of the absolute value
# of the % change so the most extreme changes (both positive and negative) display first. 

citywide_plot <- ggplot() +
  geom_dumbbell(data = narcotics, aes(y = factor(drug_type), x = `2010`, xend = `2019`),
                size = 1.0, color = "black", size_x = 3, size_xend = 3,
                colour_x = "#FF0000", colour_xend = "41b7e6",
                alpha = 0.5) +
  geom_point(data = narcotics_a, aes(x = Reports, y = drug_type, color = Year), size = 3, alpha = 0.5) +
  geom_rect(data = narcotics, aes(xmin = 50000, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#FFFFFF") +
  geom_text(data = narcotics, aes(label = paste0(per_change, "%"), y = drug_type, x = 50000), color = "#41b7e6", fontface = "bold", size = 4) +
  geom_text(data = filter(narcotics, drug_type == "METHAMPHETAMINE"), aes(x = 50000, y = drug_type, label = "% Difference"), color = "#41b7e6", size = 4.1, hjust = .6, vjust = -1.2, fontface = "bold") +
 labs(title = "Change in Narcotic Crime Reports in Chicago, IL from 2010 to 2019",
      subtitle = "with percent change from 2010 to 2019, City-Wide",
      caption = "X-Axis has been transformed by log10 for illustative purposes. Items on the Y-Axis are arranged in descending order of the absolute value of the percent change from 2010 to 2019.", 
      x = "Number of Reports",
      y = "Narcotic Crime Reported") +
  scale_x_continuous(trans = 'log10') + 
  scale_color_manual(name = "", values = c("#FF0000", "#41b7e6")) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#FF0000", size = 20, face = "bold"),
        plot.subtitle = element_text(face = "bold.italic"),
        plot.caption = element_text(face = "italic", hjust = 1.5),
        legend.position = "bottom")

citywide_plot

ggsave(filename = "Citywide Plot.png",
       plot = citywide_plot,
       width = 10,
       height = 10,
       units = "in")    


# For the most part, this plot reads like most other barbell plots, it shows the 
# change in number of reports in 2010 (red) and in 2019 (blue) with the crime types
# listed in descending order of the abs() of the % change, so the crime types are
# listed in descending order of the total % change with the greatest changes (in either direction) 
# listed first. Special attention should be given to the fact that the x-axis of this
# plot uses a logged scale, meaning that visually equal areas do not represent equal
# actual values. This means that lines towards the left of the plot will appear longer
# even if the absolute change in reported crimes is fairly small. For example, the
# line for cannabis is shorter than the line for "Other Drug Crimes" but represents a
# greater absolute change of nearly 20,000, despite the change in other crimes being 
# a difference of only 50.        

##################
#Almost all drugs saw a decrease in reports, so I want to look at reports by ward
#to see which 5 wards saw the smallest percent changes in narcotics
#reports from 2010 to 2019

narcotics_b <-
  narcotics_2010_2019_1 %>%
  group_by(Ward, drug_type, Year) %>%
  count(drug_type, name = "Reports")

narcotics_b

ward_narcotics <-
  narcotics_b %>%
  group_by(Ward) %>%
  summarise(report_2010 = sum(Reports[Year == 2010], na.rm = T),
            report_2019 = sum(Reports[Year == 2019], na.rm = T),
            diff = report_2019 - report_2010,
            perc_change = (diff / report_2010) * 100) %>%
  arrange(desc(perc_change))
ward_narcotics

ward_narcotics %>%
  top_n(5, wt = perc_change)
#We can see that wards 24,36, 27, 37 and 22 saw the smallest changes in narcotics
#crimes reported, though all were > 55%.

#Which 5 wards had the highest percent change in narcotic crime reports
ward_narcotics %>%
  top_n(-5, wt = perc_change)
#39,15,38,46,2

#Which 5 wards had the highest numbers of reports
ward_narcotics %>%
  arrange(desc(perc_change))
#28,24,27,37 and 16
#We can see that 3 (24, 27, 37) of the 5 wards with the highest number of  
#narcotic crime reports in 2010 saw the lowest percent changes in narcotics
#crimes in 2019
#24 = North Lawndale, some West Garfield Park
#27 = West Loop, parts of East Garfield and Humboldt Parks
#37 = Parts of Austin and Humboldt Park
#
#
############################################################
#     2nd Plot, A Look at the Worst Affected Wards         #
############################################################
# 
# Knowing that wards 24, 36, 27, 37, and 22 saw the least overall change in drug crime
# and three of them were in the highest number of crime reports in 2010, let's 
# look at the same plot we had before, but instead of city-wide crime data, let's 
# only look at these specific wards to see if any city-wide trends change in scale 
# or in direction. 

narcotics_c <-  narcotics_2010_2019_1 %>%
  filter(drug_type != "SYNTHETIC MARIJUANA") %>%
  filter(Ward == 24 | Ward == 36 | Ward == 27 | Ward == 37| Ward == 22) %>%
  group_by(drug_type, Year) %>%
  count(drug_type, name = "Reports")

         
narcotics_wards <- narcotics_c %>%
  pivot_wider(names_from = "Year",
              values_from = "Reports") %>%
  mutate(per_change = round(((`2019` - `2010`)/`2010`) * 100, digits = 2)) %>%
  mutate(per_change_ordering = abs(round(((`2019` - `2010`)/`2010`) * 100, digits = 2))) %>%
  arrange(desc(per_change_ordering)) %>%
  mutate(drug_type = as.factor(drug_type))

narcotics_wards$drug_type <-  fct_reorder(.f = narcotics_wards$drug_type, .x = narcotics_wards$per_change_ordering, .desc = F)

wards_plot <- ggplot() +
  geom_dumbbell(data = narcotics_wards, aes(y = drug_type, x = `2010`, xend = `2019`),
                size = 1.0, color = "black", size_x = 3, size_xend = 3,
                colour_x = "#FF0000", colour_xend = "41b7e6",
                alpha = 0.5) + 
  geom_point(data = narcotics_c, aes(x = Reports, y = drug_type, color = as.factor(Year)), size = 3, alpha = 0.5) +
  geom_rect(data = narcotics_wards, aes(xmin = 50000, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#FFFFFF") +
  geom_text(data = narcotics_wards, aes(label = paste0(per_change, "%"), y = drug_type, x = 50000), color = "#FF0000", fontface = "bold", size = 4) +
  geom_text(data = filter(narcotics_wards, drug_type == 'SYNTHETIC DRUGS'), aes(x = 50000, y = drug_type, label = "% Difference"), color = "#FF0000", size = 4.1, hjust = .6, vjust = -1.2, fontface = "bold") +
  labs(title = "Change in Narcotic Crime Reports in Chicago, IL from 2010 to 2019",
       subtitle = "with percent change from 2010 to 2019, Top 5 Wards with lowest percent change (Wards 24, 36, 27, 37, 22)",
       caption = "X-Axis has been transformed by log10 for illustative purposes. Items on the Y-Axis are arranged in descending order of the absolute value of the percent change from 2010 to 2019.", 
       x = "Number of Reports",
       y = "Narcotic Crime Reported") +
  scale_x_continuous(trans = 'log10') + 
  scale_color_manual(name = "", values = c("#FF0000", "#41b7e6")) +
  theme_minimal() +
  theme(plot.title = element_text(color = "#41b7e6", size = 20, face = "bold"),
        plot.subtitle = element_text(face = "bold.italic"),
        plot.caption = element_text(face = "italic", hjust = 1.5),
        legend.position = "bottom")

wards_plot

ggsave(filename = "Wards Plot.png",
       plot = wards_plot,
       width = 10,
       height = 10,
       units = "in")    

# From this we can see that most of the trends hold true, synthetic drugs, barbituates, and meth all 
# increased at both levels, BUT synthetic drugs saw a MUCH greater increase in these
# wards then city-wide (300% increase v. 91% increase). PCP crimes also saw a clear INCREASE in these
# wards unlike the generally unchanged city-wide rate.  