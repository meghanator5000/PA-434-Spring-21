# I bring in the libraries
library(randomNames)
library(dplyr) 

names_test <- randomNames(4)
names_test

class(names_test)
# the class of the names is character, which makes sense

# Variable 1: Set your seed and generate a random of sample of 40 observations ranging from 0 to 255. This variable represents how many days each individuals was unemployed in the past full year. Name it accordingly.
set.seed(434)

days_unemployed <- sample(x = 0:225, size = 40)
days_unemployed

set.seed(434)
job_apps <- sample(x = 1:25, size = 40, replace = T)
job_apps

set.seed(434)
gender <- sample(x = 0:1, size = 40, replace = T)
gender

set.seed(434)
names <- randomNames(n = 40, gender = gender)
names

unemployment_df <- as.data.frame( cbind( days_unemployed, job_apps, gender, names ))
unemployment_df

dim(unemployment_df)
# there are 40 rows and 4 columns, just as I wanted

class(unemployment_df)
# this is a dataframe, just like I wanted / expected

class(unemployment_df$days_unemployed)
# this is a character, but I think number of days should be numeric so I adjust for that below

unemployment_df$days_unemployed <- as.numeric(unemployment_df$days_unemployed)
class(unemployment_df$days_unemployed)
# yay, it is now numeric!

class(unemployment_df$job_apps)
#this is a character, but I think number of applications should be numeric so I adjust for that below

unemployment_df$job_apps <- as.numeric(unemployment_df$job_apps)
class(unemployment_df$job_apps)
# yay, it is now numeric!

class(unemployment_df$gender)
class(unemployment_df$names)
# these are both characters, which I think is good for these columns

# Average number of unemployment day and standard deviation
mean(unemployment_df$days_unemployed)
sd(unemployment_df$days_unemployed)
# the mean of unemployment days is 113.8 (yikes, that is really high) and the standard deviation is 60.35374, this is more than half the mean which is pretty large, so the data is likely spread out

min(unemployment_df$job_apps)
max(unemployment_df$job_apps)
mean(unemployment_df$job_apps)
# since I set the range for the sample, the min and max are unsurprisingly 1 and 25; the mean is about halfway through at 13.025, which is a lot of job applications

# Number of male and female applicants included in the sample
sum(unemployment_df$gender == 1, na.rm=TRUE)
sum(unemployment_df$gender == 0, na.rm=TRUE)
# there are 22 female and 18 male applicants in the sample, for a total of 40 applicants

# Average number of submitted applications for female applicants
female_applicants <- unemployment_df %>%
  filter(gender == 1)
female_applicants

# test the average number of job_apps for the female dataframe
mean(female_applicants$job_apps)
# the mean of the number of job applications submitted by female applicants is VERY slightly higher than the overall mean

# Maximum number of employment days for female and male applicants
unemployment_df$days_employed <- 365 - unemployment_df$days_unemployed
unemployment_df$days_employed
max(unemployment_df$days_employed)
# the maximum days employed total is 355 days

# since I created the female dataframe before adding this column, I went back to add it now the same way as for the general dataframe
female_applicants$days_employed <- 365 - female_applicants$days_unemployed
female_applicants$days_employed
max(female_applicants$days_employed)
# the maximum identified above as 355 days was a woman

# I created a male_applicants dataframe the same way as I did for female applicants, by filtering by gender
male_applicants <- unemployment_df %>%
  filter(gender == 0)
male_applicants
max(male_applicants$days_employed)
# the maximum employment days for men was 353

mean(female_applicants$days_unemployed)
mean(male_applicants$days_unemployed)
# female applicants are unemployed an average of 128 days and male applicants are unemployed an average of 96 days
# tested these means to get a better idea of gender gaps to consider alongside policy

# Think about policy! 
# Based on this dataset, I would direct policy makers to the high average of unemployed days for the sample (113). This reflects that there is a problem for unemployment. 
# While the distribution is spread out, it should still be a goal to get this average down and consider the distribution.
# In terms of gender disparity, female applicants are unemployed an average of 128 days, much higher than the male average of 96. Getting both of these numbers lower and decreasing this gap should be a focus of policy. 
# Also, the average number of applications submitted is 13, which is a lot of applications per applicant. This means that people are looking for work actively, but are still spending a third of the year unemployed on average. 









