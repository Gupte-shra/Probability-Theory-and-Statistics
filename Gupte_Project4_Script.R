library(janitor)
library(pacman)
library(lubridate)
library(tidyverse)
library(dplyr)
#Preliminary Work – Getting Started

# I have chosen the data from the Kaggle.com on the English Education of the population in a certain region. 
# As required the data set has 1105 rows that is within the limit of  and 31 attributes approved by the professor. 

#3-4 Questions to analyse the data are -
#Q1 - Comparison of Job Density, Income, Population and Education. 
#Q2 - Which town name has the most education stats of student population?
#Q3 - What is the population on the town name with lowest education level?

## Part 1 = Exploring
# Question 1 - Review the data set - Please refer to slide deck, slide 1

#Question 2 - Cleaning the data set 
#Importing the data frame in the R Script
Eng_edu <- read.csv("/Users/shraddha/Desktop/MPSA - SHRADDHA GUPTE/Introduction to Analytics ALY6000/Module 4/Gupte_Project4/archive (4)/english_education.csv")

#Cleaning the names in the Eng_edu data_frame
Eng_edu <- clean_names(Eng_edu)

#Renaming columns
Eng_edu <- Eng_edu %>% rename(
  Population_Size = population_2011, 
  Town_Name = town11nm, 
  Town_Size = size_flag, 
  Region = rgn11nm, 
  Work_Area = ttwa_classification, 
  Job_Density = job_density_flag, 
  Income = income_flag, 
  University = university_flag, 
  Education_Score = education_score,
  Age_group_35_64 = level4qual_residents35_64_2011, 
  Child_count = ks4_2012_2013_counts)
Eng_edu

#Removing columns = removing the unnecessary column from the data frame 
Eng_edu <- subset(Eng_edu, select = -c(town11cd, coastal, coastal_detailed, ttwa11cd, ttwa11nm, key_stage_2_attainment_school_year_2007_to_2008))
Eng_edu

#Managing NA
Eng_edu <- Eng_edu[!is.na(Eng_edu$Population_Size),]

#removing rows with region below average population size 
mean_value <- mean(Eng_edu$Population_Size)
mean_value
#Considering the regions with above average population size
Eng_edu <- filter(Eng_edu, Eng_edu$Population_Size>mean_value)

#Correcting the data type
Eng_edu$Population_Size <- as.numeric(Eng_edu$Population_Size)

#Organizing the data to round up the numbers
Eng_edu$Education_Score <- round(Eng_edu$Education_Score,0)
Eng_edu

#Question 3 - Descriptive statistics for Education Score - Please refer to slide 1
mean_edu <- mean(Eng_edu$Education_Score)
mean_edu <- round(mean_edu, 0)
mean_edu
median_edu <- median(Eng_edu$Education_Score)
median_edu
mode_edu <- mode(Eng_edu$Education_Score)
mode_edu <- as.integer(mode_edu)
mode_edu<- na.omit(mode_edu)
mode_edu
sd_edu <- sd(Eng_edu$Education_Score)
sd_edu
var_edu <- var(Eng_edu$Education_Score)
var_edu

#Question 4 - Visualization based on raw data - Please refer to slide deck, slide 2 & 3
library(ggplot2)

#a. Analysis based on Population - Please refer to slide deck, slide 2 A 
Population_town_df <- Eng_edu %>% group_by(Town_Size) %>% summarise(Population_count = sum(Population_Size))
Population_town_df
ggplot(Population_town_df, aes(x = Town_Size, y = Population_count , fill = Town_Size)) +
geom_bar(stat = "identity", width = 0.75) +  
labs(title = "Population Count by Town Size",fill = "Town Size",y = NULL) +
theme_minimal() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + scale_y_continuous(labels = scales::label_number())


#b. Analysis based on University - Please refer to slide deck, slide 2 B
Uni_town_df <- Eng_edu %>% group_by(University, Town_Size) %>% summarise(Uni_count = n())
Uni_town_df
ggplot(Uni_town_df, aes(x = University, y = Uni_count, fill = Town_Size)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "University Count by Town Size", x = "University", y = "Count") +
theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1))

#c. Education level population from age group 35-64 - Please refer to slide deck, slide 3 C
Age_group_df <- Eng_edu %>% group_by(Age_group_35_64, Town_Size) %>% summarise(Population_count = sum(Population_Size))
Age_group_df
ggplot(Age_group_df, aes(x = Town_Size, y = Population_count , fill = Age_group_35_64)) +
geom_bar(stat = "identity", position = "dodge") +labs(title = "Education level of citizens from age group 35-64 by Population Size",
x = "Town Size", y = "Population Count") +
theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  scale_y_continuous(labels = scales::label_number())

#d. Latest student count at key stage 4 - Please refer to slide deck, slide 3 C
Student_count_df <- Eng_edu %>% group_by(Town_Size) %>% summarise(student_count = sum(Child_count))
Student_count_df
ggplot(Student_count_df, aes(x = Town_Size, y = student_count, fill = Town_Size)) +
geom_bar(stat = "identity", position = "dodge") + labs(title = "Current student count at key stage 4",
x = "Town Size", y = "Student Count") +
theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Part 2 = Expanding
# Question 1  - Creating new variables by adding new columns

#a. Average of level of education by students at age 19
Eng_edu$edu_level_19 <- (Eng_edu$activity_at_age_19_full_time_higher_education + 
                        Eng_edu$activity_at_age_19_sustained_further_education + 
                        Eng_edu$activity_at_age_19_appprenticeships)/3
Eng_edu$edu_level_19

#b. Average of level of education by students at age 18
Eng_edu$edu_level_18 <- (Eng_edu$level_2_at_age_18 + Eng_edu$level_3_at_age_18)/2
Eng_edu$edu_level_18
Eng_edu

#Question 2 - Creating the new data frame by selecting only the required columns for the analysis
New_eng_edu_df <- subset(Eng_edu, select = c(Town_Name, Population_Size, Town_Size, Region, Work_Area, Job_Density, Income, University, Age_group_35_64, Child_count, edu_level_18, edu_level_19, highest_level_qualification_achieved_b_age_22_average_score, Education_Score))
New_eng_edu_df <- New_eng_edu_df %>% rename(edu_level_22 = highest_level_qualification_achieved_b_age_22_average_score)
New_eng_edu_df$edu_level_18 <- round(New_eng_edu_df$edu_level_18, 0) 
New_eng_edu_df$edu_level_19 <- round(New_eng_edu_df$edu_level_19, 0) 
New_eng_edu_df$edu_level_22 <- round(New_eng_edu_df$edu_level_22, 0)
New_eng_edu_df

#arranging the data based on education score
New_eng_edu_df<- arrange(New_eng_edu_df, desc(Education_Score))
New_eng_edu_df
New_eng_edu_df$Working_Population <- (New_eng_edu_df$Population_Size - New_eng_edu_df$Child_count)
New_eng_edu_df


#Question 3 - Extracting Information and Producing Visualisation - Please refer to slide deck, slide 4 - 8
#a. Understanding the Job density and Income based on Population - Please refer to slide deck, slide 4
Job_Income_pop_df <- New_eng_edu_df %>% group_by(Job_Density, Income) %>% summarise(working_count = sum(Working_Population))
Job_Income_pop_df

# Filtering the data for each job density
cities_df <- subset(Job_Income_pop_df, Income == "Cities")
HD_df <- subset(Job_Income_pop_df, Income == "Higher deprivation towns")
LD_df <- subset(Job_Income_pop_df, Income == "Lower deprivation towns")
MD_df <- subset(Job_Income_pop_df, Income == "Mid deprivation towns")

# Creating a function to plot a pie chart
plot_pie_chart <- function(data, title) {ggplot(data, aes(x = "", y = working_count, fill = Job_Density)) +
geom_bar(stat = "identity", width = 1) +coord_polar("y", start = 0) +labs(title = title,
fill = "Job Density",y = NULL) +theme_void()}

# Creating three pie charts for each job density
Cities_plot <- plot_pie_chart(cities_df, "Population Count based on Job Density in Cities")
HD_plot <- plot_pie_chart(HD_df, "Population Count based on Job Density in Higher deprivation towns")
LD_plot <- plot_pie_chart(LD_df, "Population Count based on Job Density in Lower deprivation towns")
MD_plot <- plot_pie_chart(MD_df, "Population Count based on Job Density in Mid deprivation towns")
Cities_plot
HD_plot
LD_plot
MD_plot

#b. Understanding the Job density and Income based on Education Score - Please refer to slide deck, slide 5
Job_Income_edu_df <- New_eng_edu_df %>% group_by(Job_Density, Income) %>% summarise(Edu_total = sum(Education_Score))
Job_Income_edu_df

# Filtering the data for each job density
cities_df <- subset(Job_Income_edu_df, Income == "Cities")
HD_df <- subset(Job_Income_edu_df, Income == "Higher deprivation towns")
LD_df <- subset(Job_Income_edu_df, Income == "Lower deprivation towns")
MD_df <- subset(Job_Income_edu_df, Income == "Mid deprivation towns")

# Creating a function to plot a pie chart
plot_pie_chart <- function(data, title) {ggplot(data, aes(x = "", y = Edu_total , fill = Job_Density)) +
geom_bar(stat = "identity", width = 1) +coord_polar("y", start = 0) +labs(title = title,
fill = "Job Density",y = NULL) +theme_void()}

# Creating three pie charts for each job density
Cities_plot <- plot_pie_chart(cities_df, "Education Score based on Job Density in Cities")
HD_plot <- plot_pie_chart(HD_df, "Education Score based on Job Density in Higher deprivation towns")
LD_plot <- plot_pie_chart(LD_df, "Education Score based on Job Density in Lower deprivation towns")
MD_plot <- plot_pie_chart(MD_df, "Education Score based on Job Density in Mid deprivation towns")
Cities_plot
HD_plot
LD_plot
MD_plot

#c. Education stats for the current student population - Please refer to slide deck, slide 6
#Computing % score for each stat individually since scales are not consistent.
New_eng_edu_df$Edu_stat_18 <- round(((max(New_eng_edu_df$edu_level_18) - New_eng_edu_df$edu_level_18)/max(New_eng_edu_df$edu_level_18)*100),0)
New_eng_edu_df$Edu_stat_19 <- round(((max(New_eng_edu_df$edu_level_19) - New_eng_edu_df$edu_level_19)/max(New_eng_edu_df$edu_level_19)*100),0)
New_eng_edu_df$Edu_stat_22 <- round(((max(New_eng_edu_df$edu_level_22) - New_eng_edu_df$edu_level_22)/max(New_eng_edu_df$edu_level_22)*100),0)
New_eng_edu_df$Edu_stat_18
New_eng_edu_df$Edu_stat_19
New_eng_edu_df$Edu_stat_22

# Calculating overall_score which is the avg of % of each stat for every age group.Please refer to slide deck, slide 7
New_eng_edu_df$Edu_stat_overall <- round((New_eng_edu_df$Edu_stat_18+New_eng_edu_df$Edu_stat_19+New_eng_edu_df$Edu_stat_22)/3,0)
New_eng_edu_df$Edu_stat_overall

hist(New_eng_edu_df$Edu_stat_18, col = 'magenta', border = 'black', ylab = 'Number of Towns', xlab = 'Student education score at 18', main = 'Number of towns based on education score at 18')
hist(New_eng_edu_df$Edu_stat_19, col = 'purple', border = 'black', ylab = 'Number of Towns', xlab = 'Student education score at 19', main = 'Number of towns based on education score at 19')
hist(New_eng_edu_df$Edu_stat_22, col = 'navyblue', border = 'black', ylab = 'Number of Towns', xlab = 'Student education score at 22', main = 'Number of towns based on education score at 22')
hist(New_eng_edu_df$Edu_stat_overall, col = 'red', border = 'black', ylab = 'Number of Towns', xlab = 'Overall Student education score', main = 'Number of towns based on overall education score')

# Selecting top and bottom 10 towns with high education stat.
#i. Top
Edu_stat_Hten <- head(arrange(New_eng_edu_df, desc(Edu_stat_overall)),10)
Edu_stat_Hten

ggplot(Edu_stat_Hten, aes(x = Edu_stat_overall, y = Town_Name, fill ="orange")) + 
geom_bar(stat = "identity") + labs(x = "Overall Current Education Score")

#ii. Bottom
Edu_stat_Tten <- tail(arrange(New_eng_edu_df, desc(Edu_stat_overall)),10)
Edu_stat_Tten

ggplot(Edu_stat_Tten, aes(x = Edu_stat_overall, y = Town_Name, fill = "orange")) + 
geom_bar(stat = "identity") + labs(x = "Overall Current Education Score")

#d. Comparing the current student education score and overall education score -Please refer to slide deck, slide 8
ggplot(New_eng_edu_df, aes(x = Edu_stat_overall, fill = "Student Score")) +
geom_boxplot(aes(y = 1)) +  geom_boxplot(aes(x = Education_Score, fill = "Overall Education Score", y = 2)) + 
labs(title = "Box Plot of Student Education Score and Overall Education Score", y = "Education Score",
fill = "Score Type") + theme_minimal() +theme(legend.title = element_blank()) 
+scale_y_continuous(limits = c(0, 3)) + scale_x_continuous(limits = c(-10, 25)) 

#e. Identifying Population size with Lowest Education score - Please refer to slide deck, slide 9
Low_Edu_score <- tail(arrange(New_eng_edu_df, desc(Education_Score)),10)
Low_Edu_score
Low_Edu_score <- filter(Low_Edu_score, Education_Score < -4)
Low_Edu_score
ggplot(Low_Edu_score, aes(x = Town_Name, y = Working_Population , fill = Town_Size)) +
geom_bar(stat = "identity", position = "dodge") +labs(title = "Lowest education level (-5) of current working population ",
x = "Town Name", y = "Population Size") +theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  scale_y_continuous(labels = scales::label_number())

## Part 3 = Communicating -  Please refer to slide 10

# Question 1 - Please refer to slide 10
# Question 2 - All tasks have been performed in R. 


