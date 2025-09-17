#Shraddha Gupte, 25th January 2024 and ALY6000 Introduction to Analytics
library(tidyverse)

# Assignment Part 1

# Question 1
data_2015 <- read.csv("/Users/shraddha/Desktop/MPSA - SHRADDHA GUPTE/Introduction to Analytics ALY6000/Module 2/Gupte-Project2.R/2015.csv")
head(data_2015)

# Question 2
names(data_2015)

# Question 3
# Was a view function hence removed

# Question 4
glimpse(data_2015)

# Question 5 Installing Janitor
library(janitor)
data_2015 <- clean_names(data_2015)
data_2015
original_data <- data.frame(data_2015)

# Question 6
happy_df <- original_data[c("country","region","happiness_score","freedom")]
happy_df

# Question 7
top_ten_df <- head(happy_df, 10)
top_ten_df

# Question 8
no_freedom_df <- filter(happy_df, freedom < 0.20)
no_freedom_df

# Question 9
best_freedom_df <- arrange(happy_df, desc(freedom))
best_freedom_df

# Question 10
data_2015$gff_stat <- rowSums(data_2015[c("family","freedom","generosity")])
data_2015

# Question 11
group_happy_df <- group_by(happy_df, region)
group_happy_df
regional_stats_df <- group_happy_df %>% summarise(country_count = n(), mean_happiness = mean(happiness_score) , mean_freedom = mean(freedom))
regional_stats_df

# Assignment Part 2

# Question 12
baseball <- read.csv("/Users/shraddha/Desktop/MPSA - SHRADDHA GUPTE/Introduction to Analytics ALY6000/Module 2/Gupte-Project2.R/baseball.csv")
baseball

# Question 13
head(baseball)
names(baseball)
glimpse(baseball)

# Question 14
baseball <- filter(baseball, AB != 0)
baseball

# Question 15
baseball$BA <- baseball$H / baseball$AB
baseball

# Question 16
baseball$OBP <- (baseball$H + baseball$BB) / (baseball$AB + baseball$BB)
baseball

# Question 17
strikeout_artist <- head(arrange(baseball, desc(SO)), 10)
strikeout_artist

# Question 18
eligible_df <- filter(baseball, AB >=300 | G>=100)
eligible_df

# Question 19
hist(eligible_df$BA, col = 'green', border = 'blue',ylab = 'count', xlab = 'BA')

# Question 20

# selecting only important stats columns from baseball and storing in MVP_df.
MVP_df <- baseball[c("Last", "First", "OBP", "HR", "RBI")]
MVP_df

# Getting Mean score for each imp stat.
meanOBP <- round(mean(MVP_df$OBP), 2)
meanHR <- round(mean(MVP_df$HR),0)
meanRBI <- round(mean(MVP_df$RBI),0)
meanOBP
meanHR
meanRBI

# Filtering players above mean score to get list of above avg players.
MVP_df <- filter(MVP_df, OBP>0.25 & HR>9 & RBI>39)
MVP_df

	
MVP_df$OBP <- round((((max(MVP_df$OBP) - MVP_df$OBP) / max(MVP_df$OBP) )*100),0)
MVP_df$HR <- round((((max(MVP_df$HR) - MVP_df$HR) / max(MVP_df$HR) )*100),0)
MVP_df$RBI <- round((((max(MVP_df$RBI) - MVP_df$RBI) / max(MVP_df$RBI) )*100),0)
MVP_df

# Calculating overall_score which is the avg of % of each stat for every player.
MVP_df$overall_score <- round((MVP_df$OBP + MVP_df$HR + MVP_df$RBI) / 3, 0)
MVP_df

# Selecting top 10 high score value players.
MVP_df_ten <- head(arrange(MVP_df, desc(overall_score)), 10)
MVP_df_ten

#Graphical representation of the stats showing MVP 
ggplot(MVP_df_ten, aes(x = Last, y = overall_score, fill = "yellow")) + 
  geom_bar(stat = "identity") +
  labs(x = "Last Name")



