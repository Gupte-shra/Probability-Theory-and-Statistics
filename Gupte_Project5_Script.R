library(janitor)
library(pacman)
library(lubridate)
library(tidyverse)
library(dplyr)

# USING THE BALL DATA SET 

# Question 1
Ball_data <- read.csv('/Users/shraddha/Desktop/MPSA - SHRADDHA GUPTE/Introduction to Analytics ALY6000/Module 5/Gupte_Project5_Script/ball-dataset.csv')
Ball_data

# Question 2
freq_color <- Ball_data %>% group_by(color) %>% summarise(label_count= n())
freq_color
 
# OR

freq_color <- table(Ball_data$color)
freq_color

# Question 3 
freq_label <- Ball_data %>% group_by(label) %>% summarise(color_count= n())
freq_label

# OR

freq_label <- table(Ball_data$label)
freq_label

#Question 4
library(ggplot2)
freq_color <- Ball_data %>% group_by(color) %>% summarise(label_count= n())
freq_color
ggplot(freq_color, aes(x = color, y = label_count, fill = color)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Color Counts of Balls", x = "Color", y = "Count") + 
scale_fill_manual(values = c("blue" = "blue", "green" = "green", "red" = "red", "yellow" = "yellow"))

# Question 5
freq_label <- Ball_data %>% group_by(label) %>% summarise(color_count= n())
freq_label 
ggplot(freq_label, aes(x = label, y = color_count, fill = label)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Label Counts of Balls", x = "Label", y = "Count")

###
count_table<- Ball_data %>% group_by(color, label) %>% summarise(label_count= n())
count_table

#Question 6
prob6_result <- sum(Ball_data$color == "green") / nrow(Ball_data)
prob6_result

#Question 7 
prob7_result <- sum(Ball_data$color == "blue" | Ball_data$color == "red") / nrow(Ball_data)
prob7_result

# Question 8
prob8_result <- sum(Ball_data$label == "A" | Ball_data$label == "C") / nrow(Ball_data)
prob8_result

# Question 9
prob9_result <- sum(Ball_data$label == "D" & Ball_data$color == "yellow") / nrow(Ball_data)
prob9_result

# Question 10
prob10_result <- sum(Ball_data$label == "D" | Ball_data$color == "yellow") / nrow(Ball_data)
prob10_result

# Question 11
prob11_blue <- sum(Ball_data$color == "blue") / nrow(Ball_data)
prob11_blue 
prob11_red <- sum(Ball_data$color == "red") / (nrow(Ball_data)-1)
prob11_red
prob11_result <- prob11_blue * prob11_red
prob11_result

# Question 12 
prob12_result <- (sum(Ball_data$color == "green") / nrow(Ball_data))*
  sum(Ball_data$color == "green") / (nrow(Ball_data)-1)*
  sum(Ball_data$color == "green") / (nrow(Ball_data)-2)*
  sum(Ball_data$color == "green") / (nrow(Ball_data)-3)
prob12_result

# Question 13
prob13_result <- sum(Ball_data$color == "red") / nrow(Ball_data) * sum(Ball_data$label == "B") / (nrow(Ball_data)-1)
prob13_result

# Question 14
my_factorial <- function(n) {if (n == 0) {return(1)} else {return(n * my_factorial(n - 1))}}
my_factorial(0)
my_factorial(3)
my_factorial(5)

# CREATING A COIN FLIPPING DATA FRAME 

# Question 15
library(tibble)
outcomes <- expand.grid(c("H", "T"), c("H", "T"), c("H", "T"), c("H", "T"))
colnames(outcomes) <- c("first", "second", "third", "fourth")
coin_outcomes <- as.tibble(outcomes)
coin_outcomes

# Question 16
coin_outcomes$Probability <- apply(coin_outcomes, 1, function(row) prod(ifelse(row == "H", 0.6, 0.4)))
coin_outcomes

# Question 17 
coin_outcomes$Headcount <- rowSums(coin_outcomes == "H")
coin_outcomes
num_heads_prob <- table(coin_outcomes$Headcount) / nrow(coin_outcomes)
num_heads_prob

# Question 18 

p_head <- 0.6
p_tail <- 0.4
prob18_result <- 4 * p_head^3 * p_tail
print(prob18_result)

# Question 19 

filtered_data <- subset(coin_outcomes, Headcount == 2 | Headcount == 4)
prob19_result <- sum(filtered_data$Probability)
prob19_result

# Question 20 

filtered_data <- subset(coin_outcomes, Headcount <= 3)
prob20_result <- sum(filtered_data$Probability)
prob20_result

# Question 21
outcome_probabilities <- aggregate(Probability ~ Headcount, data = coin_outcomes, FUN = sum)
ggplot(outcome_probabilities, aes(x = Headcount, y = Probability)) +
geom_bar(stat = "identity", fill = "cyan",) + labs(title = "Probability Distribution of head with 4 flips",x = "Number of Heads",y = "Probability")


# SOCCER GAMES

# Question 22
prob_win_home <- (75/100)*(75/100)*(75/100)*(75/100)*(75/100)
prob_win_home
prob_win_away <-  (50/100)*(50/100)*(50/100)*(50/100)*(50/100)
prob_win_away
prob22_result <- prob_win_home*prob_win_away
prob22_result

#Question 23
n <- 10
p_home <- 0.75
p_away <- 0.50
prob22_result <- choose(n, 10) * p_home^5 * p_away^5
print(prob22_result)
prob_zero_games <- p_home^0 * (1 - p_home)^5 * p_away^0 * (1 - p_away)^5
prob_one_game <- choose(5, 1) * p_home^1 * (1 - p_home)^4 * p_away^0 * (1 - p_away)^5 +
  choose(5, 1) * p_home^0 * (1 - p_home)^5 * p_away^1 * (1 - p_away)^4
prob_at_most_one_game <- prob_zero_games + prob_one_game
prob23_result <- 1 - prob_at_most_one_game
print(prob23_result)

#Question 24
prob24_result <- choose(5, 3) * choose(5, 2)
prob24_result
