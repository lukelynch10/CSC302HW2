# Libraries 
library(dplyr)
# problem1
df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                     'Richards','George','Ema','Samantha','Catherine'),
              State=c('Alaska','California','Texas','North Carolina','California','Texas',
                      'Alaska','Texas','North Carolina','Alaska','California','Texas'),
              Sales=c(14,24,31,12,13,7,9,31,18,16,18,14));
aggregate(df1$Sales, by=list(df1$State), FUN=sum);
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales));
#        Group.1  x
#1         Alaska 39
#2     California 55
#3 North Carolina 30
#4          Texas 83

#problem 2
wc_data <- read.csv("WorldCupMatches.csv");
#a
num_rows <- nrow(wc_data);
num_cols <- ncol(wc_data);
print(paste("Rows:", num_rows, "Columns:", num_cols));
#Rows 852, Columns: 20
#b
summary(wc_data);
#c
unique_locations <- length(unique(wc_data$City));
print(paste("Unique locations:", unique_locations));
#151 locations
#d
average_attendance <- mean(wc_data$Attendance, na.rm = TRUE);
print(paste("Average attendance:", average_attendance));
# average attendance was: 45,164
#e
home_goals <- aggregate(wc_data$Home.Team.Goals, by = list(wc_data$Home.Team.Name), FUN = sum, na.rm = TRUE);
colnames(home_goals) <- c("Home Team", "Total Goals");
print(home_goals);
#1543 home goals
#f
attendance_by_year <- wc_data %>% group_by(Year) %>% summarise(avg_attendance = mean(Attendance, na.rm = TRUE));
print(attendance_by_year);
#average by year: 
#1930	32,808.28
#1934	21,352.94
#1938	20,872.22
#1950	47,511.18
#1954	29,561.81
#1958	23,423.14
#1962	27,911.63
#1966	48,847.97
#1970	50,124.22
#1974	49,098.76
#1978	40,678.71
#1982	40,571.60
#1986	46,039.06
#1990	48,388.75
#1994	68,991.12
#1998	43,517.19
#2002	42,268.70
#2006	52,491.23
#2010	49,669.63
#2014	55,374.91
# the trend has been sporadic over the years 

# problem 3
metabolite_data <- read.csv("metabolite.csv")

# a
alzheimers_count <- sum(metabolite_data$Condition == "Alzheimers", na.rm = TRUE)
View(paste("Alzheimer's patients:", alzheimers_count))

# b
missing_values <- colSums(is.na(metabolite_data))
View(missing_values)

# c
cleaned_data <- metabolite_data[!is.na(metabolite_data$Dopamine), ]
View(paste("Rows after removing missing Dopamine values:", nrow(cleaned_data)))

# d
median_c4 <- median(cleaned_data$c4.OH.Pro, na.rm = TRUE)
cleaned_data$c4.OH.Pro[is.na(cleaned_data$c4.OH.Pro)] <- median_c4
View(paste("Median c4-OH-Pro used for replacement:", median_c4))







