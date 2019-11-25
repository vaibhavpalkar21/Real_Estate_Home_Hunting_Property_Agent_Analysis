## Business Analyst ROle Assignment for Squad

## Problem statement

# A Real estate agent works under a licensed broker (or brokerage). 
# The brokerage provides the real estate agent with leads (leads are people who are looking to buy, 
# sell or rent), and the agent's job is to help them in their home buying, selling or renting process. 
# They do this by making calls and regularly following-up with them.

## Business understanding

# Find the parameters which help agent to improve to him to help leads which are provided by the broker
# Agent used to follow this process by making call to each lead and puts note in CRM

#########---------------------------------------###########

# Data collection # Data cleaning # Data Preparation

#########---------------------------------------###########

# We have the BA_Call_Data and its metadata

# required libraries
options(max.print=100000)
library(lubridate)
library(ggplot2)
library(dplyr)

# Read data
BA_Call_Data <- read.csv("BA_Calls_data.csv", header = T, stringsAsFactors = F)
str(BA_Call_Data)
# Lets check the summary of BA_call_data to get idea about data
summary(BA_Call_Data)

# lets check NA values
sum(is.na(BA_Call_Data)) # 0 blak values

# lets check blank values
blank_values <- sapply(BA_Call_Data, function(x) length(which(x == "")))
blank_values # only outcome variable has 155175 blank vlues

# lets check duplicate values in data
duplicate_values <- BA_Call_Data[duplicated(BA_Call_Data), ]
duplicate_values # No duplicate values

# 
table(BA_Call_Data$outcome)

# Lets check for timestamp variable and convert it to r date format
# lets convert created date time to YYYY-m-d HMS format
BA_Call_Data$created <- parse_date_time(BA_Call_Data$created, "Ymd HMS", tz = "UTC", truncated = 3)

# # lets convert created_at date time to YYYY-m-d HMS format
BA_Call_Data$created_at <- gsub('.{6}$', '', BA_Call_Data$created_at)
BA_Call_Data$created_at <- parse_date_time(BA_Call_Data$created_at, "Ymd HMS", tz = "UTC", truncated = 3)

# By exploring data set it is confirm that variable 'created' (timezone UTC) and 'created_at' (timezone CST)
# is same but CST time is ahead of UTC by 5 and 6 hr
# we will use the CST time zone for our analysis

# lets convert lead_created_at date time to YYYY-m-d HMS format 
BA_Call_Data$lead_created_at <- gsub('.{6}$', '', BA_Call_Data$lead_created_at)

BA_Call_Data$lead_created_at <- parse_date_time(BA_Call_Data$lead_created_at, "Ymd HMS", tz = "UTC", truncated = 3)
typeof(BA_Call_Data$created_at)

# Lets check the unique levels in id, userid, createdbyid and personId
unique_id <- unique(BA_Call_Data$id)
length(unique_id) # there are 180488 unique call Id's

unique_createdById <- unique(BA_Call_Data$createdById)
length(unique_createdById) # there are 167 unique createdById which are agents who created this call

unique_userId <- unique(BA_Call_Data$userId)
length(unique_userId) # there are 169 unique userId which are agents who made call

unique_personid <- unique(BA_Call_Data$personId)
length(unique_personid) # there are 28004 unique person id which are leads

# CreatedByID and userID are same 
# so lets check for difference in them, created by id's which are not in user id's
sum(BA_Call_Data$createdById!=BA_Call_Data$userId) 
# ther are 70574 id's which are not available in them
# most of them are -1 values which is erronous data

# Lets check -1 id for both columns userid and createdbyid (which is erronous data)
# user id cant be -1 so we have to treat it as well
BA_Call_Data_1 <- BA_Call_Data[which(BA_Call_Data$createdById == -1 & BA_Call_Data$userId == -1),]
nrow(BA_Call_Data_1) # there are 20 id's as -1 in both userid and createdbyid columns

View(BA_Call_Data_1)
# here most of the calls happened by leads and outcome is No answer

# lets remove those 20 rows as it is not useful
BA_Call_Data <- BA_Call_Data[-which(BA_Call_Data$createdById == -1 & BA_Call_Data$userId == -1),]

# lets check -1 id'd for column createdById
nrow(BA_Call_Data[which(BA_Call_Data$createdById == -1), ]) # 70574 id's with -1 value

# lets check -1 id'd for column userid
nrow(BA_Call_Data[which(BA_Call_Data$userId == -1), ]) # 0 id's with -1 value

# lets check the values which are different in both columns
sum(BA_Call_Data$createdById!=BA_Call_Data$userId) # 70574 it is clear that it is with id -1

# so by analysing both columns it is clear that both columns have same user id and both should be same
# but in createdbyid column has -1 values which should be replace
# now lets make both column as same user id's
BA_Call_Data$createdById <- BA_Call_Data$userId # now we have same user id's in both column

# lest check -1 id'd for column createdById
nrow(BA_Call_Data[which(BA_Call_Data$createdById == -1), ]) # 0 id's with -1 value

# lets check for duration column
summary(BA_Call_Data$duration) # min - 0 and max - 63841 call duration

length(levels(as.factor(BA_Call_Data$duration))) # there are total 1720 call entries

# lets check for isincoming variable 
table(BA_Call_Data$isIncoming)
# False - 174138 (most of time agent has called), TRUE - 6330(sometime lead has also called)

# lets check for outcome variable 
table(BA_Call_Data$outcome)
# blank - 155174, bad number - 2464, interested - 1, message - 8206, no answer - 14623

# lets check for left a note column (if note is there by agent)
table(BA_Call_Data$left_a_note)
# 0 - 11348, 1 - 169120
# left note percentage
169120/(169120+11348)*100 # for 93.71% people agent has left note

#########---------------------------------------###########

# Feature Engineering and Data Analysis

#########---------------------------------------###########

# split date and time into 2 column for create date and time
BA_Call_Data$created_date <- format(BA_Call_Data$created_at, "%Y-%m-%d")
typeof(BA_Call_Data$created_date)
BA_Call_Data$created_time <- format(BA_Call_Data$created_at, "%H:%M:%S")
typeof(BA_Call_Data$created_time)

# lets extract the day and the hour form separated time data for created_at
BA_Call_Data$created_time_hr <- format(strptime(BA_Call_Data$created_time,"%H:%M:%S"),'%H')
BA_Call_Data$created_time_hr <- as.numeric(BA_Call_Data$created_time_hr)
typeof(BA_Call_Data$created_time_hr)

# extract day
BA_Call_Data$created_date_day <- weekdays(as.Date(BA_Call_Data$created_date))

# split date and time into 2 column for lead created at date and time

# lets extract the day and the hour form separated time data for lead_created_at
# for lead_created_at 
BA_Call_Data$lead_created_at_date <- format(BA_Call_Data$lead_created_at, "%Y-%m-%d")

BA_Call_Data$lead_created_at_time <-  format(BA_Call_Data$lead_created_at, "%H:%M:%S")

# lets extract the day and the hour form separated time data for created_at
BA_Call_Data$lead_created_at_time_hr <- format(strptime(BA_Call_Data$lead_created_at_time,"%H:%M:%S"),'%H')
BA_Call_Data$lead_created_at_time_hr <- as.numeric(BA_Call_Data$lead_created_at_time_hr)
typeof(BA_Call_Data$lead_created_at_time_hr)

# extract day
BA_Call_Data$lead_created_at_time_day <- weekdays(as.Date(BA_Call_Data$lead_created_at_date))

# Create time slots for created_at and for lead_created_at time
BA_Call_Data$created_at_timeslot <- sapply(BA_Call_Data$created_time_hr, function(x) if(x >= 4 & x <= 8) {print("early morning")} 
                                           else if (x >= 8 & x <= 12) {print("morning")}
                                           else if(x >= 12 & x <= 16) {print("afternoon")}
                                           else if(x >= 16 & x <= 20) {print("evening")}
                                           else if(x >= 20 & x <= 24) {print("night")}
                                           else {print("late night")})


BA_Call_Data$lead_created_at_timeslot <- sapply(BA_Call_Data$lead_created_at_time_hr, function(x) if(x >= 4 & x <= 8) {print("early morning")} 
                                                else if (x >= 8 & x <= 12) {print("morning")}
                                                else if(x >= 12 & x <= 16) {print("afternoon")}
                                                else if(x >= 16 & x <= 20) {print("evening")}
                                                else if(x >= 20 & x <= 24) {print("night")}
                                                else {print("late night")})

BA_Call_Data$call_time_diffrence <- difftime(BA_Call_Data$created_at, BA_Call_Data$lead_created_at, units = "hours")

# write csv file for final dataset
BA_Call_Data_final <- BA_Call_Data
write.csv(BA_Call_Data_final,"BA_Call_Data_final.csv")

BA_Call_Data_final <- read.csv("BA_Call_Data_final.csv", header = T, stringsAsFactors = F)

#######################

## EXPLORATORY ANALYSIS

# lets plot data and find pattern from it
# for isincoming call identifier

ggplot(BA_Call_Data_final, aes(x=BA_Call_Data_final$isIncoming)) + geom_bar(fill = 'red') + 
  labs(x="isIncoming", y="count") + ggtitle("isIncoming Vs total count") +
  geom_text(stat='count', aes(label=..count..), vjust=0)
# True menas lead calls and False means agent call, here most of the call are happened from agents
# only around 3% (6330) leads have call personaly and almost 97%(174138) called by agent

# plot for outcome 
ggplot(BA_Call_Data_final, aes(x=BA_Call_Data_final$outcome)) + geom_bar(fill = "blue") + geom_text(stat = 'count', aes(label=..count..), vjust=0.5) +
  labs(x="outcome", y="count") + ggtitle("outcome vs count") + theme(axis.text.x = element_text(face="bold", size=5, angle=45))

# here it is clear that for most of the count outcome entry is blank
# 14623 call have not received calls
# 8206 leads have left message
# 2464 are bad customer
# only 1 customer is interested 
# so it is clear that outcome varibale it not reliable

# plot for userid
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$userId)) + geom_histogram(col = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=-3, size=3) + 
  labs(x="userId", y="count") + ggtitle("userId Vs total count")
# user id between 50 to 55 has more number of agents who has called maximum time

# plot left a note
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$left_a_note)) + geom_bar(fill = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=1) + 
  labs(x="left note", y="count") + ggtitle("left note Vs total count")
# for 169120 calls agent has left a note 
# for 11348 calls agent did not left a not
# here we can assume that for leads who are interested or with positive attitude agent has left note

# plot for created date day
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$created_date_day)) + geom_bar(fill = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=1) + 
  labs(x="day", y="count") + ggtitle("day Vs total count")
# it is clear that on monday, tuesday, wednesday, thursday maximum leads are contacted

# plot for created_at_timeslot
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$created_at_timeslot)) + geom_bar(fill = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=1) + 
  labs(x="created time slot", y="count") + ggtitle("created time slot Vs total count")
# most numbers of leads contacted at morning and afternoon

# plot for lead_created_at_time_day
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$lead_created_at_time_day)) + geom_bar(fill = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=1) + 
  labs(x="lead created day", y="count") + ggtitle("lead created day Vs total count")
# most lead created on monday, tuesday, wednesday

# plot for lead_created_at_timeslot
ggplot(BA_Call_Data_final, aes(BA_Call_Data_final$lead_created_at_timeslot)) + geom_bar(fill = 'red') + geom_text(stat = 'count', aes(label=..count..), vjust=1) + 
  labs(x="lead created time slot", y="count") + ggtitle("lead created time slot Vs total count")
# most number of lead created at morning, afternoon, evening


############################

## lets find that how much calls have happen for each customer
Calls_per_leads <- summarise(group_by(BA_Call_Data_final, BA_Call_Data_final$personId), n())
colnames(Calls_per_leads) <- c('personId','call_count')

Calls_per_leads <- Calls_per_leads %>% arrange(desc(call_count))
View(Calls_per_leads)

# top 5 leads by call count
head(Calls_per_leads, 5)
# here are the top 5 leads who has received max calls
# 156299 - 1271, 156301 - 181, 132034 - 170, 130581 - 156, 163356 - 144

# bottom 5 leads by call count
tail(Calls_per_leads, 5)
# here are the bottom 5 leads who has received less calls
# 182678-  1, 182685  - 1, 182690 - 1, 182692 - 1, 182697 - 1

############################

## lets find that how much calls have happen been made by each agent 
Calls_by_agent <- summarise(group_by(BA_Call_Data_final, BA_Call_Data_final$userId), n())
colnames(Calls_by_agent) <- c('agent_id','call_Count')

Calls_by_agent <- Calls_by_agent %>% arrange(desc(call_Count))
View(Calls_by_agent)

# top 5 agents by call count
head(Calls_by_agent, 5)
# here are the top 5 leads who has received max calls
# 53 - 48888, 120 - 18087, 1 - 6940, 151 - 6509, 57 - 4199

# bottom 5 agents by call count
tail(Calls_by_agent, 5)
# here are the top 5 leads who has received less calls
# 79 - 1, 140 - 1, 190 - 1, 142 - 1, 209 - 1

###########################

# lets analyse the call time difference
leads_agent_call_data <- BA_Call_Data_final[c("id", "createdById", "personId", "userId", "isIncoming", "duration", "call_time_diffrence")]

leads_agent_call_data <- leads_agent_call_data %>% arrange(desc(leads_agent_call_data$call_time_diffrence))

# here lets find what is difference between lead generated and call made or lead call then lead created
agent_called_lead <- filter(BA_Call_Data_final, BA_Call_Data_final$isIncoming == "FALSE")

agent_called_lead <- agent_called_lead %>% arrange(desc(agent_called_lead$call_time_diffrence))

agent_called_lead$dayspassed <- (agent_called_lead$call_time_diffrence/24)

agent_called_lead <- agent_called_lead %>% arrange(desc(agent_called_lead$dayspassed))

# lets check for agents who have contacted after 7 days passed
dayspassed_7 <- filter(agent_called_lead, agent_called_lead$dayspassed > 7)

agent_count_dayspassed_7 <- summarise(group_by(dayspassed_7, dayspassed_7$createdById), n())
colnames(agent_count_dayspassed_7) <- c("agent_id", "call_count")

agent_count_dayspassed_7 <- agent_count_dayspassed_7 %>% arrange(desc(agent_count_dayspassed_7$call_count))
View(agent_count_dayspassed_7)
head(agent_count_dayspassed_7, 10)
# those are top 10 agents who took to much time to contact after lead created (days passed 7 or not contacted)
# 53 - 11389, 1 - 6280, 120 -  4548, 57 - 2796, 119 - 2722, 
# 203 - 2653, 72 - 2322, 123 -  2270, 117 - 2024, 149 - 1776

########################

## lets check when lead called then what is the response of the agent
leads_called_themself <- filter(BA_Call_Data_final, BA_Call_Data_final$isIncoming == "TRUE")

leads_called_themself <- leads_called_themself %>% arrange(desc(leads_called_themself$call_time_diffrence))

leads_called_agent_not_received <- filter(leads_called_themself, leads_called_themself$duration == 0)

agents_not_responded_to_leads <- summarise(group_by(leads_called_agent_not_received, leads_called_agent_not_received$createdById), n())

agents_not_responded_to_leads <- agents_not_responded_to_leads %>% arrange(desc(agents_not_responded_to_leads$`n()`))
colnames(agents_not_responded_to_leads) <- c('agent_id', 'call_count')
View(agents_not_responded_to_leads)

barplot(agents_not_responded_to_leads$agent_id, col = 'red', xlab = 'Agent id', ylab = 'call count')
ggplot(agents_not_responded_to_leads, aes(agents_not_responded_to_leads$agent_id, agents_not_responded_to_leads$call_count)) + geom_line()

# those are the agents who is responsible for not calling back to leads with how many time they have ignore
agents_not_responded_to_leads$agent_id
# 53, 120, 151, 57, 203, 174, 121, 118, 72, 44, 142, 56, 
# 117, 19, 59, 1, 39, 84, 147, 113, 134, 112, 148, 155

######################
# lets check when agent has called immediately (best performing)
agent_conatcted_immediately <- BA_Call_Data_final
agent_conatcted_immediately$contact_time <- round((agent_conatcted_immediately$call_time_diffrence)/24, 3)

agent_conatcted_immediately <- agent_conatcted_immediately[order(agent_conatcted_immediately$contact_time),]

agent_conatcted_in_same_day <- filter(agent_conatcted_immediately, agent_conatcted_immediately$contact_time >= 0 & agent_conatcted_immediately$contact_time <= 1)

agent_conatcted_quick_performance <- summarise(group_by(agent_conatcted_in_same_day, agent_conatcted_in_same_day$userId), n())
colnames(agent_conatcted_quick_performance) <- c("agent_id", "count")

agent_conatcted_quick_performance <- agent_conatcted_quick_performance %>% arrange(desc(agent_conatcted_quick_performance$count))

head(agent_conatcted_quick_performance, 10)
# those are top 5 agents who has responded quickly
# 53, 120, 151, 84, 72, 57, 95, 119, 127, 159

######################

## lets check data for call time difference when first call made and then lead created
# just for getting idea
contacted_first_then_lead_created <- filter(leads_agent_call_data, leads_agent_call_data$call_time_diffrence <= 0)
table(contacted_first_then_lead_created$isIncoming) # here most of time lead has contacted

contacted_first_then_lead_created$dayspassed <- abs(round((leads_called_first$call_time_diffrence)/24, 3))

contacted_first_agents_call_count <- summarise(group_by(contacted_first_then_lead_created, contacted_first_then_lead_created$createdById),n())

contacted_first_agents_call_count <- contacted_first_agents_call_count %>% arrange(desc(contacted_first_agents_call_count$`n()`))

head(contacted_first_agents_call_count, 6)
# 120 - 53, 151 - 37, 156 - 26, 53 - 16, 72 - 14, 1 - 13

######################

# lets check for agents who had conversation with leads
agent_conversation_data <- filter(BA_Call_Data_final, BA_Call_Data_final$duration > 0)
summary(agent_conversation_data$duration)

agents_call_count <- summarise(group_by(agent_conversation_data, agent_conversation_data$userId), n())
agents_call_count <- agents_call_count %>% arrange(desc(agents_call_count$`n()`))

# based on the call conversation best and worst performing agents
best_performing_agent <- head(agents_call_count, 20)
View(best_performing_agent)
# those are best performing agebts based on call they made and talked to leads
# 53 - 38834, 120 - 13595, 1 - 6894, 151 - 5800, 119 - 2643, 203 - 2503, 117 - 2294, 57 - 2160
# 95 - 2115, 127 - 1754, 72 - 1746, 170 - 1547, 118 - 1233, 155 - 1151, 174 - 1128, 16 - 1095
# 124 - 1088, 87 - 1057, 43 - 968, 125 - 924

worst_performing_agent <- filter(agents_call_count, agents_call_count$`n()` < 10)
View(worst_performing_agent)

# those are worst_performing_agent who had called and talk to lead very few times
# 58 - 9, 148 - 9, 143 - 8, 213 - 8, 47 - 7, 63 - 6, 110 - 6, 122 - 6, 17 - 5, 45 - 5, 
# 67 - 5, 162 - 5, 176 - 5, 208 - 5, 219 - 5, 212 - 4, 223 - 4, 206 - 3, 93 - 2, 96 - 2
# 103 - 2, 132 - 2, 137 - 2, 160 - 2, 178 - 2, 49 - 1, 79 - 1, 83 - 1, 101 - 1, 106 - 1, 
# 123 - 1, 133 - 1, 144 - 1, 165 - 1, 182 - 1, 190 - 1, 214 - 1, 222 - 1

####################
# lets check based on calls, what is the average time agent has spend per user

avg_call_per_per_agent <- aggregate(agent_conversation_data$duration, list(agent_conversation_data$userId), mean)
colnames(avg_call_per_per_agent) <- c("agent_id", "avg_duration(sec)")

avg_call_per_per_agent <- avg_call_per_per_agent %>% arrange(desc(avg_call_per_per_agent$avg_duration))

# lets check perorance of agents who has max and min average time per customer
head(avg_call_per_per_agent, 10)
# 144 - 828.0000, 122 - 732.1667, 214 - 478.0000, 17 - 460.2000, 53 - 390.0000, 
# 43 - 343.2924, 195 - 339.7135, 39 - 336.5895, 16 - 335.4110, 103 - 320.5000

tail(avg_call_per_per_agent, 10)
# 110 - 36.0000, 222 - 26.0000, 132 - 18.5000, 53 - 18.1082, 101 - 15.0000
# 137 - 13.0000, 79 - 11.0000, 106 - 10.0000, 190 - 8.0000, 165 - 1.0000

# we can say agent who is continuously following and talking more to leads are performing well

#######################