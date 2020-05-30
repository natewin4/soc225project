# soc225project
final project

# Graph 1- Comparing Total Hospital Beds by State
covid <- read.csv("data/HRRScorecard.csv")

covid %>% as_data_frame() #recoding a variable
df = str_split(covid$HRR, pattern = ",", simplify = T) %>% as_data_frame()
COVID <- cbind(covid, df)%>%
 rename(City = V1,
         State = V2)
  

hospital <- COVID %>%
  select(State, Total.Hospital.Beds)
  

library(dplyr)
df <- hospital %>% 
  group_by(State) %>% 
  summarise(Total.beds = sum(as.numeric(Total.Hospital.Beds), na.rm = FALSE)) #creating a numeric variable
  
us_hospitals <- df[-c(1),]


us_hospitals %>%
  ggplot(aes(x=State, y=Total.beds))+
  geom_col()+
  labs(title= "Total Number of Hospital Beds by State")+
  ylab("Total Number of Beds")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Graph 2- Comparing Total Cases to Total Beds
  library(tidyverse)
uscovid <- read.csv("data/USCOVID.csv")

numbers <- uscovid %>%
  select(state, cases)

library(tidyverse)

covid_19 <- numbers %>%  #creating a character variable
  mutate(state = as.character(state))


cases <- aggregate(covid_19$cases, by=list(state=covid_19$state), FUN=sum)

cases$state <- gsub("Alabama", "AL", cases$state)
cases$state <- gsub("Alaska", "AK", cases$state)
cases$state <- gsub("Arizona", "AZ", cases$state)
cases$state <- gsub("Arkansas", "AR", cases$state)
cases$state <- gsub("California", "CA", cases$state)
cases$state <- gsub("Colorado", "CO", cases$state)
cases$state <- gsub("Connecticut", "CT", cases$state)
cases$state <- gsub("Delaware", "DE", cases$state)
cases$state <- gsub("Florida", "FL", cases$state)
cases$state <- gsub("Georgia", "GA", cases$state)
cases$state <- gsub("Hawaii", "HI", cases$state)
cases$state <- gsub("Idaho", "ID", cases$state)
cases$state <- gsub("Illinois", "IL", cases$state)
cases$state <- gsub("Indiana", "IN", cases$state)
cases$state <- gsub("Iowa", "IA", cases$state)
cases$state <- gsub("Kansas", "KS", cases$state)
cases$state <- gsub("Kentucky", "KY", cases$state)
cases$state <- gsub("Louisiana", "LA", cases$state)
cases$state <- gsub("Maine", "ME", cases$state)
cases$state <- gsub("Maryland", "MD", cases$state)
cases$state <- gsub("Massachusetts", "MA", cases$state)
cases$state <- gsub("Michigan", "MI", cases$state)
cases$state <- gsub("Minnesota", "MN", cases$state)
cases$state <- gsub("Mississippi", "MS", cases$state)
cases$state <- gsub("Missouri", "MO", cases$state)
cases$state <- gsub("Montana", "MT", cases$state)
cases$state <- gsub("Nebraska", "NE", cases$state)
cases$state <- gsub("Nevada", "NV", cases$state)
cases$state <- gsub("New Hampshire", "NH", cases$state)
cases$state <- gsub("New Jersey", "NJ", cases$state)
cases$state <- gsub("New Mexico", "NM", cases$state)
cases$state <- gsub("New York", "NY", cases$state)
cases$state <- gsub("North Carolina", "NC", cases$state)
cases$state <- gsub("North Dakota", "ND", cases$state)
cases$state <- gsub("Ohio", "OH", cases$state)
cases$state <- gsub("Oklahoma", "OK", cases$state)
cases$state <- gsub("Oregon", "OR", cases$state)
cases$state <- gsub("Pennsylvania", "PA", cases$state)
cases$state <- gsub("Rhode Island", "RI", cases$state)
cases$state <- gsub("South Carolina", "SC", cases$state)
cases$state <- gsub("South Dakota", "SD", cases$state)
cases$state <- gsub("Tennessee", "TN", cases$state)
cases$state <- gsub("Texas", "TX", cases$state)
cases$state <- gsub("Utah", "UT", cases$state)
cases$state <- gsub("Vermont", "VT", cases$state)
cases$state <- gsub("Virginia", "VA", cases$state)
cases$state <- gsub("Washington", "WA", cases$state)
cases$state <- gsub("West Virginia", "WV", cases$state)
cases$state <- gsub("Wisconsin", "WI", cases$state)
cases$state <- gsub("Wyoming", "WY", cases$state)
cases$state <- gsub("District of Columbia", "DC", cases$state)
cases$state <- gsub("West VA", "WV", cases$state)

us_cases <- cases[-c(12, 37, 42, 50, 51), ] 
us_cases <- us_cases[order(us_cases$state),]

US_cases <- us_cases %>%
  dplyr::rename(total_cases = x)

library(dplyr)
us_beds <- us_hospitals %>%
  dplyr::rename(state = State)


total <- cbind(US_cases, us_beds) #joining two data sets
total2 <- total[,-3]
  
  #graph 2
total2 %>%
  ggplot(aes(x = total_cases, y=Total.beds))+
  geom_point(alpha = 0.5)+
  xlab("Total Number of Cases per State")+
  ylab("Total Beds per State")+
  labs(title = "COVID-19 in the U.S.: Total Hospital Beds vs. Total Cases")
