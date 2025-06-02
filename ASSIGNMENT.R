library(dplyr)
library(ggplot2)
library(kableExtra)
getwd()
setwd("/Users/zoyaadnan/Documents/analytics/assignment-zoyaa19")
getwd()
##working directory set

##TASK 1
##read data
users<-read.csv("users.csv")
reviews<-read.csv("reviews.csv")
##check/look at data
head(users)
##Take members_since column and turn into dates
users$member_since<-as.Date(users$member_since)
#check/look at data
head(users$member_since)

##create type of user column 
users<-users%>%
  mutate(typeOfUser=ifelse(member_since<as.Date("2017-01-01"), "Veteran",
       ##Check if user joined before 2017 is true                  
                    ifelse(member_since<as.Date("2023-01-01"), 
                           "Intermediate", "New")
       ##Otherwise, check if user joined before 2023 is true
                    ))   
##check if column has been added
head(users)

reviewUsers<-left_join(reviews,users, by="user_id")

##make a summary table
summaryTable<-reviewUsers%>% group_by(typeOfUser)%>%
  summarise(
    userCount=n(),##count number of users
    avgRevStars=mean(average_stars, na.rm=TRUE),##calculate mean of average_stars
    avgRev=mean(review_count, na.rm=TRUE) ##calculate mean of review_count
            )
##check summary table
head(summaryTable)        
##create a summary table of data using kable
summaryTable%>% kable()

##create visualization of data
ggplot(summaryTable)+
  ##create column graph visualization
  geom_col(aes(x=typeOfUser, y=avgRevStars, fill=typeOfUser)) +
  labs(title="Average Review Stars by User Age Group", x="Type of User", 
       y="Average Review Stars")+
  ##y-axis should go up by 0.5
  scale_y_continuous(breaks = seq(0,5, by=0.5))


