rm(list=ls())

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c( "mlr","randomForest","caTools","rpart","ggthemes","DataExplorer","forcats",
             "dplyr","ggplot2","plyr","ggthemes","grid",
             "rpart.plot","RcmdrMisc", "gridExtra","factoextra","FactoMineR")
check.packages(packages)



setwd("C:/Project2")

data <- read.csv("Absenteeism_at_work_Project.csv", na.strings = c('NA'))

#Write function for Mode
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}




#Preprocess data['Reason for absence'] variable ; impute it with number 26; 
#26 level is unjustified reason so We can merge these row levels

sum(is.na(data$Reason.for.absence))
sort(unique(data$Reason.for.absence))

data$Reason.for.absence =ifelse(is.na(data$Reason.for.absence), 26, data$Reason.for.absence)

sum(is.na(data$Reason.for.absence))

##Preprocess data$Month of absence variable , Impute it with Mode
class(data$Month.of.absence)


sort(unique(data$Month.of.absence))

data<- data[!(data$Month.of.absence==0),]

data$Month.of.absence =ifelse(is.na(data$Month.of.absence),Mode(data$Month.of.absence), data$Month.of.absence)

sort(unique(data$Month.of.absence))

sum(is.na(data$Month.of.absence))

data$Month.of.absence<-as.integer(data$Month.of.absence)
class(data$Month.of.absence)


#Preprocess data['Transportation expense '] variable ; Drop the missing entries since 

sum(is.na(data$Transportation.expense)) #Only 8 records

data<-  data[!is.na(data$Transportation.expense),]


#Preprocess data['Distance from Residence to Work'] variable ; Impute it with mean

sort(unique(data$Distance.from.Residence.to.Work))

data$Distance.from.Residence.to.Work =ifelse(is.na(data$Distance.from.Residence.to.Work), mean(data$Distance.from.Residence.to.Work,na.rm=TRUE), data$Distance.from.Residence.to.Work)

sum(is.na(data$Distance.from.Residence.to.Work))

#Preprocess data['Service time'] variable ; Impute it with mean
sort(unique(data$Service.time))

data$Service.time =ifelse(is.na(data$Service.time), mean(data$Service.time,na.rm=TRUE), data$Service.time)

sum(is.na(data$Service.time))

#Preprocess data['Age'] variable ; Impute it with mean
sort(unique(data$Age))

data$Age =ifelse(is.na(data$Age), mean(data$Age,na.rm=TRUE), data$Age)

sum(is.na(data$Age))

#Preprocess data['Work load Average/day'] variable 
# Impute it with mean; Will look at this later

sort(unique(data$Work.load.Average.day))
class(data$Work.load.Average.day)

###pre Process Work.load.Average.day ; Since only one entry so better drop it
data$Work.load.Average.day <- gsub("\\,", "", data$Work.load.Average.day) # replace comma with dots
data$Work.load.Average.day <- as.numeric(as.character(data$Work.load.Average.day ))

sum(is.na(data$Work.load.Average.day))

data<- data[!(is.na(data$Work.load.Average.day)),]

sum(is.na(data$Work.load.Average.day))



##Hit target,Impute with mean

sort(unique(data$Hit.target))

data$Hit.target =ifelse(is.na(data$Hit.target), mean(data$Hit.target,na.rm=TRUE), data$Hit.target)

sum(is.na(data$Hit.target))

##Disciplinary failure  ; Impute it with Mode
sort(unique(data$Disciplinary.failure))


data$Disciplinary.failure =ifelse(is.na(data$Disciplinary.failure), Mode(data$Disciplinary.failure), data$Disciplinary.failure)

sum(is.na(data$Disciplinary.failure))


#Impute Education with Mode

sort(unique(data$Education))

data$Education =ifelse(is.na(data$Education), Mode(data$Education), data$Education)

sum(is.na(data$Education))

##Son;Impute with Mode
sort(unique(data$Son))

data$Son =ifelse(is.na(data$Son), Mode(data$Son), data$Son)

sum(is.na(data$Son))


##Social drinker;Impute with Mode


sort(unique(data$Social.drinker))

data$Social.drinker =ifelse(is.na(data$Social.drinker), Mode(data$Social.drinker), data$Social.drinker)

sum(is.na(data$Social.drinker))

##Social smoker;Impute with Mode

sort(unique(data$Social.smoker))

data$Social.smoker =ifelse(is.na(data$Social.smoker), Mode(data$Social.smoker), data$Social.smoker)

sum(is.na(data$Social.smoker))

#Pet;Impute with Mode

sort(unique(data$Pet))

data$Pet =ifelse(is.na(data$Pet), Mode(data$Pet), data$Pet)

sum(is.na(data$Pet))

#Weight;impute with mean

sort(unique(data$Weight))

data$Weight =ifelse(is.na(data$Weight), mean(data$Weight,na.rm=TRUE), data$Weight)

sum(is.na(data$Weight))

#Height ; Impute with mean

sort(unique(data$Height))

data$Height =ifelse(is.na(data$Height), mean(data$Height,na.rm=TRUE), data$Height)

sum(is.na(data$Height))

#Body mass index  ;Impute with mean

sort(unique(data$Body.mass.index))

data$Body.mass.index =ifelse(is.na(data$Body.mass.index), mean(data$Body.mass.index,na.rm=TRUE), data$Body.mass.index)

sum(is.na(data$Body.mass.index))


#Absenteeism time in hours

sort(unique(data$Absenteeism.time.in.hours))

data$Absenteeism.time.in.hours =ifelse(is.na(data$Absenteeism.time.in.hours), mean(data$Absenteeism.time.in.hours,na.rm=TRUE), data$Absenteeism.time.in.hours)

sum(is.na(data$Absenteeism.time.in.hours))

##################### To calculate Workloss
###Generate anew variable called Workloss in terms of Work.load.Average.day & Absenteeism.time.in.hours
data$workloss <- ((data$Work.load.Average.day) /8) * (data$Absenteeism.time.in.hours)

d<-data ## Take backup of data

# Basic statistics and data preparation-
#   Factors are in interger format , so for the sake of analysis I have changed them to factor format.

str(d)

summary(d)

# converting variables to factors-

col <- c(2:5,12:17)

da <- d


da[col] <- lapply(da[col], factor)


summary(da)
summarizeColumns(da)

##Filter out Absenteeism.time.in.hours which are more than 8 Hours since , assuming 
#Office working hours is 8 Hours
da<-da %>% 
  filter(Absenteeism.time.in.hours >0 & Absenteeism.time.in.hours <=8)


data_New <- da



da <- da %>%
  mutate(Reason.for.absence = fct_recode(Reason.for.absence,'infectious,parasitic diseases'='0',
                                         'Certain infectious and parasitic diseases'='1','Neoplasms'='2','Diseases of the blood and blood-forming organs and certain disorders involving the
                                         immune mechanism'='3','Endocrine, nutritional and metabolic diseases'='4', 
                                         'Mental and behavioural disorders'='5','Diseases of the nervous system'='6','Diseases of the eye and adnexa'='7',
                                         'Diseases of the ear and mastoid process'='8','Diseases of the circulatory system'='9','Diseases of the respiratory system'='10', 
                                         'Diseases of the digestive system'='11','Diseases of the skin and subcutaneous tissue'='12', 
                                         'Diseases of the musculoskeletal system and connective tissue'='13','Diseases of the genitourinary system'='14','Pregnancy, childbirth and the puerperium'='15', 
                                         'Certain conditions originating in the perinatal period'= '16','Congenital malformations, deformations and chromosomal abnormalities'='17',
                                         'Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere
                                         classified'= '18', 'Injury, poisoning and certain other consequences of external causes'='19','External causes of morbidity and mortality'='20',
                                         'Factors influencing health status and contact with health services'='21','patient follow-up'='22','medical consultation'='23','blood donation'='24',
                                         'laboratory examination'='25','unjustified absence'='26','physiotherapy'='27','dental consultation'='28'))

unique(da$Reason.for.absence)


da <- da %>%
  mutate(Month.of.absence= fct_recode(Month.of.absence,'None'='0','Jan'='1','Feb'='2','Mar'='3','Apr'='4','May'='5',
                                      'Jun'='6','Jul'='7','Aug'='8','Sep'='9','Oct'='10','Nov'='11','Dec'='12') )
da <- da %>%
  mutate(Seasons= fct_recode(Seasons,'summer'='1','autumn'='2','winter'='3','spring'='4'))

da <- da %>%
  mutate(Education = fct_recode(Education,'highschool'='1','graduate'='2','postgraduate'='3','master& doctrate'='4'))

da <- da %>%
  mutate(Disciplinary.failure= fct_recode(Disciplinary.failure,'No'='0','Yes'='1'))
da <- da %>%
  mutate(Social.drinker= fct_recode(Social.drinker,'No'='0','Yes'='1'))
da <- da %>%
  mutate(Social.smoker= fct_recode(Social.smoker,'No'='0','Yes'='1'))
da <- da %>%
  mutate(Day.of.the.week = fct_recode(Day.of.the.week,"Monday"="2","Tuesday"="3","Wednesday"="4","Thursday"="5","Friday"="6"))


d1<-da

da<-da %>% 
  filter(Absenteeism.time.in.hours >0 & Absenteeism.time.in.hours <=8)


max(da$Absenteeism.time.in.hours)

min(da$Absenteeism.time.in.hours)



detach("package:plyr", unload=TRUE)


##############Data exploration-
# To arrange multiple ggplot2 graphs on the same page, the standard R functions - par() and layout() - cannot be used.
# 
# The basic solution is to use the gridExtra R package, which comes with the following functions:
#   
#   grid.arrange() and arrangeGrob() to arrange multiple ggplots on one page
# marrangeGrob() for arranging multiple ggplots over multiple pages.

p <- ggplot(da, aes(x = Pet, fill = Pet)) + geom_bar()

s <- ggplot(da, aes(x = Son, fill = Son)) + geom_bar()


SS <- ggplot(da, aes(x =  Social.smoker, fill =  Social.drinker)) + geom_bar()

Day <- ggplot(da, aes(x = Day.of.the.week, fill =  Day.of.the.week)) + geom_bar() 
S <- ggplot(da, aes(x =   Seasons,fill = Seasons)) + geom_bar()

grid.arrange(p,s, nrow = 2)

grid.arrange(SS,S, nrow = 2)


grid.arrange(Day, nrow = 1)


# Some more digging into the data-
# I have taken those data that consists of Absenteesim in hours that are 
#relavent to the analysis.
# The 649 obs were found to have absent with respect to total of 740 obs.
# 
# Here the proportion of elements of categorical variables that contribute to the
#target variable.
# I have taken only certain variable that I thought would come in to process.
# 
absent <- as.data.frame( da %>% select(everything()) %>% filter(Absenteeism.time.in.hours > 0))

season1 <- as.data.frame(absent %>% group_by(Seasons) %>% summarise(count= n(),
                                                                    percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

reorder(season1$Seasons,season1$percent) #Quick check

ggplot(season1,aes(x= reorder(Seasons,percent), y= percent, fill = Seasons)) +
  geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Seasons')

# The attribute disciplinary failure is taken into consideration and it was 
#found it had no obvious part on target variable

Disciplinary.failure <-as.data.frame(absent %>% group_by(Disciplinary.failure) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

ggplot(Disciplinary.failure,aes(x= reorder(Disciplinary.failure,percent), y= percent, fill = Disciplinary.failure)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Disciplinary failure')




# Here the various types of reasons for absence attribute is analysed -
#   NOTE: The top four of them cover 50% of the resons for absence
# * medical consultation - 22.6 %
# * dental consultation - 16.9%
# * physiotherapy - 10.8 %
# * Diseases of the musculoskeletal system and connective tissue - 6.2%
# * Unjustified absence - 5.4 %
# * Patient follow up - 5.4%
# * Laboratory Examination - 4.9%
# * Injuriey , posioning and certain other consequences of external causes - 4 %
hist(da$Absenteeism.time.in.hours)

Reason <-  as.data.frame(absent %>% group_by(Reason.for.absence) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(Reason,aes(x = reorder(Reason.for.absence,percent), y= percent, fill= Reason.for.absence)) +
  geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')

# Close to the half of employees drink alcohol(320/420),so the attempted analysis can be 
# taken into consideration that the it can be a element that influence the target variable.

ggplot(absent,aes(x= Age,y= Absenteeism.time.in.hours,fill= Social.drinker)) +
  geom_bar(stat='identity',position= position_dodge()) + 
  scale_x_continuous(breaks =c(seq(20,60,5)),limits=c(20,60))

# Service time across hit target

ggplot(absent,aes(x= Service.time,y= Hit.target)) + geom_point()+ geom_smooth(method = 'lm') + 
  ggtitle('Analysis of Hit.target across Service time') + xlab('Service time(years)') + ylab('Hit target(%)')



# Here trend of service time across age is taken. And they have positive correlation

ggplot(absent,aes(x= Age,y= Service.time)) + geom_point()+ geom_smooth(method = 'lm') + labs(title='Analysis of Service time across Age',x='Age',y='Service time')


# Here trend of age across Absenteeism.time.in.hours taken. And they have Negative correlation

ggplot(absent,aes(x= Age,y= Absenteeism.time.in.hours)) + geom_point()+ geom_smooth(method = 'lm') + labs(title='Analysis of Absenteeism.time.in.hours across Age',x='Age',y='Absenteeism.time.in.hours')

# Here trend of service time across Absenteeism.time.in.hours is taken. And they have Negative correlation

ggplot(absent,aes(x= Service.time,y=Absenteeism.time.in.hours )) + geom_point()+ geom_smooth(method = 'lm') + labs(title='Analysis of Service time across Absenteeism.time.in.hours',x='Service time' ,y='Absenteeism.time.in.hours')

#Here our 3 rd Hypothesis is justified that People become more responsible as Age increases
#or Service time increases







##Trend between Work.load.Average.day & Absenteeism.time.in.hours and they've positive correlation


ggplot(absent,aes(x= Work.load.Average.day,y= Absenteeism.time.in.hours)) + 
  geom_point()+ geom_smooth(method = 'loess') + 
  labs(title='Analysis of Work.load.Average.dayacross Absenteeism.time.in.hours',
       x='Work.load.Average.day',y='Absenteeism.time.in.hours')


ggplot(absent,aes(x= Work.load.Average.day,y= Absenteeism.time.in.hours)) + 
  geom_point()+ geom_smooth(method = 'lm') + 
  labs(title='Analysis of Work.load.Average.day across Absenteeism.time.in.hours',
       x='Work.load.Average.day',y='Absenteeism.time.in.hours')  


#Here our 6th Hypothesis is justified that The pressure at work sometimes takes a toll on the
#employees. This results in increased levels of stress. The employees then resort to 
#excuses that can help them stay away from work.


####Feature Engineering ###

##Define Health Status
#If Body.mass.index <=18 then person is considered "UnderWeight"
#If 18< Body.mass.index <25 then person is considered "Normal"
#If Body.mass.index >25 then person is considered "Obese"

da$HealthStatus<- ifelse(da$Body.mass.index <=18, da$HealthStatus <- "UnderWeight", 
                         ifelse(da$Body.mass.index >18  &  da$Body.mass.index < 25, 
                                da$HealthStatus <- "Normal", 
                                da$HealthStatus <- "Obese"))



#Analysis HealthStatus V/S Absenteeism.time.in.hours

ggplot(da, aes(x = Absenteeism.time.in.hours)) + geom_histogram(binwidth = 2) + facet_grid(HealthStatus ~ .)
##We see that Obese people tend to take long hours of absence or full day leaves comparetively

###############Social Drinker analysis
#Analysis Day.of.the.week V/S Absenteeism.time.in.hours

ggplot(da, aes(x = Absenteeism.time.in.hours)) + geom_histogram(binwidth = 2) + facet_grid(Day.of.the.week ~ .)

#Analysis Social.drinker V/S Absenteeism.time.in.hours

ggplot(da, aes(x = Absenteeism.time.in.hours)) + geom_histogram(binwidth = 2) + facet_grid(Social.drinker ~ .)


#We see that there're more long hours of absence or full day leaves cn Mondays comparetively
#our 5th Hypothesis is justified ,that drinking and amusements in the late hours of night make it difficult for the workers 
#to reach in time on their duties. They like to become absent rather than late since they 
#know that.
#########################################


#Analysis Social.smoker V/S Absenteeism.time.in.hours

ggplot(da, aes(x = Absenteeism.time.in.hours)) + geom_histogram(binwidth = 2) + facet_grid(Social.smoker ~ .)

#This kinda justifies our 7th Hypothesis .
#We see that Non-Smokers are being abscent for 1 to 3 hours more than smokers.
#Do q quick survey for it, whether our cafeteria is having enough food counters and do we've 
#enough games available in company itself for employees


#Analysis HealthStatus V/S Work.load.Average.day
#Density plot
ggplot(da, aes(x = Work.load.Average.day, color = HealthStatus)) + geom_density()

#We see that Obese people are having more Work load.Average per day. Or it's vice versa since 
#Few people have been so engrossed in work that work load is taking toll on their health and 
#they've become couch potato and resulted them being Obese


#Analysis Day.of.the.week V/S Work.load.Average.day
#Density plot
ggplot(da, aes(x = Work.load.Average.day, color = Day.of.the.week)) + geom_density()
#We see that Work load average per day is quite higher on Tuesday comparetively so we draw 
#conclusion that since a decent number of people took off on Monday or been abscent on Monday
#and this resulted in Work load average per day being high on Tuesday.


#Analysis Social.smoker V/S Work.load.Average.day
#Density plot
ggplot(da, aes(x = Work.load.Average.day, color = Social.smoker)) + geom_density()

#Analysis Social.drinker V/S Work.load.Average.day
#Density plot
ggplot(da, aes(x = Work.load.Average.day, color = Social.drinker)) + geom_density()

#Analysis Month.of.absence V/S Work.load.Average.day
#Density plot
ggplot(da, aes(x = Work.load.Average.day, color = Month.of.absence)) + geom_density()


#We see that Work.load.Average.day is relatively higher in Jan & Feb, Since most of It comapnies
#have shutdown time in December and this might result in high Work.load.Average.day in Jan & Feb

##################### To calculate current Workloss

da$workloss <- ((da$Work.load.Average.day) /8) * (da$Absenteeism.time.in.hours)
sum(da$workloss)

##Current workloss is 9,05,94,032 units

# library(plyr)

#Assuming Company pays to Transport Vendor on monthly basis; If an employee is absent for a day
#then still company has to pay his monthly amount to the vendor; So considering this also a kind
#of loss to the company

LeaveOfDay <- as.data.frame( da %>% select(everything()) %>% filter(Absenteeism.time.in.hours ==8))
Transportation.expense<-sum(LeaveOfDay$Transportation.expense)
Transportation.expense

#Loss of company in transportation expenses this year is 47,788 rs.
#ie.3982.333 rs. per month

#############################Data Exploration################### 

#Analyze HealthStatus V/S Absenteeism.time.in.hours
group<-da %>%
  group_by(HealthStatus) %>%
  select(HealthStatus ,Absenteeism.time.in.hours) %>%
  arrange(HealthStatus) %>%
  distinct()




#
group<-da %>%
  group_by(ID,Month.of.absence,Day.of.the.week) %>%
  select(ID,Age,HealthStatus, Month.of.absence,Day.of.the.week,Work.load.Average.day,Absenteeism.time.in.hours,workloss) %>%
  arrange(ID,Month.of.absence,Day.of.the.week) %>%
  distinct()


#Make Absenteeism.time.in.hours bucket and analyze across various features such as 
# reason.for.absence,workloss,HealthStatus
Abs_Bucket <-da %>%
  group_by(Absenteeism.time.in.hours,Reason.for.absence,HealthStatus) %>%
  select(Absenteeism.time.in.hours,Reason.for.absence,workloss,HealthStatus) %>%
  summarise(count=n()) %>%
  arrange(Absenteeism.time.in.hours,Reason.for.absence,HealthStatus) %>%
  distinct() 

group1 <-da %>%
  group_by(Absenteeism.time.in.hours) %>%
  select(Age,HealthStatus, Month.of.absence,Day.of.the.week,Work.load.Average.day,Absenteeism.time.in.hours) %>%
  arrange(HealthStatus,Month.of.absence,Day.of.the.week) %>%
  distinct()

group1<-da %>%
  group_by(Reason.for.absence,Absenteeism.time.in.hours) %>%
  filter(Reason.for.absence == "medical consultation") %>%
  select(Reason.for.absence,HealthStatus,Absenteeism.time.in.hours) %>%
  arrange(Reason.for.absence)  


#Make Distance.from.Residence.to.Work bucket and analyze across various features such as 
# Absenteeism.time.in.hours,Reason.for.absence

Dist_Bucket <-da %>%
  group_by(Distance.from.Residence.to.Work) %>%
  select(Distance.from.Residence.to.Work,Absenteeism.time.in.hours,Reason.for.absence) %>%
  arrange(Distance.from.Residence.to.Work,Absenteeism.time.in.hours,Reason.for.absence) %>%
  distinct() 

#Make Service.time bucket and analyze across various features such as 
# Absenteeism.time.in.hours,Reason.for.absence

Svc_Time_Bucket <-da %>%
  group_by(Service.time) %>%
  select(Service.time,Absenteeism.time.in.hours,Reason.for.absence) %>%
  arrange(Service.time,Absenteeism.time.in.hours) %>%
  distinct()  



# Generate 2011 data to predict loss if same trend continues

Absence2011Data <- data_New


Absence2011Data $Age <- Absence2011Data$Age + 1
Absence2011Data $Service.time <- Absence2011Data $Service.time + 1


glimpse(Absence2011Data)
Absence2011Data$Absenteeism.time.in.hours <- NULL

##############Linear Regression
mod1<- lm(Absenteeism.time.in.hours~., data =data_New )

summary(mod1)

preds <- predict(mod1, Absence2011Data )

sum(preds)
#5007.25 Hours will be lost if same trend continues.


par(mfrow=c(2,2))
suppressWarnings(plot(mod1))

###################Random Forest#########
mod2 <- randomForest(Absenteeism.time.in.hours~ . ,  importance=TRUE ,data=data_New)

importance(mod2)

###Plot Variable importance
varImpPlot(mod2, sort=TRUE, n.var=min(30, nrow(mod2$importance)))
#####Build Random Forest Model on Important variables only

mod2 <- randomForest(Absenteeism.time.in.hours~ Reason.for.absence+Month.of.absence+Work.load.Average.day+
                     Day.of.the.week+ Age+Distance.from.Residence.to.Work+Body.mass.index+Social.drinker+Social.smoker
                        ,  importance=TRUE ,data=data_New)
##Plot Variable importance once again
varImpPlot(mod2, sort=TRUE, n.var=min(30, nrow(mod2$importance)))


Absence2011Data$Absenteeism.time.in.hours <- predict(mod2, Absence2011Data )

sum(Absence2011Data$Absenteeism.time.in.hours)

#2648.963 working Hours will be lost if same trend continues.


Absence2011Data$workloss<-0
Absence2011Data$workloss <- ((Absence2011Data$Work.load.Average.day) /8) * (Absence2011Data$Absenteeism.time.in.hours)
sum(Absence2011Data$workloss)

#Hence we can say that 89,379,007 units in a year ie. 7,448,251 units of work loss per month
#will occur , if same trend continues

Absence2011Data$Absenteeism.time.in.hours<- NULL #Delete it for further computation


########Build LinearRegression
mod3<- lm(Absenteeism.time.in.hours~ Reason.for.absence+Month.of.absence+Work.load.Average.day+
            Day.of.the.week+ Age+Distance.from.Residence.to.Work+Body.mass.index+Social.drinker+Social.smoker
          , data =data_New )

summary(mod3)

Absence2011Data$Absenteeism.time.in.hours <- predict(mod3, Absence2011Data )


sum(Absence2011Data$Absenteeism.time.in.hours)
#2663.352 Hours will be lost if same trend continues.


par(mfrow=c(2,2))
suppressWarnings(plot(mod1))

#############
###########Decision tree###########
library(tree)
output.tree<- tree(Absenteeism.time.in.hours~ Reason.for.absence+Month.of.absence+Work.load.Average.day+
                     Day.of.the.week+ Age+Distance.from.Residence.to.Work+Body.mass.index+Social.drinker+Social.smoker
                   , data=data_New)

pred3 <- predict(output.tree, Absence2011Data )

sum(pred3)
# 5783.417 Hours will be lost if same trend continues.

library(rpart)
library(rpart.plot)


Mod5 <- rpart(Absenteeism.time.in.hours~ Reason.for.absence+Work.load.Average.day+
                Day.of.the.week+Age+Distance.from.Residence.to.Work+Body.mass.index+Social.drinker+Social.smoker
                
                          , data=data_New,method= "anova")

pred4 <- predict(output.tree, Absence2011Data )
sum(pred4)

###Plot Decision Tree rules
par(mfrow=c(1,1))
rpart.plot(Mod5)





