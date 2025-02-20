#POKEMON GO CASE
#TEAM 18
#Charles DE LA RIVIERE
#Marie MASSERAN
#Gagan AGARWAL
#Clara JOUY
#Xavier MELLEVILLE


########################## LAST UPDATES ########################## 
#by Xavier M on Nov22 23:59, Q°1 (3 subquestions) about base table : 
#STILL NEEDS TO BE REVIEWED
#also missing answer for the last Q° in subquestion 2, 
#about the general profile (see comments in the code below)

#by Xavier M on Nov24 at 16:18,Q°1
#completed the missing part for 'sketching a general profile'
#added monetary value and CLV in the basetab (active customers)

#by Xavier M on Nov27 at 8pm
#Q2 : added the basic Life Cycle Grid
#to add: more sophisticated grids + interpretation/recommendation

#by Xavier M on Nov30 at 12
#Q2 : added the Life Cycle Grid with monetary value
#Q3 : added the code to answer the question
#We may need to add other variables to the logistic regression
#Recommendations still to be discussed

#by Clara on Dec1 at 18
#Q1: Reshaping the basetable to:
#regroup everyone (players and buyers)
#compute a sess frequency and session recency to profile not only on transaction but on the use of the app
#add all the variables at our disposal to profile customers
#define a new variable "seniority" computed as the difference between our analysis date (31/08/2018) and their registration date
#Q2: Simplify the building of the grids by rewritting the code on the Q1 basetable that have all the necessary informartions

#by Gagan on Dec2 at 15
#Q2 : Added the Life Cycle Grid with Income
#Added the Life Cycle Grid with Age Groups

#By Clara on DEC2 at 23
#Q1 : Added some suggestion of graph for customer profiling
#Q2: Added grids on buyers non buyers
#Gathered all the code

#By Clara on Dec3 at 9
#Cleaned the code and organized the grid and the zzresearch for profiling

#By Clara on Dec4 at 9,17,20
#Corrected question 1 with Xavier (CLV)
#Discuss grids with Xavier adn Gagan (17H30-19h30)
#Support to xavier for modelling and performance assistance, modif of almost all
#the code with him
#Change slightly the grids

#By Clara Dec5 at 7
#Change the variable seniority as disscussed into cohort of 6 months seniority

#By Clara Dec5 at 11
#CHange all the design of graphs



#COMMENTS


##########################              ########################## 


#LOAD RELEVANT LIBRARIES
library(dplyr)
library(Amelia) #for missmap
library(reshape2)#arrange function
library(ggplot2)
library(magrittr)


#DATA PREPARATION
#Loading files
custdata = read.csv('customerdata.csv')
fallfin = read.csv('fallfintrx.csv')
fallsess = read.csv('fallsesstrx.csv')
summerfin = read.csv('summerfintrx.csv')
summersess = read.csv('summersesstrx.csv')

#Exploring the structure of data
custdata %>% str #We have to remark that all the clients have registered on the game before the period we are loing at. 
summerfin %>% str ; #summerfin$ProductID = as.factor(summerfin$ProductID)
summersess %>% str
fallfin %>% str ; #fallfin$ProductID = as.factor(fallfin$ProductID)
fallsess %>% str


custdata$CustomerID %>% unique %>% length() #One unique line per customers (5000 customers)
fallfin$CustomerID %>% unique %>% length() #One unique line per micro-transaction (during Fall)
fallsess$CustomerID %>% unique %>% length() #One unique line per play session (during Fall)
summerfin$CustomerID %>% unique %>% length() #One unique line per micro-transaction (during Summer)
summersess$CustomerID %>% unique %>% length() #One unique line per play session (during Summer)

#Checking missing values
any(is.na(custdata));any(is.na(fallfin));any(is.na(fallsess));any(is.na(summerfin));any(is.na(summersess))
#NO MISSING VALUES

#Creating a dictionary referring to the monetary value of each productID
price=data.frame(ProductID=1:5,Price= c(2.99,4.99,9.99,25,99)) #prices are in USD


#CASE QUESTIONS
################################################
#Q1:  CREATION OF BASE TABLE + PROFILING
################################################

#Q1.1 Create a basetable containing the active customers (at least 1 play session during the
#summer period). Indicate whether they received the fall boost package discount or not.

#Active customer: how to define it? 
#First, a customer is defined by its ID, so we will look at the Customer ID variable in each table
#Secondly, a customer is active when its customer ID appears at least once in the summersess table
#NOW, let's create a base table called basetab similar to custdata but with only active customers 
summersess = mutate(summersess, session=1) #this will help us count the number of playing sessions

basetab = summersess %>%
  group_by(CustomerID)%>%
  summarise(nb_summersess = sum(session))%>%
  ungroup()%>%
  filter(!is.na(nb_summersess)) #NA in nb_summersess means that the CustomerID was not in the summersess table, then it means that the player was inactive: we remove that

basetab %>% nrow #We have 4703 active players among the 5000 recorded in our data base

basetab = custdata[basetab$CustomerID,]

#NOW our base table corresponds to the custdata table but only with active players
#The dummy variable fallbonus indicates whether they received the fall boost package discount or not.

#We can also note that some ancient customer (207) that did not play during the summer are playing again in 
#fall
newfallcust <- fallsess %>% filter(CustomerID %in% setdiff(fallsess$CustomerID,basetab$CustomerID))
length(unique(newfallcust$CustomerID))


#Q1.2 Calculate the demographics and RFM metrics for the relevant play and financial
#transactions database. Based on these metrics, sketch a general profile (use the correct
#descriptive metric for each variable) of the customer base according to demographics,
#spending and usage transactions. 

#Demographics can be found in the base table basetab, with the variables Sex, Age and Income


#RFM metrics

#We want to compute two kind of RFM metrics: one based on the sessions dataset to analyse the playing pattern
#and one based on the financial transaction database to undestand the players ready to buy and the buying pattern 

summerfin = full_join(summerfin, price, "ProductID")

summerfin$Date = as.Date(summerfin$Date,"%Y-%m-%d")
currentdate = as.Date("2018-08-31","%Y-%m-%d")
RFM = summerfin %>%
  group_by(CustomerID)%>%
  summarise(buy.frequency=n(),buy.recency=currentdate-max(Date),monetaryvalue=sum(Price))%>%
  ungroup()
RFM = RFM %>% arrange(CustomerID, decreasing = FALSE)


basetab <- merge(basetab,RFM,all.x=TRUE) %>% 
  mutate(Buyers = ifelse(!is.na(monetaryvalue),1,0))

basetab$Buyers <- factor(basetab$Buyers, levels=c(0,1), labels=c("Non_Buyers","Buyers"))

#We have a lot of missing value because only 36% of active players are buyers
#We will then also compute the frequency and recency of players with the summersess file
simp <- function(x)
{round(mean(x),0)}

summersess$Date = as.Date(summersess$Date,"%Y-%m-%d")
RFMsess = summersess %>%
  group_by(CustomerID)%>%
  summarise(sess.frequency=n(),
            sess.recency=currentdate-max(Date),
            avg.Experience= simp(Experience),#we keep all the data to do a more accurate profiling, we took the average metric per player
            avg.Pokestops=simp(Pokestops),
            avg.Gyms= simp(Gyms),
            avg.Raids=simp(Raids),
            avg.Social=simp(Social),
            avg.Pokemons= simp(Pokemons),
            avg.Distance= simp(Distance),
            avg.Duration= simp(Duration)) %>%
  ungroup()
RFMsess = RFMsess %>% arrange(CustomerID, decreasing = FALSE)

basetab <- merge(basetab,RFMsess,all.x=TRUE) %>% 
  mutate(monetaryvalue= ifelse(is.na(monetaryvalue),0,monetaryvalue)) %>% #The non buyers have no monetary value
  mutate(buy.frequency= ifelse(is.na(buy.frequency),0,buy.frequency)) %>% #The non buyers have no monetary value
  mutate(buy.recency= as.numeric(ifelse(is.na(buy.recency),200,buy.recency))) #the rencency for non buyers is innacurate, we take a high number not 
#alter too much the data (the lower the recency the better)

#Create a seniority variable
basetab$Registrationdate <-as.Date(basetab$Registrationdate,'%Y-%m-%d')
summary(basetab$Registrationdate)
difftime(time1 = max(basetab$Registrationdate),time2 = min(basetab$Registrationdate),units = "weeks")/4 #We have 1 years 10 months between the first and the last registered
dep=difftime(as.Date('2018-08-31',"%Y-%m-%d"),as.Date('2018-05-01',"%Y-%m-%d"),units = "weeks")
dep=round(as.numeric(dep),0)


basetab <- basetab %>% mutate(seniority =difftime(time1 = currentdate,time2 =  as.Date(Registrationdate,"%Y-%m-%d"),units = "weeks")) #compute the seniority in weeks
basetab$seniority <- round(as.numeric(basetab$seniority),0)
avg.seniority = mean(basetab$seniority)
par(mfrow=c(1,1))
hist(basetab$seniority, 
     xlab = 'seniority', 
     main="Repartition of players' seniority in weeks")


#We can separate them into 6 months group (assuming 4 week per months)
basetab <- basetab %>%
  mutate(seniority=ifelse(between(seniority,dep,dep+24), '<6 months', #nex players = loin since less than five months
                          ifelse(between(seniority,dep+24,dep+48), '6-12 months',#middle term players have joined since less than one year
                                 ifelse(between(seniority,dep+48,dep+72),'12-18 months','18-24 months')))) #long term players have joined between one and two years , then very long term players

basetab$seniority <- factor(basetab$seniority, levels=c('<6 months',"6-12 months","12-18 months",'18-24 months'))

basetab$sess.recency <- as.numeric(basetab$sess.recency)
basetab$CustomerType <- factor(basetab$CustomerType,levels = c(1,2,3,4), labels = c("Walker","Miscellaneous Player","Social Raider","Catcher"))
basetab <- basetab %>% mutate(age.group=ifelse(between(Age, 0, 14), '0-14 Years',
                                               ifelse(between(Age, 15, 25), '15-25 Years',
                                                      ifelse(between(Age, 26, 40), '26-40 Years',
                                                             ifelse(between(Age, 41, 60),'41-60 Years', '61 Years and Older')))))

summary(basetab)
#We can see that only 36% of the players are buyers, so itmight be interesting to look at them specificly. 
#Creating a basetable for the buyers
basetab.buy <- basetab[basetab$Buyers=="Buyers",]

#We can compute two different RFMs that can be insightful: 
# -The general RFM of the overall dataset -> to look at the behaviour of the average customern (we will rather fuccus on the RF of sessions)
# -The specific RFM of buyers to better understand the buying pattern

#
RFMs= rbind(Users= sapply(basetab[,c("buy.frequency","buy.recency","sess.frequency","sess.recency","monetaryvalue")],mean),
            Buyers = sapply(basetab.buy[,c("buy.frequency","buy.recency","sess.frequency","sess.recency","monetaryvalue")],mean))

#We can see that the buyers are playing more often in average and are more recent players.          


#Q1.3: Calculate the customer life time value for these customers. You can make assumptions for
#unknown variables (e.g., discount rate, # periods in the future), but motivate your
#assumptions clearly in the report.

#We will foccuss on computing the CLV buyers as the others have no monetary value. 

#TO calculate the Customer Life time Value (CLV) we need to know these informations for each CustomerID:
#Acquisition Cost -->NO DATA, need assumptions
#Recurring contribution margin --> we know 1 component, Revenue, but we miss the Cost -->NO DATA, need assumptions
#Retention probability --> we have information about churn, thanks to the table for the fall period -->we could compute 
#a retention probability by performing a logistic regression on churn
#Number of years out to estimate --> NO DATA, need assumptions (based on industry common practices)
#Discount rate --> can be estimated by taking the discount rate of the European Central Bank for the year 2018
#We may take the value of 1.66%, as it fits with the objective of the CLV calculation:
#assess how much it could cost for the company to borrow money in 2018.
#That's why we choose to assume that our discount rate is the ECB composite cost-of-borrowing indicator of November 2018.
#src: https://www.ecb.europa.eu/press/pr/stats/mfi/html/ecb.mir1811.en.html

#Acquisition cost
##################
#We take the data found on statista (https://www.statista.com/statistics/185736/mobile-app-average-user-acquisition-cost/) 
#We found that AC for suscribe is in avg 2.17$ US for a common app (you have to subscribe to play to pokémon go)
#The acquisition cost for in-app purchase is not used here as it is not specified in the case that the app has done such marketing 
#investments and the application advertisement is mainly working on Word of mouth. 
cr=0.85713 # the converstion rate from US$ to on August, 30th 2018 (https://fr.exchange-rates.org/Rate/USD/EUR/30-08-2018)
#Thus we have
AC=2.71*cr


#######################
#First try to compute CLV with in-app purchase AC but really to high
#basetab <- basetab %>%
#  mutate(AC=ifelse(monetaryvalue==0,2.17*cr,(2.17+86.7)*cr ))
#basetab.buy <- basetab.buy %>%
#  mutate(AC=ifelse(monetaryvalue==0,2.17*cr,(2.17+86.7)*cr ))
#########################

#m = RFM$monetaryvalue #As it is a mobile app, the cost is mainly structured by fixed cost of development, IT maintenance and marketing spending. 
#Then we assume that the marginal cost is 0, so the recurring margin is equal to the recurring revenue

#r = ?
#To assess the retention rate, we will assess the probability that a paying customer in the Summer 2018 remains
#a paying customer in the Fall 2018, given RFM metrics
fallfin = full_join(fallfin, price, "ProductID")#adding the price to the transaction database

RFM2 = merge(RFM,fallfin,by="CustomerID",all.x =TRUE)
RFM2 = RFM2 %>%
  group_by(CustomerID)%>%
  summarise(monetaryvalue_fall = sum(Price))%>%
  ungroup()%>%
  mutate(churn = is.na(monetaryvalue_fall))
mean(RFM2$churn == TRUE) #corresponds to our churn rate between summer and fall 
r = (1 - mean(RFM2$churn == TRUE)) #we have the retention rate for 4 months, which is the step we select


t = 24/4 #we assume a 24-month life time period as the average industry is 3 months but we can see when plotting the
#seniority of our buyers that half of them plays around one year and our average seniority is of 55 weeks, so we will rather take two years.   
#We have 6 periods of 4months.
ggplot(aes(x=seniority),data = basetab.buy)+
  geom_bar(stat="count")+
  ggtitle("Repartition of buyers depending on seniority")
avg.seniority

d = 0.0166/3 #based on assumption above (ECB, Nov 2018)

CLV_fun = function(x){
  m = x #as our data about revenue during the summer runs on a 4-month period we divide it by for to have the mv for one month
  clv = -AC
  for(i in 0:t)#attention: start in month 0
  {
    clv = clv+((r^i)*m/(1+d)^i)
  }
  round(clv,2)
}

#NOW we want to integrate monetary value and CLV in our active customers table (basetab), in order to find statistical insights
basetab = basetab %>% mutate(CLV=CLV_fun(monetaryvalue))
basetab.buy = basetab.buy %>% mutate(CLV= CLV_fun(monetaryvalue))

mean(basetab$CLV)
mean(basetab.buy$CLV)


#The CLV of the overall actif players is slightly positive at 3.73 which is low because the vast majority of people do not buy
#thus have a negative CLV.But thze CLV of buyers is quite good at 14.45 
##


##########################
#Sketch a general profile#
##########################

#NEED HELP HERE : not sure about what they expect when they say draw a general profile
#I first thought it referred to the 4 customer types (walkers,...) but it did not as we already have data on it
#Maybe they expect us to say like: "40% of paying customers are 18-25yold"... this kinf off thing, I dont know
basetab$Sex <- factor(basetab$Sex,levels = c(0,1), labels = c('Male','Female'))


sapply(basetab,mean)


#average age of a player: 27.6yearq
Prep <- basetab %>% 
  group_by(CustomerType)

ggplot(data= basetab, aes(x=seniority,fill=Sex))+
  geom_bar(stat='count', position= "stack")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10),)+
  scale_fill_stata()+
  facet_grid(rows = vars(Buyers),cols=vars(CustomerType))

ggplot(data= basetab, aes(x=,fill=Sex))+
  geom_bar(stat='count', position= "stack")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10),)+
  scale_fill_stata()+
  facet_grid(rows = vars(Buyers),cols=vars(CustomerType))


  
  
#39,5% of female, 60,5% of male
#Most of the player are playing five times or more
median(basetab$sess.recency)
#Half of the players have played at least once 
#between the half and the end of August
mean(as.numeric(basetab$Buyers)-1)
#36% only of player have bought something at least once on the app
#20% on reçu un fall bonus
summary(as.factor((basetab$Income)))
#Majority have a medium income

hist(basetab.buy$monetaryvalue,freq = TRUE)




#39.5% of active players are female (60.5% are male)
#The mean age of active players is 28 yold
#20% of active players received a fall bonus
#In average, they have a medium estimated income
#Active players spent in average $3.97 during the Summer
#Active customers have an average CLV of $8.24
#So our typical player is a male of 28 years old having a medium income,
#spending $4 during the Summer and having a CLV of $8.24


#Other insightful metrics
mean(basetab.buy$CLV) #paying customer have an average CLV of $0.68
mean(basetab.buy$monetaryvalue) #paying-customers spent in average $11 during the Summer




############################
#Q2: Lifecycle grids on players#
############################
library(ggthemes)

#Exploratory analysis
ggplot(basetab, aes(x=sess.frequency)) +
  theme_stata() +
  scale_x_continuous(breaks=c(1:10)) +
  geom_bar(alpha=0.6) +
  ggtitle("Distribution by frequency")

ggplot(basetab, aes(x=sess.recency)) +
  theme_stata() +
  geom_bar(alpha=0.6) +
  ggtitle("Distribution by recency")

#Building the frenquency and recency segments
basetab.segm <- basetab %>%
  mutate(segm.freq=ifelse(between(sess.frequency, 1, 1), '1',
                          ifelse(between(sess.frequency, 2, 2), '2',
                                 ifelse(between(sess.frequency, 3, 3), '3',
                                        ifelse(between(sess.frequency, 4, 4), '4',
                                               ifelse(between(sess.frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(sess.recency, 0, 10), '0-10 days',
                         ifelse(between(sess.recency, 11, 31), '11-30 days',
                                ifelse(between(sess.recency, 31, 60), '31-60 days',
                                       ifelse(between(sess.recency, 61, 80), '61-80 days',
                                              ifelse(between(sess.recency, 81, 100), '81-100 days', '>100 days'))))))


# Defining order of boundaries
basetab.segm$segm.freq <- factor(basetab.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
basetab.segm$segm.rec <- factor(basetab.segm$segm.rec, levels=c('>100 days', '81-100 days', '61-80 days', '31-60 days', '11-30 days', '0-10 days'))


#GRID1: by quantity#
####################

lcg <- basetab.segm %>%
  group_by(segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client="client")
ungroup()

ggplot(lcg, aes(x=client ,y=quantity, fill=quantity))+
  geom_bar(stat='identity', alpha=0.6) +
  geom_text(aes(y=max(quantity)/2, label=quantity), size=4) +
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("RF quantity grid")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))





#GRID2: by monetary value#
##########################

lcg <- basetab.segm %>%
  group_by(segm.rec, segm.freq,CustomerType) %>%
  summarise(value=round(mean(monetaryvalue),1)) %>%
  ungroup()

ggplot(lcg, aes(x=CustomerType, y=value, fill=CustomerType))+
  geom_bar(stat='identity') +
  geom_text(aes(y=value+2.5, label=value), size=3) +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"))+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Average Monetary value grid per customer", subtitle='by Customer Type on the frequence of playing')+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()




#GRID3: BUYERS VS NON BUYERS LYCECYCLE GRIDS#
#############################################

lcg <- basetab.segm %>%
  group_by(segm.rec, segm.freq,Buyers) %>%
  summarise(quantity=n()) %>%
  ungroup()

ggplot(lcg, aes(x=Buyers ,y=quantity, fill=Buyers))+
  geom_bar(stat='identity') +
  geom_text(aes(y=quantity+70, label=quantity), size=3) +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"))+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Number of customer depending on recency and frequency of playing session",subtitle = 'Buyers VS Non Buyers')+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()



#GRID4: CUSTOMER TYPE LFG#
##########################



lcg.tot <- basetab.segm %>%
  group_by(segm.freq,segm.rec, CustomerType)%>%
  summarise(tot = n())%>%
  ungroup()
lcg <- basetab.segm %>%
  group_by(segm.rec, segm.freq,CustomerType,Buyers) %>%
  summarise(quantity=n()) %>%
  ungroup()
lcg = merge(lcg,lcg.tot, by=c('segm.freq','segm.rec','CustomerType'), all.x = TRUE) %>%
  mutate(percent= round(quantity/tot*100,0.01))

ggplot(lcg, aes(x=CustomerType ,y=quantity, fill=Buyers))+
  geom_bar(stat='identity') +
  geom_text(stat='identity',
            aes(label=ifelse(Buyers=='Non_Buyers',tot,''),y=tot+70),size=3)+
  geom_text(stat='identity',
            aes(label=ifelse(Buyers=='Buyers'&percent >=30,
                             paste(percent,'%'),''), y=tot+30),,size=3, alpha= 0.7)+
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"))+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Customer Type LFG (Proportion of buyers%)")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()



#GRID4: Seniority LFG#
##########################



lcg.tot <- basetab.segm %>%
  group_by(segm.freq,segm.rec, seniority)%>%
  summarise(tot = n())%>%
  ungroup()
lcg <- basetab.segm %>%
  group_by(segm.rec, segm.freq,seniority,Buyers) %>%
  summarise(quantity=n()) %>%
  ungroup()
lcg = merge(lcg,lcg.tot, by=c('segm.freq','segm.rec','seniority'), all.x = TRUE) %>%
  mutate(percent= round(quantity/tot*100,0.01))

ggplot(lcg, aes(x=seniority ,y=quantity, fill=Buyers))+
  geom_bar(stat='identity', alpha=0.9) +
  geom_text(stat='identity',
            aes(label=ifelse(Buyers=='Non_Buyers',tot,''),y=tot+80),size=3)+
  geom_text(stat='identity',
            aes(label=ifelse(Buyers=='Buyers',
                             paste(percent,'%'),''), y=tot+34),,size=3, alpha= 0.7)+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Seniority LFG (Quantity, Proportion of buyers%)")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()



#GRID5: by Income#
##################

lcg <- basetab.segm %>%
  group_by(Income, segm.rec, segm.freq) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

lcg.adv <- lcg %>%
  mutate(rec.type = ifelse(segm.rec %in% c(">100 days", "81-100 days", "61-80 days"), "not recent", "recent"),
         freq.type = ifelse(segm.freq %in% c(">5", "5", "4"), "frequent", "infrequent"),
         Player.type = interaction(rec.type, freq.type)) 

ggplot(lcg.adv, aes(x=Income, y=quantity, fill=quantity))+
  geom_bar(stat='identity') +
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("RF Income grid")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))



#GRID6: by Age group#
#####################

lcg <- basetab.segm %>%
  group_by(age.group, segm.rec, segm.freq,Sex) %>%
  summarise(quantity=n()) %>%
  mutate(client='client') %>%
  ungroup()

lcg.adv <- lcg %>%
  mutate(rec.type = ifelse(segm.rec %in% c(">100 days", "81-100 days", "61-80 days"), "not recent", "recent"),
         freq.type = ifelse(segm.freq %in% c(">5", "5", "4"), "frequent", "infrequent"),
         Player.type = interaction(rec.type, freq.type))

ggplot(lcg.adv, aes(x=age.group, y=quantity, fill=age.group))+
  geom_bar(stat='identity') +
  facet_grid(segm.freq ~ segm.rec)+
  theme(axis.text.x=element_text(angle=90, hjust=.5, vjust=.5, face="plain"))+
  ggtitle("RF Age grid")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()





#Lifecycle grids on buyers#
###########################

# exploratory analysis

ggplot(basetab.buy, aes(x=buy.frequency)) +
  theme_stata() +
  scale_x_continuous(breaks=c(1:10)) +
  geom_bar(alpha=0.6) +
  ggtitle("Distribution by frequency")

ggplot(basetab.buy, aes(x=buy.recency)) +
  theme_stata() +
  geom_bar(alpha=0.6) +
  ggtitle("Distribution by recency")


basetab.buy.segm <- basetab.buy %>%
  mutate(segm.freq=ifelse(between(buy.frequency, 1, 1), '1',
                          ifelse(between(buy.frequency, 2, 2), '2',
                                 ifelse(between(buy.frequency, 3, 3), '3',
                                        ifelse(between(buy.frequency, 4, 4), '4',
                                               ifelse(between(buy.frequency, 5, 5), '5', '>5')))))) %>%
  mutate(segm.rec=ifelse(between(buy.recency, 0, 20), '0-20 days',
                         ifelse(between(buy.recency, 21, 40), '21-40 days',
                                ifelse(between(buy.recency, 41, 60), '41-60 days',
                                       ifelse(between(buy.recency, 61, 80), '61-80 days',
                                              ifelse(between(buy.recency, 81, 100), '81-100 days', '>100 days'))))))


# defining order of boundaries
basetab.buy.segm$segm.freq <- factor(basetab.buy.segm$segm.freq, levels=c('>5', '5', '4', '3', '2', '1'))
basetab.buy.segm$segm.rec <- factor(basetab.buy.segm$segm.rec, levels=c('>100 days', '81-100 days', '61-80 days', '41-60 days', '21-40 days', '0-20 days'))


#GRID1: by Customer type#
#########################

lcg <- basetab.buy.segm %>%
  group_by(segm.rec, segm.freq,CustomerType) %>%
  summarise(quantity=n(),value=round(mean(monetaryvalue),0),tot=round(sum(monetaryvalue),0))%>%
  ungroup()

ggplot(lcg, aes(x=CustomerType ,y=quantity, fill=CustomerType))+
  geom_bar(stat='identity') +
  geom_text(aes(y=max(quantity)/3, label=quantity), color='white',size=3,alpha=0.65) +
  geom_text(aes(y=quantity+15, label=paste(value,"€")), size=3)+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("RF depending on the profile of the player")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()


#Attempt with churn



lcg <- merge(basetab.buy.segm,RFM2,by='CustomerID') %>% 
  select(-monetaryvalue_fall) %>% 
  mutate(churn= ifelse(churn==TRUE,"Churner","Non_Churner"))

lcg <- lcg %>%
  group_by(segm.rec, segm.freq,churn,CustomerType) %>%
  summarise(quantity=n(),value=round(mean(CLV),0))%>%
  ungroup()

ggplot(lcg, aes(x=CustomerType ,y=mean(value), fill=churn))+
  geom_bar(stat='identity') +
  geom_text(aes(y=max(quantity)/3, label=quantity), color='white',size=3,alpha=0.65) +
  geom_text(aes(y=max(value)+5, label=ifelse(churn=="Churner",'',paste(value,"€"))), size=3)+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Churner VS non churner buying behaviour",subtitle = "Average monetary value on the top")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()



#GRID4: by Customer type and moneytary value#
#############################################

lcg <- basetab.buy.segm %>%
  group_by(segm.rec, segm.freq,CustomerType) %>%
  summarise(value=round(sum(monetaryvalue),1),quantity=n())%>%
  ungroup()

ggplot(lcg, aes(x=CustomerType ,y=value, fill=CustomerType))+
  geom_bar(stat='identity') +
  geom_text(aes(y=value+60, label=ifelse(value<=100,'',paste(value,"€"))), size= 3) +
  geom_text(aes(y=100, label=ifelse(value<=100,'',quantity)), color='white',size=3,alpha=0.65) +
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Monetary value depending on type of customer", subtitle = 'Depending on buying recency and frequency of buying (Quantity)')+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()

#########################################
#GRID5: by seniority and moneytary value#
#########################################




lcg <- basetab.buy.segm %>%
  group_by(segm.rec, segm.freq,seniority) %>%
  summarise(quantity=n(),value=round(sum(monetaryvalue),1), avg.val=round(mean(monetaryvalue),1))%>%
  ungroup()

ggplot(lcg, aes(x=seniority, fill=seniority))+
  geom_bar(stat='identity', aes(y=value)) +
  geom_text(aes(y=value+150, label=ifelse(value<=100,'',value)), size= 3) +
  geom_text(aes(y=value/2, label=ifelse(value<=100,'',quantity)),color='white',size= 3,alpha=0.7)+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("Monetary value depending on the seniority of the player (Quantity)")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()

ggplot(lcg, aes(x=seniority, fill=seniority))+
  geom_bar(stat='identity', aes(y=avg.val), alpha=2)+
  geom_text(aes(y=avg.val+25, label=round(avg.val,0)), size= 3) +
  geom_text(aes(y=avg.val+10, label=ifelse(avg.val<=10,'',quantity)), size= 2.5,alpha=0.7)+
  facet_grid(segm.freq ~ segm.rec)+
  ggtitle("RF depending on the seniority of the player (Quantity)")+
  ggthemes::theme_stata()+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill=NA,linetype = 1),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain"),
        axis.text.y = element_text(size = 8,angle = 360),
        axis.title.y =  element_text(size = 15, vjust = 1.5),
        axis.title.x =  element_text(size = 15),
        strip.text = element_text(size=10))+
  scale_fill_stata()

################################################
#Q3: CHURN ANALYSIS + MARKETING STRATEGY
################################################


#Calculate the average churn rate

fallfin = fallfin %>% group_by(CustomerID) %>% summarise(monetaryvaluefall=sum(Price))
newbase = merge(basetab,fallfin,by="CustomerID",all.x=TRUE)
 
newbase = newbase %>%
  mutate(churn = as.numeric(is.na(monetaryvaluefall))) 
mean(newbase$churn)
#According to the churn definition, being an active player during the summer
#that do not perform any transaction during the fall,
#the average churn rate is 87.49 %


#Find the significant factors that affect the churn rate

#As a first assumption, we can imagine that:
#The younger you are, the less likely you are to churn because younger people usually play more
#The more income you have, the less likely you are to churn, because you may be limited by your budget
#The more senior you are, the more likely you are to churn because you can get bored by the game
#If you received a fall bonus, you are less likely to churn because you don't want to not miss the benefit of the bonus and feel privileged



#First we prepare our data to perform our logistic regression, selected almost all variables first
numbase = newbase %>% select(-Registrationdate,-age.group,-monetaryvaluefall)
#we remove those variables because their are already represented by others:
#registration date --> seniority
#age.group --> age
#monetaryvaluefall --> we are only interested in the summer period here.

#Here we transform categorical variables into dummy variables for each level
lev1 = levels(numbase$CustomerType)
lev2 = levels(numbase$seniority)

numbase = numbase %>% 
  mutate(walker = ifelse(CustomerType == lev1[1],1,0)) %>%
  mutate(miscellaneous =ifelse(CustomerType == lev1[2],1,0)) %>%
  mutate(socialraider =ifelse(CustomerType == lev1[3],1,0)) %>%
  mutate(catcher = ifelse(CustomerType == lev1[4],1,0))%>%

  mutate(male = ifelse(Sex=='Male',1,0)) %>%
  
  mutate(Buyers = ifelse(Buyers == "Buyers",1,0)) %>%
  
  mutate(newplayer = ifelse(seniority == lev2[1],1,0)) %>%
  mutate(midtermplayer = ifelse(seniority == lev2[2],1,0)) %>%
  mutate(longtermplayer = ifelse(seniority == lev2[3],1,0)) %>%
  mutate(verylongtermplayer = ifelse(seniority == lev2[4],1,0))
  
numbase = numbase %>% select(-CustomerType,-Sex,-seniority) 
#we remove categorical variables that are now converted into dummy variables

#########################################
#Creating the test and validation sample
#########################################


#We scale our data in order to better assess the explanatory power of each variable
numbase.scaled = data.frame(scale(numbase %>% select(-churn)))
numbase.scaled =  numbase.scaled %>% mutate(churn=numbase$churn)


library(caTools)
set.seed(654)
split <- sample.split(numbase.scaled$CustomerID, SplitRatio = 0.90)
training <- subset(numbase.scaled, split == TRUE)
validation <- subset(numbase.scaled, split == FALSE)

########################################################################
#Now we check the correlation matrix, and we will try to avoid putting correlated variables as 
#explanatory variables in our regression
corr1 = cor(training)

library(corrgram)
corrgram(training, order=TRUE, lower.panel=panel.pie,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Pokgo vars")

#Variables describing how correlated are the player patterns variables with the customer types. 
#so we will drop them and just keep the customer types. 
#Same for the monetary value and CLV, lets keep only the monetary value because we want to keep observed values (non-computed) to make more simple 
#The buy.frequency is very correlated with recency and monetaryvalue, we will keep only the recency and drop the Buyer/Non_Buyer variable which add no information. 
#We also keep only the sess.recency as it is highly correlated with the sess.frequency, which is also very correlated with the monetaryvalue etc. (which the recency is not)

training = training %>% select(CustomerID,monetaryvalue,sess.recency,walker,catcher,socialraider,miscellaneous,Age,midtermplayer,longtermplayer,verylongtermplayer,fallbonus,Income,churn)


#lets's check again

corrgram(training, order=TRUE, lower.panel=panel.pie,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax, 
         main="Pokgo vars")


#The remaining variables are not significantly correlated with each other, then we keep them for our model.
####################################
#END OF CORRELATION ANALYSIS





#MODELING
##########

empty <- glm(churn ~ 1, family=binomial(link='logit'),data=training)
full <- glm(churn ~. - CustomerID, family=binomial(link='logit'),data=training)
modeltraining<-step(empty, scope=list(lower=empty, upper=full), direction="forward")
modeltraining<-step(full, direction="backward")
modeltraining<-step(full, direction="both")
summary(modeltraining)

#From our logistic regression on the churn pattern, here are our insights (by decreasing order of impact):
#If you have spent a lot of money (high monetary value), you are less likely to churn compared to other players
#If you have received a fall bonus, you are less likely to churn
#If you are a new player, you are more likely to churn


#add predicted churn probabilities in a new column
training$predict <- predict(modeltraining,training, type = 'response')
validation <- validation %>%select(CustomerID,monetaryvalue,sess.recency,walker,catcher,socialraider,miscellaneous,Age,newplayer,midtermplayer,longtermplayer,verylongtermplayer,fallbonus,Income,churn)
validation$predict <- predict(modeltraining,validation, type = 'response')


#Assessing the impact of fallbonus on the probability to churn
training2 = training %>% select(monetaryvalue,sess.recency,catcher,socialraider,midtermplayer,longtermplayer,verylongtermplayer,fallbonus)
train.mean = sapply(training2,mean)

logit = function(x){
  y = 1/(1+exp(-x))
  y
  }

test.withoutfallbonus = sum(coef(modeltraining)*c(1,train.mean[-8],-0.5010761))
test.withfallbonus = sum(coef(modeltraining)*c(1,train.mean[-8],1.9952806))

fallbonus.prob.impact = ( logit(test.withfallbonus) - logit(test.withoutfallbonus) ) / logit(test.withoutfallbonus)
#Ceteris paribus, giving a fallbonus to someone reduces its probability to churn by 17.46%

numbase.churner = numbase %>% filter(churn==1) %>% filter(Buyers==1) #In our strategy, we will focus churners that are also
#buyers as we are not interested in non-buyers because they have a negative CLV
churner.CLV = mean( numbase.churner$CLV ) #a churner that is also a payer has an average CLV of $14.37
#Then, the fact of loosing a customer makes us loose the potential future revenue from this customer,
#represented by its CLV
#If giving a fallbonus reduces the probability to churn by 17.46%, then in average we avoid loosing...
fallbonus.prob.impact*churner.CLV
#...€2.51 per customer. If our fallbonus costs us more than this then it's not worth it.

hist(numbase.churner$CLV)
#But when we analyse the repartition of the distribution of CLV among churner, we observe a lot of disparities
#A lot of churners have a negative CLV, where as others have high CLV
#Then, an agile strategy would be to focus our fallbonus on customers having a positive CLV, and 
#abandon customers with a negative CLV, as it would cost us money to keep them as customers.



#Performance
############

#Now we would like to assess the performance of our logistic regression
library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(tidyr)
library(InformationValue)

#Loading of the function used: the ones that have been provided during the course. Thank you very much
################################################################
AccuracyCutoffInfo <- function( train, test, predict, actual )
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .4, .8, by = .05 )
  
  accuracy <- lapply( cutoff, function(c)
  {
    # use the confusionMatrix from the caret package
    cm_train <- caret:::confusionMatrix( as.factor(as.numeric( train[[predict]] > c )), as.factor(train[[actual]]) )
    cm_test  <- caret:::confusionMatrix( as.factor(as.numeric( test[[predict]]  > c )), as.factor(test[[actual]] ) )
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  }) %>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" )
  
  return( list( data = accuracy, plot = plot ) )
}

ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
{	
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in 
  # a two by two confusion matrix	
  predict <- data[[predict]]
  actual  <- relevel( as.factor( data[[actual]] ), "1" )
  
  result <- data.table( actual = actual, predict = predict )
  
  # caculating each pred falls into which category for the confusion matrix
  result[ , type := ifelse( predict >= cutoff & actual == 1, "TP",
                            ifelse( predict >= cutoff & actual == 0, "FP", 
                                    ifelse( predict <  cutoff & actual == 1, "FN", "TN" ) ) ) %>% as.factor() ]
  
  # jittering : can spread the points along the x axis 
  plot <- ggplot( result, aes( actual, predict, color = type ) ) + 
    geom_violin( fill = "white", color = NA ) +
    geom_jitter( shape = 1 ) + 
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
    scale_y_continuous( limits = c( 0, 1 ) ) + 
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend 
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )
  
  return( list( data = result, plot = plot ) )
}

ROCInfo <- function( data, predict, actual, cost.fp, cost.fn )
{
  # calculate the values using the ROCR library
  # true positive, false postive 
  pred <- prediction( data[[predict]], data[[actual]] )
  perf <- performance( pred, "tpr", "fpr" )
  roc_dt <- data.frame( fpr = perf@x.values[[1]], tpr = perf@y.values[[1]] )
  
  # cost with the specified false positive and false negative cost 
  # false postive rate * number of negative instances * false positive cost + 
  # false negative rate * number of positive instances * false negative cost
  cost <- perf@x.values[[1]] * cost.fp * sum( data[[actual]] == 0 ) + 
    ( 1 - perf@y.values[[1]] ) * cost.fn * sum( data[[actual]] == 1 )
  
  cost_dt <- data.frame( cutoff = pred@cutoffs[[1]], cost = cost )
  
  # optimal cutoff value, and the corresponding true positive and false positive rate
  best_index  <- which.min(cost)
  best_cost   <- cost_dt[ best_index, "cost" ]
  best_tpr    <- roc_dt[ best_index, "tpr" ]
  best_fpr    <- roc_dt[ best_index, "fpr" ]
  best_cutoff <- pred@cutoffs[[1]][ best_index ]
  
  # area under the curve
  auc <- performance( pred, "auc" )@y.values[[1]]
  
  # normalize the cost to assign colors to 1
  normalize <- function(v) ( v - min(v) ) / diff( range(v) )
  
  # create color from a palette to assign to the 100 generated threshold between 0 ~ 1
  # then normalize each cost and assign colors to it, the higher the blacker
  # don't times it by 100, there will be 0 in the vector
  col_ramp <- colorRampPalette( c( "green", "orange", "red", "black" ) )(100)   
  col_by_cost <- col_ramp[ ceiling( normalize(cost) * 99 ) + 1 ]
  
  roc_plot <- ggplot( roc_dt, aes( fpr, tpr ) ) + 
    geom_line( color = rgb( 0, 0, 1, alpha = 0.3 ) ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.2 ) + 
    geom_segment( aes( x = 0, y = 0, xend = 1, yend = 1 ), alpha = 0.8, color = "royalblue" ) + 
    labs( title = "ROC", x = "False Postive Rate", y = "True Positive Rate" ) +
    geom_hline( yintercept = best_tpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" ) +
    geom_vline( xintercept = best_fpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" )				
  
  cost_plot <- ggplot( cost_dt, aes( cutoff, cost ) ) +
    geom_line( color = "blue", alpha = 0.5 ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.5 ) +
    ggtitle( "Cost" ) +
    scale_y_continuous( labels = comma ) +
    geom_vline( xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4" )	
  
  # the main title for the two arranged plot
  sub_title <- sprintf( "Cutoff at %.2f - Total Cost = %f, AUC = %.3f", 
                        best_cutoff, best_cost, auc )
  
  # arranged into a side by side plot
  plot <- arrangeGrob( roc_plot, cost_plot, ncol = 2, 
                       top = textGrob( sub_title, gp = gpar( fontsize = 16, fontface = "bold" ) ) )
  
  return( list( plot 		  = plot, 
                cutoff 	  = best_cutoff, 
                totalcost   = best_cost, 
                auc         = auc,
                sensitivity = best_tpr, 
                specificity = 1 - best_fpr ) )
}

################################################################


# Assessing accuracy: AUC curve
pred <- prediction(training$predict,training$churn)
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)
auc.perf = performance(pred, measure = "auc")
auc.perf
auc.perf@y.values[[1]]
#The AUC is 0.78, which is not so good and might be more precise. 

#To find the right cutoff
accuracy_info <- AccuracyCutoffInfo( train = training, test = validation,predict = "predict", actual = "churn" )
accuracy_info$plot
#Here we can see that a better cutoff would be around 0.70 
#We select the optimum cutoff
c=0.7

#Confusion matrix
cm_info <- ConfusionMatrixInfo( data = validation, predict = "predict", 
                                actual = "churn", cutoff = c)
cm_info
#We see that with this cutoff, we have more False positive (predicted as churner while non-churner)
#than False negative (predicted as non-churner while churner)


#ROC
#For a false positive (predicting that the customer will churn while he/she will not),
#we could assume a cost equivalent to the monetary value of our fallbonus (which we don't know)
#But in fact, if the fallbonus as a monetaryvalue for the customer, it has a zero marginal cost for Niantic.
#However, as PokemonGo is a competitive game, non-churners that received the fallbonus will not need to 
#buy the equivalent of this fallbonus compared to non-churners that dit not receive the fallbonus
#THEN we assume that the cost of our false positive represent the loss of the sale (the player would have paid for it otherwise),
#equivalent to the amount of the bonus, 
#let's assume €2 so that it increase our profit as it is lower that what we gain
#with the fallbonus, as computed earlier (€2.51)

fp = 2
#For a false negative (predicting that the customer will not churn while he/she will),
#we assume a cost equivalent to the loss of the CLV corresponding to the customer
#Here we will consider the cost of false negative as the average CLV of churners

fn = 0

roc_info <- ROCInfo( data = cm_info$data, predict = "predict", 
                     actual = "actual", cost.fp = fp, cost.fn = fn )
grid.draw(roc_info$plot)
#The cutoff that minimize the cost if infinite because we have a null total cost.
#Then this suggests us that we 


#Performance & Prediction
1-misClassError(validation$churn,validation$predict, threshold = 0.01)# 86,84% of the predictions have been
#accurately classified
precision(validation$churn,validation$predict, threshold = c) 
#89.39% of churners are correctly predicted as churners


lift <- function(depvar, predcol, groups=10) 
{
  #making sure everything is numeric
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  #sort df by churn prob from high to low and create deciles:
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%#in each decile
    summarise_at(vars(depvar), funs(total = n(),#count churners
                                    totalresp=sum(., na.rm = TRUE))) %>%#sum total number of churners
    mutate(Cumresp = cumsum(totalresp),#cumulative churners
           Gain=Cumresp/sum(totalresp)*100,#churners found in decile versus total amount of churners
           Cumlift=Gain/(bucket*(100/groups)))#gain obtained in comparison to random number of churners found (10%-->20%)
  return(gaintable)
  #totalresp or total churners=92 (only in validation dataset)
  #%gain in first decile (24 detected churners/92 total churners)*100=26%
  #by randomly consulting 10% of the customers we  expect to find 10% of the churners
  #lift=%gain of the decile /%random churners found in the decile
}

#construct the decile table
decilechart = lift(validation$churn , validation$predict, groups = 10)

#attempt at a better lift chart
ggplot(decilechart, aes(x = bucket, y = Cumlift))+
  theme_stata() +
  theme(panel.grid = element_blank())+#removes colored box behind bar chart
  scale_x_continuous(breaks=c(1:10)) +
  geom_point()+
  geom_smooth()+
  geom_text(aes(x=bucket+0.5,y=Cumlift, label=round(Cumlift,2))) +
  ggtitle("Lift curve")

basetab %>% filter(Buyers == "Buyers") %>% nrow / 4703


#STRATEGY RECOMMENDATION (to be discussed)
#Our main objective is to prevent churn
#Based on our analysis of the customer data base,
#We found out these customer segments are more likely to churn:
#high paying players
#non-new player
#On the other hand, we found out that the fallbonus have an impact,
#it reduces the probability for a player to churn (by ???%)
#THEN, in order to prevent churn, we recommend Niantic to focus granting fall bonuses on this player segment

#To fine tune our strategy, we could grant higher fallbonuses to customers that are likely to churn
#(as defined above) and also have a higher CLV.
#This means that we are trying to avoid churn, especially when customers have a high potential of future revenue (CLV)






