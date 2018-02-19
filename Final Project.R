###Final Project- NHL Salaries Projected. Good or Bad Values?
#Vincent Ditmore, Bobby Mullins, Cory Swift
#Advanced Regression Techniques


###########Full Model(All Players included)###########################

###Gather Data
NHLFM= `NHL.2015.16.xls...Sheet1(3)`
#Note:right off the bat, we excluded players that played under 45 games.
#We also used salary which is the average salary insted of the cap hit and total 
#amount of money earned, as we are trying to figure whether or not the player was
#a good value this year. The problem with incentive contracts is that the bonuses
#they earn go on to count for the next year.


Salary = NHLFM$Salary
Age = NHLFM$Age
PlusMinus = NHLFM$X...
GamesPlayed = NHLFM$GP
Goals = NHLFM$G
Assists = NHLFM$A
Shots = NHLFM$Sh
PenaltyMinutes = NHLFM$PIM
TOIPerGame = NHLFM$TOI.G
Points = NHLFM$PTS# aliased coefficient

###Create Full Model
Fmodel=lm(Salary~PlusMinus+GamesPlayed+Goals+Assists+Shots+PenaltyMinutes+TOIPerGame+Points)
summary(Fmodel)
#Note:Rsquared value of .4274 which is not a bad starting point

##Age Factor
plot(NHLFM$Age,NHLFM$Salary)
#note:as we can see in this graph, players 24 and under tend to have a darker circle
#lower on the y axis. This is due to entry level contracts. In the next model we will
#aim to take these entry level contracts out by taking out all the players 24 and 
#under out of our model. We will also take out players 38 and older to keep
#variance constant.

#######Reduced Model1########

###Gather Data
NHLRM= `NHL.2015.16.xls...Sheet1(4)`
#Note:In this data set we exclude the players 24 and under and 38 and older.

Salary1 = NHLRM$Salary
Age1 = NHLRM$Age
PlusMinus1 = NHLRM$X...
GamesPlayed1 = NHLRM$GP
Goals1 = NHLRM$G
Assists1 = NHLRM$A
Shots1 = NHLRM$Sh
PenaltyMinutes1 = NHLRM$PIM
TOIPerGame1 = NHLRM$TOI.G


###Create Reduced Model
Rmodel=lm(Salary1~PlusMinus1+GamesPlayed1+Goals1+Assists1+Shots1+PenaltyMinutes1+TOIPerGame1)
summary(Rmodel)
#Note:Rsquared value of .5895 which has increased significantly from the Fmodel

##Minutes Played.. 4th line vs top 9 vs elite players
plot(NHLRM$TOI.G,NHLRM$Salary)
#Note:We are going to make minutes played into a boolean variable. We are going
#to do this because top line players usually play 18-22 minutes a night.
#Other top 9 players play around 14-17 minutes a night, while 4th line players play
#13 and under minutes. As you may notice in the graph, the more ice time a player
#has over the course of a season the more his salary will be.(trend)

#######Reduced Model2########

###Gather Data
NHLRM1= `NHL.2015.16.xls...Sheet1(5)`
#Note:In this data set we exclude the players 24 and under and 38 and older.

Salary2 = NHLRM1$Salary
Age2 = NHLRM1$Age
PlusMinus2 = NHLRM1$X...
GamesPlayed2 = NHLRM1$GP
Goals2 = NHLRM1$G
Assists2 = NHLRM1$A
Shots2 = NHLRM1$Sh
PenaltyMinutes2 = NHLRM1$PIM
TOIPerGame2 = NHLRM1$TOI.G


###Create Reduced Model
Rmodel1=lm(Salary2~PlusMinus2+GamesPlayed2+Goals2+Assists2+Shots2+PenaltyMinutes2+TOIPerGame2)
summary(Rmodel1)
#Note:our rsquared actually went down after trying to distinguish between 4th
#line vs 2nd and 3rd line vs 1st line. In our next model we will try to distinguish
#between top 6 forwards and bottom 6 forwards.


#######Reduced Model3########

###Gather Data
NHLRM2= `NHL.2015.16.xls...Sheet1(6)`
#Note:In this data set we exclude the players 24 and under and 38 and older.

Salary3 = NHLRM2$Salary
Age3 = NHLRM2$Age
PlusMinus3 = NHLRM2$X...
GamesPlayed3 = NHLRM2$GP
Goals3 = NHLRM2$G
Assists3 = NHLRM2$A
Shots3 = NHLRM2$Sh
PenaltyMinutes3 = NHLRM2$PIM
TOIPerGame3 = NHLRM2$TOI.G


###Create Reduced Model
Rmodel2=lm(Salary3~PlusMinus3+GamesPlayed3+Goals3+Assists3+Shots3+PenaltyMinutes3+TOIPerGame3)
summary(Rmodel2)
1#Note:our rsquared actually went up from the last time.So we will stop messing
#with time on ice. 

##Questioning why the predictor variable, games played, has a negative coefficient.
plot(NHLRM2$GP,NHLRM2$Salary)
#Note:In the next model we are going to make games played a boolean variable.
#If a player played 60 or more he recieves a 1 and if the player played 59 or less
#he recieved a 0.

#######Reduced Model4########

###Gather Data
NHLRM3= `NHL.2015.16.xls...Sheet1(7)`


Salary4 = NHLRM3$Salary
Age4 = NHLRM3$Age
PlusMinus4 = NHLRM3$X...
GamesPlayed4 = NHLRM3$GP
Goals4 = NHLRM3$G
Assists4 = NHLRM3$A
Shots4 = NHLRM3$Sh
PenaltyMinutes4 = NHLRM3$PIM
TOIPerGame4 = NHLRM3$TOI.G


###Create Reduced Model
Rmodel3=lm(Salary4~PlusMinus4+GamesPlayed4+Goals4+Assists4+Shots4+PenaltyMinutes4+TOIPerGame4)
summary(Rmodel3)
#Note: After making the changes we suggested above, our model is saying that
#players who play 60 games or more make 949,634 dollars less than a player
#who plays under 60 games. In our next model we have decided to take out the predictor
#variable, Games Played, as no one is really positive how many games a player
#will play due to injury, and family issues.(craig anderson)

#######Reduced Model5########

###Gather Data
NHLRM4= `NHL.2015.16.xls...Sheet4`


Salary5 = NHLRM4$Salary
PlusMinus5 = NHLRM4$X...
Goals5 = NHLRM4$G
Assists5 = NHLRM4$A
Shots5 = NHLRM4$Sh
PenaltyMinutes5 = NHLRM4$PIM
TOIPerGame5 = NHLRM4$TOI.G


###Create Reduced Model
Rmodel4=lm(Salary5~PlusMinus5+Goals5+Assists5+Shots5+PenaltyMinutes5+TOIPerGame5)
summary(Rmodel4)
#Note: Our rsquared was reduced a little as expected, however, it did not affect
#our model that much. It was deemed necessary.

##Finding the Leverage Points
lev=hat(model.matrix(Rmodel4))
plot(lev)
NHLRM4[lev>.2,]
#Note:There are no high leverage points in our dataset.

##Finding the Influence Points
cook=cooks.distance(Rmodel4)
plot(cook)
which(cook>.8)
#Note:There are no points that have that great of influence

##Taking out Certain People
#Jonathon Toews, Staal, Hossa, Brown, Lupul, Gaborik, Moulson, Gagne, Stepniak, Nash
#We did this for many reasons. Whether they are more defensive minded players,
#under performers, and over performers.This lets us better predict our y.
#we will come back to reevaluating them at the end.

#######Reduced Model6########

###Gather Data
NHLRM5= `NHL.2015.16.xls...Sheet4(2)`


Salary6 = NHLRM5$Salary
PlusMinus6 = NHLRM5$X...
Goals6 = NHLRM5$G
Assists6 = NHLRM5$A
Points6=NHLRM5$PTS
Shots6 = NHLRM5$Sh
PenaltyMinutes6 = NHLRM5$PIM
TOIPerGame6 = NHLRM5$TOI.G
Age6=NHLRM5$Age

###Create Reduced Model
Rmodel5=lm(Salary6~PlusMinus6+Goals6+Assists6+Shots6+PenaltyMinutes6+TOIPerGame6)
summary(Rmodel5)


###Final Model#########
Rmodel6=lm(Salary6~PlusMinus6+Points6+Shots6+PenaltyMinutes6+TOIPerGame6)
summary(Rmodel6)

#Note:Talk about why we swithced back to points

#######Checking assumptions

###Is the model linear?
pairs(Salary6~ PlusMinus6+Points6+Shots6+PenaltyMinutes6+TOIPerGame6, data=NHLRM5)
plot(NHLRM5$PTS,Rmodel6$residuals)
#note:Good
plot(NHLRM5$X...,Rmodel6$residuals)
#note:good
plot(NHLRM5$Sh,Rmodel6$residuals)
#note:good
plot(NHLRM5$PIM,Rmodel6$residuals)
#note:good
plot(NHLRM5$TOI.G,Rmodel6$residuals)
#note:variance differs. Heteroscedastic, however, 
#Model is linear

###Are the errors idependent and identically distributed
qqnorm(Rmodel6$res)
qqline(Rmodel6$res)
hist(Rmodel6$residuals)
#the error looks to be independent and identically distributed. obviously not perf

###Do the predictor variables seem to be independent?
pairs(~PlusMinus6+Points6+Shots6+PenaltyMinutes6+TOIPerGame6, data=NHLRM5)
#The predictor varaibles seem to important in the model. Naturally there will be 
#some collinearity

#checking if collinearity is a problem
library(car)
vif(Rmodel6)
#note:all predictor variables have a vif under 10 therefor collinearity is not
#too big of a problem.

#Autocorrelation
library(lmtest)
dwtest(Rmodel6)
#note:There is some autocorrelation in the dataset, however, we do not deem it a 
#problem, due to the fact that it is real world data/

###Data is reliable
#Note:We removed outliers. No high leverage points or influential points.

###95% Confidence interval
xBar=mean(Salary6)
xBar
n=221
dof=221-6
critvals=qt(c(.025,.975),dof)
s=sd(Salary6)

#Lower and Upper bounds
lowerBound=xBar-critvals[2]*s/sqrt(n)
lowerBound

upperBound=xBar+critvals[2]*s/sqrt(n)
upperBound



