###Simple Linear Regression
library(readxl)
NbaTeam1617 <- read_excel("Desktop/linear regression/NbaTeam1617.xlsx")
View(NbaTeam1617)

#Using 3pa as predictor variable
won = NbaTeam1617$won #response variable
ThreePointAttempts = NbaTeam1617$`3a` #explanatory variable

#Take a look at summary of data to check for outliers
summary(cbind(won,ThreePointAttempts))
boxplot(won, xlab = "Wins", ylab = "# of Wins")
hist(won) #won looks normal!

boxplot(ThreePointAttempts ) #notice Rockets' 3pa is an outlier, we'll leave it in though.
hist(ThreePointAttempts)
shapiro.test(ThreePointAttempts) #ThreePointAttempts are normal too!

                   
model1 = lm(won ~ ThreePointAttempts)
summary(model1)
# R - Squared value measure how much of the variation in Y (Wins) can be explained by X (ThreePointAttempts)
# Our model's R-Squared Value is pretty low at .07609
# notice our p-value for both the intercept and ThreePointAttempts isn't significant either.

#this is our linear regression model of our data  with the following response and explanatory variables
yHat = function(x){
  y = 20.783 + x*0.009133
  return(y)
}

#A plot of our data with or linear regression line
plot(ThreePointAttempts,won)
lines(ThreePointAttempts,yHat(ThreePointAttempts))

plot(model1)
#decent spread, centered at zero, balanced above and below line
#constant variability in residual plot


#############################################

# using 3P made as explanatory variable
won = NbaTeam1617$won
ThreePointersMade = NbaTeam1617$`3m`
summary(cbind(won,ThreePointersMade))
model2 = lm(won~ThreePointersMade)
summary(model2)


yHat = function(x){
  y = 11.20082 + x*.03764
  return(y)
}
plot(ThreePointersMade,won)
lines(ThreePointersMade,yHat(ThreePointersMade))
plot(model2)

#########################################################

#using #3p% as explanatory variable

won = NbaTeam1617$won
ThreePoinPercent = NbaTeam1617$`3p%`
summary(cbind(won,ThreePoinPercent))
model2 = lm(won~ThreePoinPercent)
summary(model2)

yHat = function(x){
  y = -99.57 + x*393.49
  return(y)
}
plot(ThreePoinPercent,won)
lines(ThreePoinPercent,yHat(ThreePoinPercent))
plot(model2)

#########################################################

#using Fieldgoal percentage as explanatory variable
won = NbaTeam1617$won
FieldGoalPercent = NbaTeam1617$`fg%`
summary(cbind(won,FieldGoalPercent))
model2 = lm(won~FieldGoalPercent)
summary(model2)

yHat = function(x){
  y = -208.76 + x*546.13
  return(y)
}
plot(FieldGoalPercent,won)
lines(FieldGoalPercent,yHat(FieldGoalPercent))
plot(model2)
qqplot(model2)

#Residuals median is close to zero and Residual plot looks good.(scattered and centered at 0)
#field goal percentage has highest R^2 value at .4438
#therefore we can conclude that fg% is more of an indicator of how many games a team will win.

#point differential as explanitory variable
won = NbaTeam1617$won
PointDiffer = NbaTeam1617$Pt.Diff.
summary(cbind(won,PointDiffer))
model2 = lm(won~PointDiffer)
summary(model2)

yHat = function(x){
  y = 41.0 + x*2.5010
  return(y)
}
plot(PointDiffer,won)
lines(PointDiffer,yHat(PointDiffer))
plot(model2)
qqplot(model2)

#########################################################

###MULTIPLE LINEAR REGRESSION

#more predictor variables
#used step-wise regression to end up with a model containing only significant variables
rebounds = NbaTeam1617$tr
twoPtPerc = NbaTeam1617$`2p%`
threePtPerc = NbaTeam1617$`3p%`
steals = NbaTeam1617$st
turnovers = NbaTeam1617$to
blocks = NbaTeam1617$bk
assist = NbaTeam1617$as
offReb = NbaTeam1617$or


orebounds = NbaTeam1617$`O tr`
otwoPtPerc = NbaTeam1617$`O 2p%`
othreePtPerc = NbaTeam1617$`O 3p%`
osteals = NbaTeam1617$`O st`
oturnovers = NbaTeam1617$`O tr`
oblocks = NbaTeam1617$`O bk`
oassist = NbaTeam1617$`O as`
ooffReb = NbaTeam1617$`O or`


#
multmodel = lm(won~ rebounds + twoPtPerc + threePtPerc  + turnovers + steals + assist  + orebounds
               + otwoPtPerc + othreePtPerc + oturnovers + orebounds + ooffReb )
summary(multmodel)


multmodel = lm(won~ rebounds + twoPtPerc + threePtPerc  + turnovers  + blocks
               + otwoPtPerc + othreePtPerc + oturnovers + orebounds )

summary(multmodel)

plot(multmodel)
#decent spread, centered at zero, balanced above and below line
#constant variability in residual plot
qqplot(multmodel)
#checking for multi-varaite normality
#point land nicely on the line
