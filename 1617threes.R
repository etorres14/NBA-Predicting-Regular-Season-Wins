library(readxl)
NbaTeam1617 <- read_excel("Desktop/math494/NbaTeam1617.xlsx")
View(NbaTeam1617)

won = NbaTeam1617$won
ThreePointAttempts = NbaTeam1617$`3a`
summary(cbind(won,ThreePointAttempts))
model1 = lm(won~ThreePointAttempts)
summary(model1)

yHat = function(x){
  y = 20.783 + x*0.009133
  return(y)
}
plot(ThreePointAttempts,won)
lines(ThreePointAttempts,yHat(ThreePointAttempts))
plot(model1)

### using 3P made as explanatory variable
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


#multiple regression
rebounds = NbaTeam1617$tr
twoPtPerc = NbaTeam1617$`2p%`
threPtPerc = NbaTeam1617$`3p%`
steals = NbaTeam1617$st
turnovers = NbaTeam1617$to
blocks = NbaTeam1617$bk
assist = NbaTeam1617$as
offReb = NbaTeam1617$or
ppg = NbaTeam1617$`Pts/gm`

multmodel = lm(won~ rebounds + twoPtPerc + ThreePoinPercent  + turnovers + steals + ppg )
summary(multmodel)
drop1(multmodel, test = 'F')
plot(multmodel)
qqplot(multmodel)
