# examples4.R

library(Lahman)

myteams <- subset(Teams, yearID > 2000) [,c("teamID", "yearID", "lgID", "G",
                                          "W", "L", "R", "RA")]
myteams$RD <- with(myteams, R - RA)
myteams$Wpct <- with(myteams, (W) / (W + L))
with(myteams, plot(RD, Wpct, xlab="Run Differential",
     ylab="Win Percentage", las=1))


# 4.3 Linear Regression
# Create model
linfit <- lm(Wpct ~ RD, data=myteams)
abline(a=coef(linfit)[1], b=coef(linfit)[2])

# Apply model to myteams
myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

# Plot residuals
with(myteams, plot(RD, linResiduals, xlab="Run Differential",
                   ylab="Residual", las=1))
abline(h=0, lty=3)

mean(myteams$linResiduals) # -3.127622e-20, unbiased model
linRSME <- sqrt(mean(myteams$linResiduals^2)) # 0.02509761

# Since model is unbiased, RMSE = SD
# ~68% of residuals are within 1 SD
nrow(subset(myteams, abs(linResiduals) < linRSME)) /
  nrow(myteams) # 0.6822222

# ~95% of residuals are within 2 SD
nrow(subset(myteams, abs(linResiduals) < 2*linRSME)) /
  nrow(myteams) # 0.9555556

summary(myteams$linResiduals)

# 4.4 Pythagorean Formula
myteams$pytWpct <- with(myteams, R^2 / (R^2 + RA^2))
myteams$pytResiduals <- myteams$Wpct - myteams$pytWpct
pytRMSE <- sqrt(mean(myteams$pytResiduals^2)) # 0.02541395

# 4.5 Adjusting the Pythagorean Exponent
myteams$logWratio <- log(myteams$W / myteams$L)
myteams$logRratio <- log(myteams$R / myteams$RA)
pytFit <- lm(logWratio ~ 0 + logRratio, data=myteams) # 1.88

# 4.6 Good/Bad Pyt Examples
setwd("Documents/baseball/r/data")
gl2011 <- read.table("gl2011.txt", sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)
BOS2011 <- subset(gl2011, HomeTeam=="BOS" | VisitingTeam=="BOS")[, c("VisitingTeam", "HomeTeam",
                                                                     "VisitorRunsScored",
                                                                     "HomeRunsScore")]
BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam=="BOS", HomeRunsScore - VisitorRunsScored,
                                          VisitorRunsScored - HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0

# Aggregate stats by W/L
aggregate(abs(BOS2011$ScoreDiff), list(W=BOS2011$W), summary)

# Look at impact of close games on pyt Win pct
results <- gl2011[, c("VisitingTeam", "HomeTeam", "VisitorRunsScored",
                      "HomeRunsScore")]
results$winner <- ifelse(results$HomeRunsScore > results$VisitorRunsScored, 
                         as.character(results$HomeTeam), as.character(results$VisitingTeam))
results$diff <- abs(results$VisitorRunsScored - results$HomeRunsScore)

onerungames <- subset(results, diff==1)
onerunwins <- as.data.frame(table(onerungames$winner))
names(onerunwins) <- c("teamID", "onerunW")

teams2011 <- subset(myteams, yearID==2011)
teams2011[teams2011$teamID=="LAA", "teamID"] <- "ANA"
teams2011 <- merge(teams2011, onerunwins, by="teamID")
with(teams2011, plot(onerunW, pytResiduals, xlab="One run wins",
                     ylab="Pythagorean residuals", las=1))

# Examine effect of good closers
top_closers <- subset(Pitching, GF>50 & ERA<2.5)[, c("playerID", "yearID",
                                                     "teamID")]
teams_top_closers <- merge(myteams, top_closers)
summary(teams_top_closers$pytResiduals)
