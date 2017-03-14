# exercises4.R

# 1. Relationship between Winning Percentage
# and Run Differential across Decades

# Collect teams by decade
library(Lahman)
teams60s <- subset(Teams, yearID > 1960 & yearID < 1971)[, c("teamID", "yearID",
            "lgID", "G", "W", "L", "R", "RA")]
teams70s <- subset(Teams, yearID > 1970 & yearID < 1981)[, c("teamID", "yearID",
            "lgID", "G", "W", "L", "R", "RA")]
teams80s <- subset(Teams, yearID > 1980 & yearID < 1991)[, c("teamID", "yearID",
            "lgID", "G", "W", "L", "R", "RA")]
teams90s <- subset(Teams, yearID > 1990 & yearID < 2001)[, c("teamID", "yearID",
            "lgID", "G", "W", "L", "R", "RA")]
teams00s <- subset(Teams, yearID > 2000 & yearID < 2011)[, c("teamID", "yearID",
            "lgID", "G", "W", "L", "R", "RA")]
teams10s <- subset(Teams, yearID > 2010)[, c("teamID", "yearID", "lgID", "G", "W",
            "L", "R", "RA")]

# Calculate Run Differential/Win Percentage
calcRD <- function(df){
  df$RD <- df$R - df$RA
  df$Wpct <- df$W / (df$W + df$L)
  df
}

teams60s <- calcRD(teams60s)
teams70s <- calcRD(teams70s)
teams80s <- calcRD(teams80s)
teams90s <- calcRD(teams90s)
teams00s <- calcRD(teams00s)
teams10s <- calcRD(teams10s)

# Fit linear model
fitlm <- function(df){
  model <- lm(Wpct ~ RD, data=df)
}
model.60s <- fitlm(teams60s)
model.70s <- fitlm(teams70s)
model.80s <- fitlm(teams80s)
model.90s <- fitlm(teams90s)
model.00s <- fitlm(teams00s)
model.10s <- fitlm(teams10s)

# Compare a team with RD=10 across decades
model.Wpct <- function(linmod){
  wpct <- (10 * coef(linmod)[2]) + coef(linmod)[1]
  as.numeric(wpct)
}

# Organize results to display as plot
RD10.results <- data.frame(decade=seq(1960, 2010, 10))
models <- list(model.60s, model.70s, model.80s, model.90s, model.00s, model.10s)
RD10.results$Wpct <- sapply(X=models, FUN=model.Wpct)
RD10.results$W <- round(RD10.results$Wpct * 162)

with(RD10.results, plot(decade, Wpct, xlab="Decade", ylab="Win Percentage",
                        main="Win Percentage with a Run Differential of +10",
                        ylim=c(0.45,0.55), las=1))

# 2. Pythagorean Residuals for Poor and Great Teams in 19th Century

# Collect data on 19th century teams
teams <- subset(Teams, yearID <= 1900)[, c("teamID", "yearID", "W", "L",
                                             "R", "RA", "G")]
teams$RD <- teams$R - teams$RA
teams$Wpct <- with(teams, W / (W + L))

# Fit model
pymodel <- lm(Wpct ~ RD, data=teams)
teams$pyWpct <- predict(pymodel)
teams$pyResiduals <- residuals(pymodel)

with(teams, plot(RD, pyResiduals, las=1, xlab="Run Differential",
                 ylab="Residual"))
abline(h=0, lty=3)


# 3. Exploring the Manager Effect during the Long Ball Era

# Collect teams 1993-Present
lb.teams <- subset(Teams, yearID >= 1993)[, c("teamID", "yearID", "G", "W",
                                          "L", "R", "RA")]
lb.teams$RD <- lb.teams$R - lb.teams$RA
lb.teams$Wpct <- with(lb.teams, W / (W + L))

# Fit pythagorean model
lb.pymodel <- lm(Wpct ~ RD, data=lb.teams)
lb.teams$pyWpct <- predict(lb.pymodel)
lb.teams$pyResiduals <- residuals(lb.pymodel)

# Find top 3 over performing teams
head(lb.teams[order(lb.teams$pyResiduals, decreasing=TRUE),])
  # -> ARI 2005, LAA 2008, BAL 2012

# Find corresponding managers
overperform <- subset(Managers, (yearID==2005 & teamID=="ARI") |
                        (yearID==2008 & teamID=="LAA") |
                        (yearID==2012 & teamID=="BAL"))
mgr.overperform <- merge(overperform, Master, by="playerID")[, c("nameFirst", "nameLast")]
  # Bob Melvin, Mike Scioscia, Buck Showalter

# Find top 3 under performing teams
head(lb.teams[order(lb.teams$pyResiduals),])
  # -> NYN 1993, CLE 2006, SDN 1994

# Find corresponding managers
underperform <- subset(Managers, (yearID==1993 & teamID=="NYN") |
                         (yearID==2006 & teamID=="CLE") |
                         (yearID==1994 & teamID=="SDN"))
mgr.underperform <- merge(underperform, Master, by="playerID")[, c("nameFirst", "nameLast")]
  # Dallas Green, Jim Riggleman, Jeff Torborg, Eric Wedge

