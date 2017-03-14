# exercises2.R

#
# 1. Top Base Stealers in the Hall of Fame
#
SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335, 307, 212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)
Player <- c("Rickey Henderson", "Lou Brock", "Ty Cobb", "Eddie Collins",
            "Max Carey", "Joe Morgan", "Luis Aparicio", "Paul Molitor",
            "Roberto Alomar")
SB.Attempt <- SB + CS
Success.Rate <- SB / SB.Attempt
SB.Game <- SB / G

plot(SB.Game, Success.Rate)

# Highest/lowest success rates
Player[which.min(Success.Rate)] # Lou Brock
Player[which.max(Success.Rate)] # Max Carey

# Most stolen bases/game
Player[which.max(SB.Game)] # Rickey Henderson


#
# 2. Character, Factor, and Logical Variables in R
#
outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double",
              "Out", "Walk", "Out", "Single")
table(outcomes)

f.outcomes <- factor(outcomes, levels=c("Out", "Walk", "Single", "Double"))
table(f.outcomes)

outcomes == "Walk"
sum(outcomes == "Walk")


#
# 3. Pitchers in the 350-Wins Club
#
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux",
          "Mathewson", "Nichols", "Spahn", "Young")
W <- c(373, 354, 364, 417, 355, 373, 361, 363, 511)
L <- c(208, 184, 310, 279, 227, 188, 208, 245, 316)

Win.Pct <- 100 * W / (W + L)
Wins.350 <- data.frame(Name, W, L, Win.Pct)
Wins.350[order(Win.Pct, decreasing=TRUE),] # Highest = Mathewson, Lowest = Galvin


#
# 4. Pitchers in the 350-Wins Club, con't
#
Name <- c("Alexander", "Clemens", "Galvin", "Johnson", "Maddux",
          "Mathewson", "Nichols", "Spahn", "Young")
SO <- c(2198, 4672, 1806, 3509, 3371, 2502, 1868, 2583, 2803)
BB <- c(951, 1580, 745, 1363, 999, 844, 1268, 1434, 1217)

SO.BB.Ratio <- SO / BB
SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)

subset(SO.BB, SO.BB.Ratio > 2.8) # Clemens, Maddux, Mathewson

SO.BB[order(SO.BB$BB, decreasing=TRUE),] # Highest Walks: Clemens, 2.96 SO/BB ratio
summary(SO.BB$SO.BB.Ratio) # Clemens SO/BB ratio in top quartile

#
# 5. Pitcher Strikeout/Walk Ratios
#
library(Lahman)
library(dplyr)

stats <- function(d){
  c.SO <- sum(d$SO, na.rm=TRUE)
  c.BB <- sum(d$BB, na.rm=TRUE)
  c.IPouts <- sum(d$IPouts, na.rm=TRUE)
  c.midYear <- median(d$yearID, na.rm=TRUE)
  
  data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts, midYear=c.midYear)
}

career.pitching <- group_by(Pitching, playerID) %>% do(stats(.))

career.10000 <- subset(career.pitching, IPouts >= 10000)
with(career.10000, lines(lowess(midYear, SO/BB)))
