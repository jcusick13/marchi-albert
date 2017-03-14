# 1. Hall of Fame Pitching Dataset
setwd("Documents/baseball/r/data")
hofpitching <- read.csv("hofpitching.csv")

hofpitching$BF.group <- with(hofpitching,
                             cut(BF, c(0, 10000, 15000, 20000, 30000),
                                 labels=c("Less than 10,000", "10,000 to 15,000",
                                          "15,000 to 20,000",
                                          "More than 20,000")))
BF.freq <- table(hofpitching$BF.group)
barplot(BF.freq, title("Batters Faced"), ylim=c(0,25))
pie(BF.freq)


# 2. HoF Pitching - continued
hist(hofpitching$WAR)
hofpitching[order(hofpitching$WAR, decreasing=TRUE),] # Cy Young & Walter Johnson WAR > 140

# 3. HoF Pitching - continued
hofpitching$WAR.Season <- with(hofpitching, WAR / Yrs)
par(plt=c(0.25, 0.95, 0.15, 0.85))
stripchart(WAR.Season ~ BF.group, data=hofpitching, method="jitter",
           pch=1, las=2)
boxplot(WAR.Season ~ BF.group, data=hofpitching,
        horizontal=TRUE, las=2, xlab="WAR per Season")

# 4. HoF Pitching - continued
hofpitching$MidYear <- with(hofpitching, (From + To) / 2)
hofpitching.recent <- subset(hofpitching, MidYear >= 1960)
hofpitching.recent <- hofpitching.recent[order(hofpitching.recent$WAR.Season,
                                               decreasing=TRUE),]
dotchart(hofpitching.recent$WAR.Season, labels=hofpitching.recent$X,
         xlab="WAR / Season")

# 5. HoF Pitching - continued
with(hofpitching, plot(MidYear, WAR.Season))
with(hofpitching, lines(lowess(MidYear, WAR.Season, f=1.0)))
with(hofpitching, identify(MidYear, WAR.Season, X, n=2)) # Monte Ward, Hank O'Day

# 6. Working with the Lahman Batting Dataset
library(Lahman)

getinfo <- function(firstname, lastname){
  playerline <- subset(Master, nameFirst==firstname & nameLast==lastname)
  name.code <- as.character(playerline$playerID)
  birthyear <- playerline$birthYear
  birthmonth <- playerline$birthMonth
  birthday <- playerline$birthDay
  byear <- ifelse(birthmonth <=6, birthyear, birthyear + 1)
  
  list(name.code=name.code, byear=byear)
}

cobb <- getinfo("Ty", "Cobb")
williams <- getinfo("Ted", "Williams")
# Two Pete Roses in Master, pull record seperately
rose.playerline <- subset(Master, playerID=="rosepe01")
birthyear <- rose.playerline$birthYear
birthmonth <- rose.playerline$birthMonth
byear <- ifelse(birthmonth <=6, birthyear, birthyear + 1)
rose <- data.frame(name.code="rosepe01", byear=byear)

cobb.stat <- subset(Batting, playerID==cobb$name.code)
williams.stat <- subset(Batting, playerID==williams$name.code)
rose.stat <- subset(Batting, playerID==rose$name.code)

cobb.stat$Age <- cobb.stat$yearID - cobb$byear
williams.stat$Age <- williams.stat$yearID - williams$byear
rose.stat$Age <- rose.stat$yearID - rose$byear

plot(rose.stat$Age, cumsum(rose.stat$H), type="l", xlab="Age",
     ylab="Cumulative Hit Total", las=1, lty=1, lwd=2,
     xlim=c(18,45))
with(cobb.stat, lines(Age, cumsum(H), lty=2, lwd=2))
with(williams.stat, lines(Age, cumsum(H), lty=3, lwd=2))
legend(20, 4000, legend=c("Rose", "Cobb", "Williams"), lty=1:3, lwd=2)


# 7. Working with Retrosheet Play-by-Play data
data1998 <- read.csv("all1998.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1998) <- fields[, "Header"]

retro.ids <- read.csv("retrosheetIDs.csv")
sosa.id <- as.character(subset(retro.ids, LAST=="Sosa" & FIRST=="Sammy")$ID)
mac.id <- as.character(subset(retro.ids, LAST=="McGwire" & FIRST=="Mark")$ID)

sosa.data <- subset(data1998, BAT_ID==sosa.id)
mac.data <- subset(data1998, BAT_ID==mac.id)

# Restrict to plays where a batting event occurred
sosa.data <- subset(sosa.data, BAT_EVENT_FL==TRUE)
mac.data <-subset(mac.data, BAT_EVENT_FL==TRUE)

# Number each plate appearance
sosa.data$PA <- 1:nrow(sosa.data)
mac.data$PA <- 1:nrow(mac.data)

mac.HR.PA <- mac.data$PA[mac.data$EVENT_CD==23]
sosa.HR.PA <- sosa.data$PA[sosa.data$EVENT_CD==23]

# Count spaces between plate appearances with HR
mac.spacings <- diff(c(0, mac.HR.PA))
sosa.spacings <- diff(c(0, sosa.HR.PA))

summary(mac.spacings)
summary(sosa.spacings)

hist(mac.spacings)
hist(sosa.spacings)
