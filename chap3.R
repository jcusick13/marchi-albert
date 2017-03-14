setwd("Documents/baseball/r/data")

# 3.2 Factor Variable
hof <- read.csv("hofbatting.csv")
head(hof)
hof$MidCareer <- with(hof, (From + To) / 2)
hof$Era <- cut(hof$MidCareer,
               breaks=c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
               labels=c("19th Century", "Dead Ball", "Lively Ball", "Integration",
                        "Expansion", "Free Agency", "Long Ball"))

barplot(table(hof$Era), xlab="Era", ylab="Frequency",
        main="Era of the Nonpitching Hall of Famers")
plot(table(hof$Era))
pie(table(hof$Era))

# 3.4 Dot Plots
hof.500 <- subset(hof, HR >=500)
head(hof.500)
hof.500 <- hof.500[order(hof.500$OPS),]
dotchart(hof.500$OPS, labels=hof.500$X, xlab="OPS")

# 3.5 Stripchart
quartz(width=7, height=3.5)
stripchart(hof$MidCareer, method="jitter", pch=1, col="blue", xlab="Mid Career")

with(hof, plot(MidCareer, OPS))
with(hof, lines(lowess(MidCareer, OPS, f=0.3)))
with(hof, identify(MidCareer, OPS, X, n=4))

# 3.6 Scatterplot
with(hof, plot(OBP, SLG))
col.pal <- colorRampPalette(c("Red", "Blue"))
hof$Col <- col.pal(6)[as.numeric(cut(hof$OPS,
                                     breaks=c(0.6, 0.7, 0.8, 0.9, 2.0)))]
with(hof, plot(OBP, SLG, xlim=c(0.25, 0.50),
               ylim=c(0.28, 0.75), pch=1, 
               xlab="On-Base Percentage", 
               ylab="Slugging Percentage",
               col=Col))
curve(0.7 - x, add=TRUE)
curve(0.8 - x, add=TRUE)
curve(0.9 - x, add=TRUE)
curve(1.0 - x, add=TRUE)

# 3.7 Parallel charts
hof$HR.Rate <- with(hof, HR/AB)
par(plt=c(0.2, 0.94, 0.145, 0.883))
stripchart(HR.Rate ~ Era, data=hof, las=2,
           pch=1, method="jitter")
boxplot(HR.Rate ~ Era, data=hof, las=2,
        horizontal=TRUE, xlab="HR Rate")

# 3.8 Comparing Sluggers
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

ruth.info <- getinfo("Babe", "Ruth")
aaron.info <- getinfo("Hank", "Aaron")
bonds.info <- getinfo("Barry", "Bonds")
arod.info <- getinfo("Alex", "Rodriguez")

ruth.data <- subset(Batting, playerID==ruth.info$name.code)
ruth.data$Age <- ruth.data$yearID - ruth.info$byear
aaron.data <- subset(Batting, playerID==aaron.info$name.code)
aaron.data$Age <- aaron.data$yearID - aaron.info$byear
bonds.data <- subset(Batting, playerID==bonds.info$name.code)
bonds.data$Age <- bonds.data$yearID - bonds.info$byear
arod.data <- subset(Batting, playerID==arod.info$name.code)
arod.data$Age <- arod.data$yearID - arod.info$byear

with(ruth.data, plot(Age, cumsum(HR), type="l", lty=3, lwd=2,
                     xlab="Age", ylab="Career Home Runs",
                     xlim=c(18, 45), ylim=c(0, 800)))
with(aaron.data, lines(Age, cumsum(HR), lty=2, lwd=2))
with(bonds.data, lines(Age, cumsum(HR), lty=1, lwd=2))
with(arod.data, lines(Age, cumsum(HR), lty=4, lwd=2))
legend(20, 700, legend=c("Bonds", "Aaron", "Ruth", "ARod"),
       lty=1:4, lwd=2)

# 3.9 1998 Home Run Race
data1998 <- read.csv("all1998.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1998) <- fields[, "Header"]

retro.ids <- read.csv("retrosheetIDs.csv")
sosa.id <- as.character(subset(retro.ids, LAST=="Sosa" & FIRST=="Sammy")$ID)
mac.id <- as.character(subset(retro.ids, LAST=="McGwire" & FIRST=="Mark")$ID)

sosa.data <- subset(data1998, BAT_ID==sosa.id)
mac.data <- subset(data1998, BAT_ID==mac.id)

createdata <- function(d){
  d$Date <- as.Date(substr(d$GAME_ID, 4, 11), format="%Y%m%d")
  d <- d[order(d$Date), ]
  d$HR <- ifelse(d$EVENT_CD==23, 1, 0)
  d$cumHR <- cumsum(d$HR)
  
  d[, c("Date", "cumHR")]
}

sosa.hr <- createdata(sosa.data)
mac.hr <- createdata(mac.data)

plot(mac.hr, type="l", lwd=2, ylab="Home Runs in the Season")
lines(sosa.hr, lwd=2, col="grey")
abline(h=62, lty=3)
text(10440, 65, "62")
legend(10440, 20, legend=c("McGwire (70)", "Sosa (66)"), lwd=2,
       col=c("black", "grey"))
