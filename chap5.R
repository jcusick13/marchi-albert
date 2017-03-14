# examples5.R

# 5.2 Runs scored in the remainder of an inning
setwd('Documents/baseball/r/data')
# Read in play-by-play data for 2011
data2011 <- read.csv("all2011.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data2011) <- fields[, "Header"]

# Count current runs and create ID for each half inning
data2011$RUNS <- with(data2011, AWAY_SCORE_CT + HOME_SCORE_CT)
data2011$HALF.INNING <- with(data2011,
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))

# Sum runs scored on each play
data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
                               (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))
# Sum total runs scored during each half inning
RUNS.SCORED.INNING <- aggregate(data2011$RUNS.SCORED,
                                list(HALF.INNING=data2011$HALF.INNING), sum)
# Total runs scored at the start of each half inning
RUNS.SCORED.START <- aggregate(data2011$RUNS,
                               list(HALF.INNING=data2011$HALF.INNING), "[", 1)


# Count of total runs scored at end of each half inning
MAX <- data.frame(HALF.INNING=RUNS.SCORED.START$HALF.INNING)
MAX$x <- RUNS.SCORED.INNING$x + RUNS.SCORED.START$x
data2011 <- merge(data2011, MAX)
# Rename max runs column
N <- ncol(data2011)
names(data2011)[N] <- "MAX.RUNS"

# Runs scored in remainder of inning for each plate appearance
data2011$RUNS.ROI <- with(data2011, MAX.RUNS - RUNS)

# 5.3 Runs expectancy matrix

# Record status of each base
RUNNER1 <- ifelse(as.character(data2011[, "BASE1_RUN_ID"]) == "", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[, "BASE2_RUN_ID"]) == "", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[, "BASE3_RUN_ID"]) == "", 0, 1)

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)
}

# Create binary representation of baserunners
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

# Record if a new runner is on each base after a play
# *Why do we include the runner starting on base when looking for a change
# *in status of that base?
NRUNNER1 <- with(data2011, as.numeric(BAT_DEST_ID==1 | RUN1_DEST_ID==1))
NRUNNER2 <- with(data2011, as.numeric(BAT_DEST_ID==2 | RUN1_DEST_ID==2 |
                                        RUN2_DEST_ID==2))  
NRUNNER3 <- with(data2011, as.numeric(BAT_DEST_ID==3 | RUN1_DEST_ID==3 |
                                        RUN2_DEST_ID==3 | RUN3_DEST_ID==3))  
NOUTS <- with(data2011, OUTS_CT + EVENT_OUTS_CT)

# Update binary base representation after play
data2011$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

# Restrict query to plays where runs scored or change in state
data2011 <- subset(data2011, (STATE != NEW_STATE) |
                     (RUNS.SCORED > 0))

# Remove innings with less than 3 outs
library(dplyr)
data.outs <- summarise(group_by(data2011, HALF.INNING), Outs.Inning=sum(EVENT_OUTS_CT))
data2011 <- merge(data2011, data.outs)
data2011c <- subset(data2011, Outs.Inning == 3)


# Count expected runs for each state of bases/outs
RUNS <- with(data2011c, aggregate(RUNS.ROI, list(STATE), mean))
RUNS$Outs <- substr(RUNS$Group.1, 5, 5)
RUNS <- RUNS[order(RUNS$Outs),]

# Reformat data frame into matrix of base state/outs
RUNS.out <- matrix(round(RUNS$x, 3), nrow=8, ncol=3)
dimnames(RUNS.out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS.out)[[1]] <- c("000", "001", "010", "011", "100",
                             "101", "110", "111")
# *How to order matrix by individual column value?

# 5.4 Measuring success of a batting play
# Run Value = Runs(New State) - Runs(Old State) + Runs(Scored on Play)

# Add zero run value for states with 3 outs
RUNS.POTENTIAL <- matrix(c(RUNS$x, rep(0, 8)), nrow=32, ncol=1)
dimnames(RUNS.POTENTIAL)[[1]] <- c(RUNS$Group.1, "000 3", "001 3", "010 3",
                              "011 3", "100 3", "101 3", "110 3", "111 3")
# Run expectancy of current state and future state
data2011$RUNS.STATE <- RUNS.POTENTIAL[data2011$STATE,]
data2011$RUNS.NEW.STATE <- RUNS.POTENTIAL[data2011$NEW.STATE,]
