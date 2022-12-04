## Day 1
#dependencies
library(readxl)
library(tidyverse)
#Import data
setwd(file.path("C:\\Users\\leono\\OneDrive\\Documents\\Advent of Code")) 
file.list <- list.files(path = "C:\\Users\\leono\\OneDrive\\Documents\\Advent of Code",pattern='*.xlsx', recursive=TRUE)
df.list <- lapply(file.list, read_excel)
input <- df.list[[1]]
#Number of elves
sum(is.na(input))+1
#Number of elves=254
p <- unname(split(input[!is.na(input)], cumsum(is.na(input))[!is.na(input)]))
r <- sapply(p, sum) 
max(r)
# Follow up puzzle
# For later reference it might be fun to know which Elf it was
q <- 1:254
elftrack <- data.frame(q,r)
elftrack <- elftrack[order(elftrack$r, decreasing=TRUE),]
head(elftrack)
sum(elftrack$r[1:3])
#Elves to talk to
greedymanzs <- list(elftrack$q[1:3])
greedymanzs

## Day 2
#Dependencies
install.packages("berryFunctions")
library ("berryFunctions")
#Loading input
A <- read_excel("C:\\Users\\leono\\OneDrive\\Documents\\Advent of Code\\Day2.xlsx")
head(A)
#Move the first entry into the first row
A <- insertRows(A, 1 , new = NA)
A[1,] <- colnames(A)
head(A)
#Rename top row
A <- rename(A,"Opponent" = `B X`)
#Split the choices
A <- str_split_fixed(A$Opponent," ",2)
#Fixing matrix array object
A <- as.data.frame(A)
A <- rename(A,"Opponent" = V1)
A <- rename(A,"Yourchoice" = V2)
head(A)
# The problem itself
## Creating some empty rows
A$Play <- NA
A$Win <- NA
#Create the default playing scores of the strategy
A <- A %>%
  mutate(Play = case_when(
    Yourchoice == "X" ~ 1,
    Yourchoice == "Y" ~ 2,
    Yourchoice == "Z" ~ 3,
  ))
#Reminder of the codes
# X=Rock, Y=Paper, Z=Scissors
# A=Rock, B=Paper, C=Scissors
A <- A %>%
  mutate(Win = case_when(
    Yourchoice == "X" & Opponent == "C" ~ 6,
    Yourchoice == "Y" & Opponent == "A" ~ 6,
    Yourchoice == "Z" & Opponent == "B" ~ 6,
    Yourchoice == "X" & Opponent == "A" ~ 3,
    Yourchoice == "Y" & Opponent == "B" ~ 3,
    Yourchoice == "Z" & Opponent == "C" ~ 3))
head(A)
A$Win <- replace_na(A$Win,0)
#Checking that everything is filled out
sum(is.na(A))
#Calculating score
A$Finalscore <- A$Play+A$Win
sum(A$Finalscore)
#The final score is 8933

##Part 2
# Intel: "Anyway, the second column says how the round needs to end: X means 
# you need to lose, Y means you need to end the round in a draw, and Z means 
# you need to win. "
B <- A
#Changing the first variable to be easier
B["Opponent"][B["Opponent"] == "A"] <- "Rock"
B["Opponent"][B["Opponent"] == "B"] <- "Paper"
B["Opponent"][B["Opponent"] == "C"] <- "Scissors"
#Changing the outcome
B["Yourchoice"][B["Yourchoice"] == "X"] <- "Lose"
B["Yourchoice"][B["Yourchoice"] == "Y"] <- "Draw"
B["Yourchoice"][B["Yourchoice"] == "Z"] <- "Win"
#Relocate win column and change scores
B <- relocate(B,Win,.before = Play)
B <- B %>%
  mutate(Win = case_when(
    Yourchoice == "Lose" ~ 0,
    Yourchoice == "Draw" ~ 3,
    Yourchoice == "Win"  ~ 6))
#Adding optimal play
B <- B %>%
  mutate(Play = case_when(
    Opponent == "Rock" & Yourchoice == "Win" ~ "Paper",
    Opponent == "Rock" & Yourchoice == "Lose" ~ "Scissors",
    Opponent == "Paper" & Yourchoice == "Win" ~ "Scissors",
    Opponent == "Paper" & Yourchoice == "Lose" ~ "Rock",
    Opponent == "Scissors" & Yourchoice == "Win" ~ "Rock",
    Opponent == "Scissors" & Yourchoice == "Lose" ~ "Paper",
    Yourchoice == "Draw" ~ Opponent
  ))
# Adding score
B$Playscore <- NA
B <- relocate(B,Playscore,.before = Finalscore)
B <- B %>%
  mutate(Playscore = case_when(
    Play == "Rock" ~ 1,
    Play == "Paper" ~ 2,
    Play == "Scissors" ~3
  ))
B$Finalscore <- B$Win+B$Playscore
sum(B$Finalscore)

## Day 3 
library ("berryFunctions")
library(readxl)
library(tidyverse)
#Formatting
CC <- read_excel("C:\\Users\\leono\\OneDrive\\Documents\\Advent of Code\\Day3.xlsx")
CC <- insertRows(CC, 1 , new = NA)
CC[1,] <- colnames(CC)
CC <- rename(CC,"Input" = FqdWDFppHWhmwwzdjvjTRTznjdMv)
as.data.frame(CC)
#function to check even
even <- function(x){
  if(nchar(x)%%2==0){
    "Even"
  }else{
    "Uneven"
  }}
#test
h <- apply(CC,1,even)
length(grep("Even", h))
#All 300 entries have an even number of characters
CC$Output1 <- NA
splitchar <- function(x){strsplit(x,"")[[1]]}
bre <- function(i){
  i <- splitchar(i) 
  if(sum(nchar(i))%%2 == 0){
    a <- (i[0:((0.5*(sum(nchar(i)))))])
    b <- (i[((0.5*(sum(nchar(i))))+1):length(i)])
    print(intersect(a,b))
  }else{
    a <- (i[0:((0.5*(sum(nchar(i)))))])
    b <- (i[((0.5*(sum(nchar(i))))+1):length(i)])
    print(intersect(a,b))
  }
}
CC$Output1[1:300] <- apply(CC,1,bre)
#Score calculation
CC$Output2 <- NA
view(CC)
Scorecalc <- data.frame(char=c(letters),num=c(1:26))
Scoretemp <- data.frame(char=c(LETTERS),num=c(27:52))
Scorecalc <- rbind(Scorecalc,Scoretemp)
finder <- function(x){which(grepl(x, Scorecalc$char))} 
CC$Output2 <- lapply(CC$Output1,finder)
view(CC)
#Fucking finally
CC$Output2<-as.numeric(CC$Output2)
sum(CC$Output2)
#Answer is 8243

#Day 3 - part 2
