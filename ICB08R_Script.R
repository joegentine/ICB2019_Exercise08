#HOMEWORK 8
#Made by Joe Gentine and Andrew Cameron 

#Graph or Running Score  
setwd("~/Desktop/r-novice-inflammation/ICB_Exercises/ICB2019_Exercise08/")
data <- read.table(file="UWvMSU_1-22-13.txt", sep="\t", header=TRUE)
#Creates file from original data that adds scores over time
a<- 0
b<-0
RunningScore<- data.frame(Time=0, Team= 0, Score=0)
RunningScore<- rbind(RunningScore, c(0, "UW", 0))
RunningScore<- rbind(RunningScore, c(0, "MSU", 0))
for(i in 1:nrow(data)){
  if(data$team[i]=="UW"){
    a<- a +data$score[i]
    RunningScore<- rbind(RunningScore, c(data$time[i], "UW", a))
  }
    else if(data$team[i]=="MSU"){
      b<- b +data$score[i]
      RunningScore<- rbind(RunningScore, c(data$time[i], "MSU", b))
    }
}
#Cleans data to graph
RunningScore<-transform(RunningScore, Time=as.numeric(Time))
RunningScore<-transform(RunningScore, Score=as.numeric(Score))
RunningScore<- transform(RunningScore, Team=as.factor(Team))
#Graphs data
library(ggplot2)
GameGraph<-ggplot(data=RunningScore, mapping = aes(x=Time, y=Score))
GameGraph+geom_line(aes(color=Team))


##Made by Andrew Cameron and Joe Gentine

## Set maximum value for guess range (min is 1)
maxVal <- 100

## Set maximum number of guesses before game over
maxGuess <- 10

## Randomly choose integer in between 1 and maxGuess
target <- as.numeric(sample(1:maxVal , 1))

## Reset guess count.
guessNo <- 0

## Read and evaluate user console input until correct answer is reached (Victory!) or no guesses remain (Failure!).
repeat {
  guessNo <- (guessNo+1)
  guessRem <- (maxGuess-guessNo)
  input <- as.numeric(readline(prompt = cat("Guess a number between 1 and " , maxVal , ". ", sep="")))
  if (input>target){
    cat("Too high. " , guessRem , " guesses remaining. ")}else if(input<target){
      cat("Too low. " , guessRem , " guesses remaining. ")}else{
        if (guessNo == 1){cat("Correct! You took " , guessNo , "try to find the right answer.")}
        else{cat("Correct! You took " , guessNo , "tries to find the right answer.")}
        break}
  if (guessRem == 0){cat("Game over! Correct answer: " , target)
    break}
}
