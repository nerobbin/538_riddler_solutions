#This script is a solution to the Riddler Express problem from 20 Dec 2019 
#(https://fivethirtyeight.com/features/can-you-find-a-matching-pair-of-socks/)

#I'm treating "x" as the number of people in the office,
#"a" as the number of people who voted for the 1st place theme (73%),
#"b" as the number for 2nd place (58%),
#and "c" as the number for 3rd place (32%).

#All 4 of these must be whole numbers.
#My approach is to take a range of whole-number values for x, 
#calculate a/b/c, and check if all 3 are whole numbers.

#Make data frame to store calculation results
df <- data.frame(x=NA, a1=NA, a2=NA, b1=NA, b2=NA, c1=NA, c2=NA, wholenum=NA)

#Run calculations on a set of values for x
for(i in 1:1000){
  
  #Store value of x
  df[i, "x"] <- i
  
  #Initialize "wholenum" to 0. This tracks whether the whole-number condition is satisfied.
  wholenum <- 0
  
  #Since the survey percentages were rounded down, a/b/c can take on multiple values within an interval.
  #Calculate the interval of values for each variable.
  a1 <- 0.73*i
  a2 <- 0.74*i
  b1 <- 0.58*i
  b2 <- 0.59*i
  c1 <- 0.32*i
  c2 <- 0.33*i
  
  #Save values to table
  df[i, "a1"] <- a1
  df[i, "a2"] <- a2
  df[i, "b1"] <- b1
  df[i, "b2"] <- b2
  df[i, "c1"] <- c1
  df[i, "c2"] <- c2
  
  #Does [a1, a2) contain a whole number?
  ##Check if a1 is whole
  if(a1 %% 1 == 0) wholenum <- 1
  ##Check if interval between a1 and a2 contains a whole number
  if(
    floor(a2) - floor(a1) > 1 | #interval spans more than 1 whole number
    (floor(a2) - floor(a1) == 1 & a2 %% 1 != 0) #interval spans 1 whole number, and that number is not a2
  ) wholenum <- 1
  
  #If [a1, a2) passes the whole-number check, check [b1, b2)
  if(wholenum == 1){
    wholenum <- 0 #set tracker to 0. If check is passed, it will be changed to 1.
    if(b1 %% 1 == 0) wholenum <- 1
    if(
      floor(b2) - floor(b1) > 1 |
      (floor(b2) - floor(b1) == 1 & b2 %% 1 != 0)
    ) wholenum <- 1
  }
  
  #If [a1, a2) and [b1, b2) pass the whole-number check, check [c1, c2)
  if(wholenum == 1){
    wholenum <- 0 #set tracker to 0. If check is passed, it will be changed to 1.
    if(c1 %% 1 == 0) wholenum <- 1
    if(
      floor(c2) - floor(c1) > 1 |
      (floor(c2) - floor(c1) == 1 & c2 %% 1 != 0)
    ) wholenum <- 1
  }
  
  #Save value of "wholenum" tracker
  df[i, "wholenum"] <- wholenum
}

#Show min value of x where "wholenum" = 1.
#This is the minimum number of people in the office that could yield the survey results.
min(df[df$wholenum==1, "x"])

#Show max value of x where "wholenum" = 0
#This is the maximum number of people that could NOT yield the survey results.
max(df[df$wholenum==0, "x"])
