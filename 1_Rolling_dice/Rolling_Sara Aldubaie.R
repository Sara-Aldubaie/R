# Vector with numerical values from 1:10
dice_10 <- 1:10

# roll_6 is a sample with a length of 6 from vector dice_10
roll_6<- sample(x = dice_10, size = 6, replace = TRUE)

# roll_6 is a sample with a length of 10 from vector dice_10
roll_10<- sample(x = dice_10, size = 10, replace = TRUE)

# sum all values inside roll_6 
sum(roll_6)

# sum all values inside roll_10 
sum(roll_10)


######## first section End ########


# Vector with numerical values from 1:20
dice_20 <- 1:20

# roll_20 is a sample with a length of 6 from vector dice_20
roll_20<- sample(x = dice_20, size = 10, replace = TRUE)


# calculate how many dice rolled more than 6 (for the 10 sided)

counter_10 = 0
for (x in roll_10) {
  
  if (x > 6){ 
    counter_10 = counter_10 +1 
  }
}
paste("Number of 10 sided dice rolled more than 6:",counter_10)


# calculate how many dice rolled more than 16 (for the 20 sided)

counter_20 = 0
for (x in roll_20) {
  
  if (x > 16){ 
    counter_20 = counter_20 +1 
  }
}
paste("Number of 20 sided dice rolled more than 16:",counter_20)
    