#### NAME: Sara Aldubaie ####

# get csv file 
deck <- read.csv("C:/Users/SarOonh/Dropbox/Data Science Bootcamp/week 4/2/deck.csv")
# view the csv file 
View(deck)

# first function
Shuffle <- function(cards){
  data <- cards[sample(1:nrow(cards)), ]
  
}

card_shuffle <- Shuffle(deck)
View(card_shuffle)

# second function


deal <- function(){
  # please enter number of player in console
  number_players <- readline(prompt= "Enter numbers of players:")
  number_players <<- number_players
  players <<- as.integer(number_players)
  
  # if there is no enough cards for every player 
  if (players*5 > 52){
    print("sorry there is no enough cards")
    
    
    # else  assign 5 cards for each player and put the result into dataframe
  }else{
    i <- 1
    Data_Frame <- data.frame (player = c(i,i,i,i,i) )
    
    while (i < players) { # i is number of player
      i <- i + 1
      x <- 1
      while (x <= 5) { # x is number of cards given to player 
        
        Data_Frame  <- rbind(Data_Frame, c(i))
        x <- x + 1
      } 
    } 
    
    # save result in a dataframe
    suppressWarnings(rm(deal_Data_Frame)) # drop deal df if existing  
    Data_Frame 
    set.seed(12345) 
    data_s1 <- card_shuffle[sample(1:nrow(card_shuffle), players*5), ]  
    data_s1   
    deal_Data_Frame <<- cbind(Data_Frame,data_s1)
    return(deal_Data_Frame)
  }
}
# run the function deal
deal()

######## END FIRST PART  ########

# function for printing the result of the game 
result <- function(){
# split dataframe depending on player number  
split_deal = split(deal_Data_Frame, with(deal_Data_Frame, interaction(player)), drop = TRUE)

# empty dataframe to save players value in it if a new round needed 
players_values <- data.frame ()

# represent the player 
count = 1
# add first player value into dataframe 

players_values <- data.frame (
  player = c(count),
  value = c(as.integer(sum(split_deal[[count]]$value))))
count <<- count +1

# add the rest of  players value 
while (count <= players) {
  
  
  x = as.integer( sum(split_deal[[count]]$value))
  
  players_values <- rbind(players_values, c(count,x))
  count = count + 1
} 

# sort result so winner player will be at top 
players_values<- players_values[order(-players_values$value),]

# get winner result 
winner <- players_values[1, 1]
# get failure players 
failure <- as.data.frame.character((players_values[2:players, 1]))
# remove "C()" to print the result in more orgnized way  
failure<- gsub("^c\\(|\\)$", "", failure)
# winner string 
print(paste("Player", winner ,"has more points than Player:", failure ))

# print each player with their points 
i<- 1
while(i <= players) {
 
    print(paste("Player", players_values[i, 1], "points:",players_values[i, 2] ))
  i = i + 1
}

}
#execute the function 
result()


