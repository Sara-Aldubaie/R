deck <- read.csv("C:/Users/SarOonh/Dropbox/Data Science Bootcamp/week 4/2/deck.csv")
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

