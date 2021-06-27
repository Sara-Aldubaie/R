## _______ Wejdan Al-Ahmadi _______##
## _______   Sara Aldubaie  _______##
## _______   Norah Alharthi  _______##

## -------------------  Data import -------------------##

#setting the director 

setwd("C:/Users/SarOonh/Dropbox/Data Science Bootcamp/week 4/project/New folder/data") 

# importing data files 

theater_1_data <- read.csv("theater.csv", header = TRUE,fileEncoding="UTF-8-BOM")

theater_2_data <- read.csv("theater_2.csv", header = TRUE,fileEncoding="UTF-8-BOM")

snack_menu_Data <- read.csv("snack.csv", header = TRUE,fileEncoding="UTF-8-BOM")

movie_list_Data <- read.csv("movie.csv", header = TRUE)

membership_Data <- read.csv("membership.csv", header = TRUE,fileEncoding="UTF-8-BOM")

## ---------------------------------------  Week 4 - Day 19 - Assignment (3)  ---------------------------------------##



# ---------------------------------------- Assignment Questions ---------------------------------------- # 


## -------------------  Task (1) - Make sure you don't have more people attending than your seats  -------------------##
# This was done in the excel sheet 

## -------------------  Task (2) - The amount of adult and children in the theater should be randomized  -------------------##
# This was done in the excel sheet

## -------------------  Task (3.3) - Create conditional statements for movies that may be PG-13 and children are not allowed to watch  -------------------##
# This was done in the excel sheet



# -------------------- Variables for Theaters -------------------- #


# This is the number of days of 1 week
Days_of_week <- 7



# This dataframe variable is for the total revenue for each day for theater 1
total_revenue_theater_1 <- data.frame(matrix(ncol = 4, nrow = 0))
name <- c("Day", "Ticket_Rev", "Snack_Rev", "Total_Rev")
colnames(total_revenue_theater_1) <- name

# This dataframe variable is for the total revenue for each day for theater 2
total_revenue_theater_2 <- data.frame(matrix(ncol = 4, nrow = 0))
name <- c("Day", "Ticket_Rev", "Snack_Rev", "Total_Rev")
colnames(total_revenue_theater_2) <- name

# This dataframe variable is for the total loss for each day for theater 1
total_loss_theater_1 <- data.frame(matrix(ncol = 4, nrow = 0))
name <- c("Day", "total_ticket_loss", "Snack_loss", "Total_loss")
colnames(total_loss_theater_1) <- name

# This dataframe variable is for the total loss for each day for theater 2
total_loss_theater_2 <- data.frame(matrix(ncol = 4, nrow = 0))
name <- c("Day", "total_ticket_loss", "Snack_loss", "Total_loss")
colnames(total_loss_theater_2) <- name

# This dataframe variable is for the membership
membership_DF <- data.frame(matrix(ncol = 6, nrow = 0))
name <- c("customer_ID", "streak", "total_months_subbed", "total_points","vouchers","movies_left")
colnames(membership_DF) <- name

# This dataframe variable is for the membership rewards 
reward <- c("one_extra_movie", "one_extra_snack","10%_discount")
points_cost <- c(25,5,10)
membership_rewards <- data.frame(reward,points_cost)



# price of tickets for theater 1
ticket_price_adult_1 <- 12.99
ticket_price_child_1 <- 8.99

# price of tickets for theater 2
ticket_price_adult_2 <- 15.99
ticket_price_child_2 <- 13.99

# ----------------------------------------------------------------- #



## -------------------  Task: Create snacks that the customers can buy and randomize who buys which snack -------------------##
snack <- function(snack_data, num_customers, quantity, Day){
  
  # This dataframe variable is for the snack revenue for each snack for each day
  snack_revenue_DF <- data.frame(matrix(ncol = 3, nrow = 0))
  name <- c("Day","snack", "price")
  colnames(snack_revenue_DF) <- name
  
  # This for loop iterates through the snack menu data and calculate the total of each snack bought for each day 
  for (row in 1:nrow(snack_menu_Data)) {
    
    snack_name <- snack_menu_Data[row, 1] # get snack name from table
    
    snack_price <- snack_menu_Data[row, 2] # get snack price from table
    
    num_sold_snack <- num_customers*quantity # multiply the number of customers snacking with a random quantity
    
    snack_revenue <- snack_price * num_sold_snack # calculate the revenue for each snack
    
    snack_revenue_DF <- rbind(snack_revenue_DF, data.frame(Day,snack_name, snack_revenue )) # add all the snack information to the snack revenue dataframe
    
    
  }
  
  return(snack_revenue_DF)
}


# ----------------------------------------------------------------- #


# function for finding the revenue from the ticket sales 

weekly_revenue <- function(theater_data_DF,ticket_price_adult,ticket_price_child,total_revenue_theater_N){
  
  # iterate through the week
  for (i in 1:Days_of_week) {
    
    # Keep track of total revenue for the day for the tickets
    Ticket_Rev<-0
    
    # for naming conventions for the dataframe 
    Day <- i
    
    # ----- Total Ticket Sales Calculation ----- # 
    
    # -To access the first 5 elements, I access the data frame using the days == i
    
    # total number of adults for each day
    total_number_adults <- sum(theater_data_DF[theater_data_DF$Day==i,c("Num_Adult")]) 
    
    # total number of children for each day
    total_number_children <- sum(theater_data_DF[theater_data_DF$Day==i,c("Num_Children")]) 
    
    # total ticket sales for adults for each day
    adult_ticket_sales <- total_number_adults * ticket_price_adult
    
    # total ticket sales for children for each day
    children_ticket_sales <- total_number_children * ticket_price_child
    
    # total ticket sales for each day
    Ticket_Rev <- adult_ticket_sales + children_ticket_sales
    
    
    # ----- Total Snack Sales Calculation ----- # 
    
    # To randomize who buys snacks first we sum the total number of customers for each day and then sample a number of them to buy snacks 
    
    # Total number of customers who visit the theater each day
    num_of_customers <- sum(theater_data_DF[theater_data_DF$Day==i,c("Seats_Occupied")])
    
    
    ## -------------------  Task (3.1) - Create snacks that the customers can buy and randomize who buys which snack  -------------------##
    
    
    # Total Random number of customers who will buy snacks that day
    num_of_customers_snacking <- sample(20:num_of_customers,1)
    
    # This is to randomize the quantity each customers buys each day 
    random_quantity_snacks <- sample(3:7,1)
    
    
    snack_revenue_DF <- snack(snack_menu_Data,num_of_customers_snacking,random_quantity_snacks,Day)
    
    
    
    # this sums the total revenue for all the snacks for each day
    Snack_Rev = sum(snack_revenue_DF[snack_revenue_DF$Day==i,3])  
    print(Snack_Rev)
    # the total revenue is calculated by adding the total snack revenue and the total ticket revenue
    Total_Rev <- Snack_Rev+Ticket_Rev 
    
    # We add all the information in one dataframe for each day 
    total_revenue_theater_N <- rbind(total_revenue_theater_N, data.frame(Day,Ticket_Rev,Snack_Rev, Total_Rev))
    
    
  }
  
  
  out <- total_revenue_theater_N
  
  return(out)
}


#function call for calculating the weekly revenues for theater 1
total_revenue_theater_1 <- weekly_revenue(theater_1_data,ticket_price_adult_1,ticket_price_child_1,total_revenue_theater_1)

#function call for calculating the weekly revenues for theater 2
total_revenue_theater_2 <- weekly_revenue(theater_2_data,ticket_price_adult_2,ticket_price_child_2,total_revenue_theater_2)


#-------------------------------------------------------------------------------------------------#

## -------------------  Task (3.6) -      Which day had the highest revenue?        -------------------##

#function to find highest revenue
max_revenue_f <- function(total_rev_theater_N){
  
  # iterate through the week
  for (i in 1:Days_of_week){
    
    #find the max value
    max_revenue <- max(total_rev_theater_N$Total_Rev, na.rm = TRUE)
    
    #find the index of the max value
    index_of_max_revenue <- which.max(total_rev_theater_N$Total_Rev)
    
    result <- print(paste("The highest revenue of the week is on day", index_of_max_revenue,
                          "at value of: ", max_revenue ))
    return(result)
    
  }
}



#calling "max_revenue_f" function
max_revenue_of_the_week_th_1 <- max_revenue_f(total_revenue_theater_1)
max_revenue_of_the_week_th_2 <- max_revenue_f(total_revenue_theater_2)



#-------------------------------------------------------------------------------------------------#


# Function to calculate the loss from the empty seats 

calc_lost <- function(theater_data_DF,ticket_price_adult,ticket_price_child, total_loss_theater_N){
  
  #calculate the amount of empty seats and how much they would bring 
  
  for (i in 1:Days_of_week) {
    
    # Keep track of total revenue for the day for the tickets
    Ticket_Rev<-0
    
    # for naming conventions for the dataframe 
    Day <- i
    
    # ------------- Total Ticket Sales Calculation  ------------- #
    
    # -To access the first 5 elements, I access the data frame using the days == i
    
    # first we get the total amount of seats occupied for each day
    seats_occupied <- sum(theater_data_DF[theater_data_DF$Day==i,c("Seats_Occupied")])
    
    # then we get the total amount of seats available for each day 
    total_seats_available <- sum(theater_data_DF[theater_data_DF$Day==i,c("Total_Seats_Available")]) 
    
    # subtracting these two variables gives us the seats left un-occupied for each day
    seats_available <- total_seats_available - seats_occupied
    
    # now to divide the available seats for adults and children on a 75 to 25 ratio 
    seats_available_adults <- seats_available * 0.75
    seats_available_children <- seats_available * 0.25
    
    # now to calculate the cost of those seats based on the price 
    cost_seats_available_adults <- seats_available_adults * ticket_price_adult
    cost_seats_available_children <- seats_available_children * ticket_price_child
    
    # total cost of seats available (this will be the loss since they arent occopied)
    total_ticket_loss <- cost_seats_available_adults + cost_seats_available_children
    
    # now that we have the total number of customers we could of had and the ticket prices we could of sold its time to calculate the snacks we could of sold

        
    # ------------- Snack Loss Calculation  ------------- #
    
    
    # This is to randomize the quantity each customers buys each day 
    random_quantity_snacks <- sample(3:7,1)
    
    # to find out the loss of profits we could of had from snacks we use the snack function and store it in a temp DF 
    snack_loss_DF<- snack(snack_menu_Data,seats_available,random_quantity_snacks,Day)
    
    
    # this sums the total loss for all the snacks for each day
    Snack_loss = sum(snack_loss_DF[snack_loss_DF$Day==i,3])  
    
    # the total loss is calculated by adding the total snack loss and the total ticket loss
    Total_loss <- Snack_loss+total_ticket_loss 
    
    # We add all the information in one dataframe for each day 
    total_loss_theater_N <- rbind(total_loss_theater_N, data.frame(Day,total_ticket_loss,Snack_loss, Total_loss))
    
    
  }
  
  
  return(total_loss_theater_N)

}


total_loss_theater_1<- calc_lost(theater_1_data,ticket_price_adult_1,ticket_price_child_1,total_loss_theater_1)

total_loss_theater_2<- calc_lost(theater_2_data,ticket_price_adult_2,ticket_price_child_2,total_loss_theater_2)


#-------------------------------------------------------------------------------------------------------------------------------#

# Theater Membership plan 

# monthly cost of subscription 35$

# Membership perks: 

# 1) 3 movies per week

# 2) 2 free snacks per visit

# 3) point and reward system: 
#   - How to gain points? 
#     1) Everytime you renew you get 10 points 
#     2) Everytime you renew on a consecutive streak you get 3 extra points per month and it adds up till 6 months 
#     (first month: 10, second month: 13, third month: 16, fourth month: 19, fifth month: 22, sixth month: 25, for a total = 105 for six months), after six months its a 25 point increase per month streak after
#     As an incentive to bring people back every month we added this system so the customer can gather more points. 
#     The more consecutive uninterrupted months they resubscribe the more points they gather
#     
#   - Rewards: 
#     1) one Extra movie = 25 points  
#     2) one Extra snack = 5 points  
#     3) 10% discount = 10 points  

# 4 - vouchers you can use or give to someone else worth (5 dollars)

# 5 - one free voucher and snacks on your birthday

#--------------------------------------------------------------------------------------------------------------------------------------------#

#this method is to calculate reward points for 6 consecutive months or less
full_streak <- function(streak,consecutive_month_points,renewal_points){
  
  if(streak==1|streak-5==1){
    consecutive_month_points <- consecutive_month_points+renewal_points
  }
  if(streak ==2|streak-4==2){
    consecutive_month_points <- consecutive_month_points+renewal_points+3
  }
  if(streak ==3|streak-3==3){
    consecutive_month_points <- consecutive_month_points+renewal_points+6
  }
  if(streak ==4|streak-2==4){
    consecutive_month_points <- consecutive_month_points+renewal_points+9
  }
  if(streak ==5|streak-1==5){
    consecutive_month_points <- consecutive_month_points+renewal_points+12
  }
  if(streak ==6){
    consecutive_month_points <- consecutive_month_points+renewal_points+15
  }
  return(consecutive_month_points)
}

#this method is to calculate the points and vouchers for each customer in the imported data
points_and_vouchers<- function(){
  
  
  # we get the number of customers so we can loop through them
  num_of_customers<-nrow(membership_Data)
  
  #iterate through all customers to update their point and voucher information
  for(i in 1:num_of_customers){
    
    #get the customers id
    customer_id <- i
    
    #get the customers streak 
    streak <- membership_Data[membership_Data$customer_ID==i,"streak"]
    
    #get the customers total months
    total_months_subbed <- membership_Data[membership_Data$customer_ID==i,"total_months_subbed"]
    
    #then we seperate the streak months from the total months to get the non-consecutive months
    non_consecutive_months <- total_months_subbed - streak
    
    renewal_points <- 10
    
    
    #calculate the non consecutive months points 
    non_consecutive_months_points <- non_consecutive_months *renewal_points
    
    consecutive_month_points <- 0
    #calculate the consecutive months points
    
    
    #check if the streak is 6 months or less
    if(streak<=6){
      
      #call the method to calculate the 6 or less month streak
      consecutive_month_points<-full_streak(streak,consecutive_month_points,renewal_points)
    }
    # if the streak is higher than 6 months 
    else if(streak>6){
      
      #first we calculate the 6 month streak so the remaining months of the streak will be worth 25 points because that is the cap
      
      #this temp is the number of capped months after 6 months (example: 10 month streak means your capped at 6 and have 4 extra months worth 25 points)
      temp <- streak -6
      
      #call the 6 month streak method to calculate the points
      consecutive_month_points<-full_streak(6,consecutive_month_points,renewal_points)
      
      #the remaining points are worth 25 for each consecutive month 
      consecutive_month_points <- (temp*25) +consecutive_month_points
      
    }
    
    #add up the total points by adding both the consecutive and non consecutive points
    total_points <- non_consecutive_months_points + consecutive_month_points
    
    
    # you get 1 voucher per month plus 1 extra voucher on your birthday 
    vouchers <- total_months_subbed
    
    #this is to calclulate 1 extra voucher every 12 months
    if(total_months_subbed > 12){
      vouchers <- vouchers + floor(total_months_subbed/12)
    }
    
    #this is the number of movies the customer has left to use up 
    movies_left <- 3
    
    #store all the information in the dataframe and return it
    membership_DF<-rbind(membership_DF, data.frame(customer_id,streak,total_months_subbed, total_points,vouchers,movies_left))
    
  }
  
  return(membership_DF)
  
}


# ----- Points and Vouchers call ----- #
membership_DF<- points_and_vouchers()




# This method is to redeem rewards based on the amount of points the customer has
redeem_reward<- function(reward,customer_id){
  
  #check if the customer has enough points 
  customer_points <- membership_DF[membership_DF$customer_id==customer_id,"total_points"]
  #get the rewards point cost 
  reward_cost <- membership_rewards[membership_rewards$reward==reward,"points_cost"]
  
  #make sure the customer has enough points by comparing the cost and the points in their wallet
  if(customer_points > reward_cost){
    cat("You have redeemed ", reward, "for a total of ", reward_cost, " points!")
    
    #subtract the cost and update the remaining balance
    remaining_balance <- customer_points - reward_cost
    membership_DF[membership_DF$customer_id==customer_id,4] <- remaining_balance
  }
  else{
    cat("sorry you dont have enough points!")
  }
  
  # we access the updated points after reduction to display how many points the customer has left
  customer_points <- membership_DF[membership_DF$customer_id==customer_id,"total_points"]
  cat(" Your remaining balance is ", customer_points )
  
  return(membership_DF)
  
}

# ----- Redeem Reward call ----- #
membership_DF<-redeem_reward("one_extra_movie", 1)

#-------------------------------------------------------------------------------------------------------------------------------#



## ----------------- Create conditional statements for movies that may be PG-13 and children are not allowed to watch -----------------##

#function takes name of the movie and returns who can attend and who cannot

movie_rate <- function(m_name,theater_Data){
  
  index <- which(unique(theater_Data$Movie == m_name)) # get index number for the selected movie
  
  index <- as.integer(index) # convert index into integer value
  
  if (theater_Data[index, 3]== "R"){ # check the rating of the movie 
    
    cat ("the movie: (",theater_Data[index, 2], ") is only avalibale for adults.", sep = "")
    
  }else {
    cat ("the movie: (",theater_Data[index, 2],") is avalibale for all adults and children.",sep = "")
  }
}

movie_rate("Totoro",theater_1_data)
movie_rate("The Hobbit",theater_1_data)

movie_rate("Another round",theater_2_data)
movie_rate("Maya the bee3",theater_2_data)


#-------------------------------------------------------------------------------------------------#


# listing all movies and who can attend them.
movie_listing <- function(theater_Data){
  
  for (i in 1:nrow(unique(theater_Data[c("Movie")]))) {
    
    movie_name <- theater_Data[i, 2]
    movie_rate <- theater_Data[i, 3]
    
    if(movie_rate == "R"){
      cat ("the movie: (",movie_name, ") is only avalibale for adults.","\n", sep = "")
    }else {
      cat ("the movie: (",movie_name, ") is avalibale for all age ranges.","\n", sep = "")
    }
  }
}
movie_listing(theater_1_data)
movie_listing(theater_2_data)

#-------------------------------------------------------------------------------------------------#

#recommending movies based on selected genre
movie_rec <- function(genre){
  
  index <- as.vector(which(movie_list_Data$Genres == genre)) # get index number for the input movie
  
  length(index)  # find the number of movies with the selected genre
  
  count =1 
  while (count <= length(index)){
    if (count > 1 ){
      cat("\n",count, movie_list_Data[index[count],1 ], "\nmovie descrption:" 
          , movie_list_Data[index[count],4 ])
      cat("\n")
      
    }else {
      cat("\n we are recommending the follwing",genre,"movie's :\n",count, movie_list_Data[index[count],1 ], "\nmovie descrption:" 
          , movie_list_Data[index[count],4 ])
      cat("\n")}
    count = count +1 
  }
  
}

movie_rec("Adventure")
movie_rec("Animation")
movie_rec("Action")
movie_rec("Comedy")
movie_rec("Fantasy")
movie_rec("Horror")



## -------------------  Task (3.4) - Make a barchart showing total revenue per day  -------------------##

head(total_revenue_theater_1) # get the head of total revenue datafram for theater 1 
head(total_revenue_theater_2) # get the head of total revenue datafram for theater 2

day_num1 <- unlist(total_revenue_theater_1[1]) 
day_num2 <- unlist(total_revenue_theater_2[1])

#barplot for theater 1
chart_1_theater_1 <- barplot(t(as.matrix(total_revenue_theater_1$Total_Rev, total_revenue_theater_1$Day)),
                             main= "Theater 1 Total revenue per day",
                             xlab = "Day",
                             ylab = "Total revenue in $USD",
                             col = c("#dcb9c5", "#d4a7b7", "#c2849a", "#c2849a", "#ba728c", "#b1617e", "#a95070"),
                             names.arg = day_num1,
                             beside=TRUE,
                             horiz = FALSE)
#barplot for theater 2
chart_1_theater_2 <- barplot(t(as.matrix(total_revenue_theater_2$Total_Rev, total_revenue_theater_2$Day)),
                             main= "Theater 2 Total revenue per day",
                             xlab = "Day",
                             ylab = "Total revenue in $USD",
                             col = c("#dcb9c5", "#d4a7b7", "#c2849a", "#c2849a", "#ba728c", "#b1617e", "#a95070"),
                             names.arg = day_num2,
                             beside=TRUE,
                             horiz = FALSE)

#-------------------------------------------------------------------------------------------------#

## -------------------  Task (3.5) -  Make any other chart  -------------------##

#we create 2 function to find sum of revenue and sum of lost to do pie plot

#function to find sum of revenue
sum_revenue <- function(total_rev_theater_N){
  
  # iterate through the week
  for (i in 1:Days_of_week){
    
    sum_revenue <- sum(total_rev_theater_N$Total_Rev, na.rm = TRUE)
    
    return(sum_revenue)
    
  }
}


#function call 
sum_rev_for_week_th_1 <- sum_revenue(total_revenue_theater_1)
sum_rev_for_week_th_2 <- sum_revenue(total_revenue_theater_2)

#function to find sum of lost
sum_lost <- function(total_loss_th_N){
  
  # iterate through the week
  for (i in 1:Days_of_week){
    
    sum_loss <- sum(total_loss_th_N$Total_loss, na.rm = TRUE)
    
    return(sum_loss)
    
  }
}
#function call 
sum_loss_for_week_th_1 <- sum_lost(total_loss_theater_1) # sum loss for all week in theater 1
sum_loss_for_week_th_2 <- sum_lost(total_loss_theater_2) # sum loss for all week in theater 2

#pie plot for theater 1

value1 <- c(sum_rev_for_week_th_1,sum_loss_for_week_th_1)
pie_percent1 <- round(100*value1/sum(value1), 1)

chart_2_theater_1 <- pie(value1, 
                         labels = pie_percent1,
                         main = "Theater 1 Revenue VS. Lost",
                         col = c("#d4a7b7","#a95070"))
legend("topright", c("Revenue", "Lost"), cex = 0.8,
       fill = c("#d4a7b7","#a95070"))  

#pie plot for theater 2
value2 <- c(sum_rev_for_week_th_2,sum_loss_for_week_th_2)
pie_percent2 <- round(100*value2/sum(value2), 1) 

chart_2_theater_2 <- pie(value2, 
                         labels = pie_percent2,
                         main = "Theater 2 Revenue VS. Lost",
                         col = c("#d4a7b7","#a95070"))
legend("topright", c("Revenue", "Lost"), cex = 0.8,
       fill = c("#d4a7b7","#a95070"))


#-------------------------------------------------------------------------------------------------------------------------------#



# Clear plots
#dev.off()  # But only if there IS a plot

# Clear console
#cat("\014")  # ctrl+L

#-------------------------------------------------------------------------------------------------#
