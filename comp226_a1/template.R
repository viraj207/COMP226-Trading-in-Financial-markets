# Viraj Patel
# Student ID : 201351707
# COMP 226 Assigment 1

book.total_volumes <- function(book) {
  total_volumes <- c(list(ask = 0, bid = 0))  # Creating a list called total_volumes with an Ask value and a Bid Value n
  total_volumes$ask <- sum(book$ask["size"])  # Summing the Ask Size values and assigning it to the created list
  total_volumes$bid <- sum(book$bid["size"])  # summing the Bid size values and assigning it to the created list
  
  return(total_volumes)                       # Returning the created list  
# print("test1")
}


book.best_prices <- function(book) {

  best_prices <- c(list(ask = 0, bid = 0))   # Creating a list called best_prices with an Ask value and a Bid Value n
  best_prices$ask <- min(book$ask["price"])  # getting the smallest price from the ask dataframe and setting it to the ask val in my list
  best_prices$bid <- max(book$bid["price"])  # getting the greatest price from the bid dataframe and setting it to the ask val in my list
  
  return(best_prices)                        #returning the created list

}

book.midprice <- function(book) {

  best_prices <- c(list(ask = 0, bid = 0))   # Creating a list called best_prices with an Ask value and a Bid Value
  best_prices$ask <- min(book$ask["price"])  # repeating steps from prev function
  best_prices$bid <- max(book$bid["price"])

  midprice <- (best_prices$bid + best_prices$ask) /2 
                                             # calculating the mid price
  
}

book.spread <- function(book) {

  best_prices <- c(list(ask =0, bid = 0))    # Creating a list called best_prices with an Ask value and a Bid Value
  best_prices$ask <- min(book$ask["price"])  # repeating steps from prev function
  best_prices$bid <- max(book$bid["price"]) 
  
  spread <- (best_prices$ask - best_prices$bid)
                                             # calculating the spread
  
}


book.add <- function(book, message) {
  book<-book.sort(book)
                                            # creating an array to hold the ID of the message to be removed
  bid_oid_del  <-'0'
  ask_oid_del  <-'0'
  book<-book.sort(book)
                                            #checking weather the message is a Sell
  if(message$side =='S'){ 
    if (message$price <= max(book$bid$price)){ 
      for (x in 1:nrow(book$bid)){          # stepping through the bid order book
        if (message$price <= book$bid$price[x]){ 
          if (book$bid$size[x] >= message$size){
            book$bid$size[x] <- book$bid$size[x]-message$size
            message$size <- 0 #set message size to 0 if bid size is larger
          }else if (book$bid$size[x] < message$size){
            bid_oid_del <- c(bid_oid_del ,book$bid$oid[x]) #update the bid_oid_size Array
            message$size <- message$size-book$bid$size[x] 
            # set the book bid size at element x to 0
            book$bid$size[x]<- 0
          }
        }
      }
    }
    if (message$size!=0){ 
      n_row <-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
      book$ask<- rbind(book$ask, n_row , stringsAsFactors = FALSE)
    }
    #Checking weather the message is a Buy
  }else if(message$side=='B'){ 
    if (message$price>=min(book$ask$price)){ 
      for (x in 1:nrow(book$ask)){ # Stepping through each element of the ask book
        if (message$price>=book$ask$price[x]){
          if (book$ask$size[x]>=message$size){
            book$ask$size[x]<-book$ask$size[x]-message$size 
            message$size<-0
          }else if (book$ask$size[x] < message$size){ 
            ask_oid_del  <- c(ask_oid_del ,book$ask$oid[x]) #update the ask_oid_size
            message$size <- message$size-book$ask$size[x] 
            #set the book ask size at element x to 0
            book$ask$size[x] <- 0
          }
        }
        
      }
    }
    #checking the message size isnt 0
    if (message$size !=0 ){ 
      n_row <-data.frame(oid=message$oid,price=message$price,size=message$size, stringsAsFactors = FALSE)
      book$bid<- rbind(book$bid, n_row , stringsAsFactors = FALSE)
    }
  }
  if (length(ask_oid_del )!= 1){
    for (i in ask_oid_del )
      if (i!= "0")
        # update the ask order book with the removed order
        book$ask<-book$ask[book$ask$oid!=i,] 
  }
  if (length(bid_oid_del )!= 1){ 
    for (i in bid_oid_del )
      if (i!= "0")
        # update the bid order book with the removed order
        book$bid<-book$bid[book$bid$oid!= i,] 
  }
#  book<-book.sort(book)
  return(book)
}



book.reduce <- function(book, message) {
  message_df <- data.frame(message)    
  reduce_ask_oid<- ""
  
#Deal with reduction of ask book
  if (nrow(book$ask)!= 0){
   for (i in 1:nrow(book$ask)){
     if (message$oid==book$ask$oid[i]){
        if (message$amount>=book$ask$size[i]){
         reduce_ask_oid<-message$oid
         #print(reduce_ask_oid)
       }
       else if (message$amount<book$ask$size[i])
         #update the size with the new reduced size
          book$ask$size[i]<-book$ask$size[i]-message$amount
     }
    }
  }
  
  
  if (!is.null(reduce_ask_oid)){
                                    #remove this order from the ask book
    book$ask<-book$ask[book$ask$oid!=reduce_ask_oid, ]
  }
  
#deal with reduction of bid book
  reduce_bid_oid <- ""
  for (i in 1:nrow(book$bid)) {
    if (message$oid==book$bid$oid[i]) {
      if (message$amount>=book$bid$size[i]) {
        reduce_bid_oid <- message$oid 
        #print(reduce_bid_oid)
      }
      else if (message$amount<book$bid$size[i]){
        book$bid$size[i]<-book$bid$size[i]-message$amount
      }
    }
  }
  if (!is.null(reduce_bid_oid)){
                                    #remove this order from the bid book
    book$bid<-book$bid[book$bid$oid!=reduce_bid_oid,]
  }
                                    #call the book.sort function on the book, to sort the book
  book.sort(book)
  return(book)                      # return the book

  
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
    # See handout for instructions
}

book.extra2 <- function(book, size) {
    # See handout for instructions
}

book.extra3 <- function(book)       {
    # See handout for instructions
}

book.extra4 <- function(book, k)   {
    # See handout for instructions
}

