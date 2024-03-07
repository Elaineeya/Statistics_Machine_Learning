name <- c("Xuan Wang")
liuid <- c("xuawa284")
#install.packages("devtools")
#devtools::install_github("MansMeg/markmyassignment")
#library(markmyassignment)
#lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab2.yml"
#set_assignment(lab_path)
sheldon_game<-function(player1,player2){
  choices <- c("scissors", "paper", "rock", "lizard", "spock")
  if ( !player1 %in% choices | !player2 %in% choices) {
    stop("Your arguments have errors, please chose one of choices: rock, paper, scissors, lizard, spock.")
  }
  player1_position <- match(player1, choices)
  player2_position <- match(player2, choices)
  if ( player1_position == player2_position ) {
    return("Draw!")
  } 
  else if ( player1_position == 1 ) {
    if ( player2_position == 2 | player2_position == 4 ) {
      return ("Player 1 wins!")
    } else {
      return ("Player 2 wins!")
    }
  }
  else if ( player1_position == 2 ) {
    if ( player2_position == 3 | player2_position == 5 ) {
      return ("Player 1 wins!")
    } else {
      return ("Player 2 wins!")
    }
  } 
  else if ( player1_position == 3 ) {
    if ( player2_position == 4 | player2_position == 1 ) {
      return ("Player 1 wins!")
    } else {
      return ("Player 2 wins!")
    }
  } 
  else if ( player1_position == 4 ) {
    if ( player2_position == 5 | player2_position == 2 ) {
      return ("Player 1 wins!")
    } else {
      return ("Player 2 wins!")
    }
  } 
  else if ( player1_position == 5 ) {
    if ( player2_position == 1 | player2_position == 3 ) {
      return ("Player 1 wins!")
    } else {
      return ("Player 2 wins!")
    }
  }
}

my_moving_median<-function(x, n, ...){
  if ( !is.vector(x) | !is.numeric(x) | !is.numeric(n) ) {
    stop("Your arugments should be a numeric vector and a numeric scalar, please check!!")
  }
  my_median_vec <- c()
  for ( i in 1:(length(x) - n) ) {
    position <- i
    my_moves <- x[i]
    moves_median <- c()
    for (j in 1:n ) {
      if ( (i + j) > length(x) ) {
        position <- length(x) - (i + j)
      } else {
        position <- i + j
      }
      my_moves <- c(my_moves, x[position])
    }
    var_args <- list(...)
    na.rm <- if(!is.null(var_args[["na.rm"]])) var_args[["na.rm"]] else FALSE
    moves_median <- median(my_moves, na.rm)
    my_median_vec <- c(my_median_vec, moves_median)
  }
  return(my_median_vec)
}

for_mult_table<- function(from, to) {
  if ( !is.numeric(from) | !is.numeric(to) ) {
    stop("Your arugments should be numeric scalar, please check!!")
  }
  length <- to - from + 1
  mult_table <- matrix(0, nrow = length, ncol = length)
  a <- from
  for ( i in 1:length) {
    b <- from
    for ( j in 1:length ) {
      mult_table[i,j] <- a*b
      b <- b + 1
    }
    a <- a + 1
  }
  colnames(mult_table) <- paste0(from:to)
  rownames(mult_table) <- paste0(from:to)
  return(mult_table)
}

find_cumsum<-function(x,find_sum) {
  if ( !is.vector(x) | !is.numeric(x) | !is.numeric(find_sum) ) {
    stop("Your arugments should be a numeric vector and a numeric scalar, please check!!")
  }
  cumsum_value <- 0
  i <- 1
  while ( cumsum_value < find_sum && i <= length(x) ) {
    cumsum_value <- cumsum_value + x[i]
    i <- i + 1
  }
  return(cumsum_value)
}

while_mult_table<-function(from,to) {
  if ( !is.numeric(from) | !is.numeric(to) ) {
    stop("Your arugments should be numeric scalar, please check!!")
  }
  length <- to - from + 1
  mult_table <- matrix(0, nrow = length, ncol = length)
  a <- from
  b <- from
  i <- 1
  while ( i <= length) {
    j <- 1
    while ( j <= length ) {
      mult_table[i,j] <- (a + i - 1)*(b + j - 1)
      j <- j + 1
    }
    i <- i + 1
  }
  colnames(mult_table) <- paste0(from:to)
  rownames(mult_table) <- paste0(from:to)
  return(mult_table)  
}

repeat_find_cumsum<-function(x,find_sum) {
  if ( !is.vector(x) | !is.numeric(x) | !is.numeric(find_sum) ) {
    stop("Your arugments should be a numeric vector and a numeric scalar, please check!!")
  }
  cumsum_value <- 0
  i <- 1
  repeat {
    cumsum_value <- cumsum_value + x[i]
    i <- i + 1
    if ( cumsum_value > find_sum | i > length(x) ) {
      break
    }
  }
  return(cumsum_value)  
}

repeat_my_moving_median<-function(x, n, ...) {
  if ( !is.vector(x) | !is.numeric(x) | !is.numeric(n) ) {
    stop("Your arugments should be a numeric vector and a numeric scalar, please check!!")
  }
  my_median_vec <- c()
  i <- 1
  moves_median <- c()
  repeat {
    position <- i
    my_moves <- x[i]
    for (j in 1:n ) {
      if ( (i + j) > length(x) ) {
        position <- length(x) - (i + j)
      } else {
        position <- i + j
      }
      my_moves <- c(my_moves, x[position])
    }
    var_args <- list(...)
    na.rm <- if(!is.null(var_args[["na.rm"]])) var_args[["na.rm"]] else FALSE
    moves_median <- median(my_moves,na.rm)
    my_median_vec <- c(my_median_vec, moves_median)
    i <- i + 1
    if ( i > (length(x) - n) ) {
      break
    }
  }
  return(my_median_vec)
}

in_environment<-function(env) {
   return(ls(env))
}

cov<-function(X) {
  if (is.data.frame(X)) {
    return(unlist(lapply(as.list(X), function(x) sd(x) / mean(x))))
  } else {
    stop("Your argument shoud be data.frame, please check!!")
  }
  
}

###the equation is referenced from https://search.r-project.org/CRAN/refmans/fastmatrix/html/moments.html  #####
moment<-function(i) {
  if (is.numeric(i)) {
    function(x) {
      sum((x-mean(x))^i) /length(x)
    }
  } else {
    stop("Your argument shoud be numerical variable, please check!!")
  }
}
