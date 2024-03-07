name <- c("Xuan Wang")
liuid <- c("xuawa284")
#install.packages("devtools")
#devtools::install_github("MansMeg/markmyassignment")
#library(markmyassignment)
#lab_path <- "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
#set_assignment(lab_path)
my_num_vector <- function() {c(log10(11), cos(pi/5), exp(pi/3), (1173 %% 7)/19)}

filter_my_vector<-function(x, leq){
  ### validate first argument ###
  if (is.vector(x)){
    for (i in 1:length(x)){
      if(!is.numeric(x[i])) stop("there is non-numeric value in your first input arguments, please check!!")
    }
  }
  else {
    stop("Your first input argument should be vector, please check!!")
  }
  ### validate second argument ###
  if(!is.numeric(leq)) stop("Your second argument should be numeric, please check!!")
  ### main function ###
  x[x>=leq]<-NA
  return(x)
}

dot_prod<-function(a, b){
  ### validate first argument ###
  if (is.vector(a)){
    for (i in 1:length(a)){
      if(!is.numeric(a[i])) stop("there is non-numeric value in your first input arguments, please check!!")
    }
  }
  else {
    stop("Your first input argument should be vector, please check!!")
  }
  ### validate second argument ###
  if (is.vector(b)){
    for (i in 1:length(b)){
      if(!is.numeric(b[i])) stop("there is non-numeric value in your second input arguments, please check!!")
    }
  }
  else {
    stop("Your second input argument should be vector, please check!!")
  }
  ### main function ###
  c(a %*% b)
}

approx_e<-function(N){
  sum <- 0
  for (i in 0:N) {
    sum <- sum + (1/factorial(i))
  }
  return(sum)
}

my_magic_matrix<-function(){
  return(matrix(c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3))
}

calculate_elements<-function(A){
  if (is.matrix(A)) {
    return(length(A))
  } else {
    stop("Your input argument is not matrix, please check!!")
  }
}

row_to_zero<-function(A, i){
  if (is.matrix(A)) {
    A[i,]<-0
    return(A)
  } else {
    stop("Your input arguments have errors, please check!!")
  }
}

add_elements_to_matrix<-function(A, x, i, j){
  if (is.matrix(A)) {
    A[i,j]<-A[i,j] + x
    return(A)
  } else {
    stop("Your input arguments have errors, please check!!")
  }
}

my_magic_list<-function(){
  return(list(info = "my own list", my_num_vector(), my_magic_matrix()))
}

change_info<-function(x, text){
  if (is.list(x) && is.character(text)) {
    x$info <- text
    return(x)
  } else {
    stop("Your input arguments have errors, please check!!")
  }
}

add_note<-function(x, note){
  if (is.list(x) && is.character(note)) {
    x$note <- note
    return(x)
  } else {
    stop("Your input arguments have errors, please check!!")
  }
}

sum_numeric_parts<-function(x){
  if (is.list(x)) {
    s <- 0
    for ( i in 1:length(x) ) {
      if (is.numeric(x[[i]])) {
        s <- s + sum(x[[i]])
      } else {
        message("Warning in sum_numeric_parts(x = a_list): NAs introduced by coercion")
      }
    }
    return(s)
  } else {
    stop("Your input arguments have errors, please check!!")
  }
}

my_data.frame<-function(){
  df <- data.frame(
    id = c(1L, 2L, 3L),
    name = c("John", "Lisa", "Azra"),
    income = c(7.30, 0.00, 15.21),
    rich = c(FALSE,FALSE,TRUE)
  )
  return(df)
}

sort_head<-function(df, var.name, n){
  df_sort <- df[order(df[[var.name]], decreasing = TRUE), ]
  head(df_sort, n)
}

add_median_variable<-function(df, j){
  df$compared_to_median <- with(df, ifelse(
    df[,j] > median(df[,j]), "Greater", ifelse(
    df[,j] < median(df[,j]), "Smaller", "Median"
    )))
  return(df)
}

analyze_columns<-function(df, j){
  col1 <- j[1]
  col2 <- j[2]
  cor_list <- list(setNames(c(mean(df[,col1]), median(df[,col1]), sd(df[,col1])), c("mean", "median", "sd")),
                   setNames(c(mean(df[,col2]), median(df[,col2]), sd(df[,col2])), c("mean", "median", "sd")),
                   cor(df[, c(colnames(df[col1]),colnames(df[col2]))])
              )
  names(cor_list) <- c(colnames(df[col1]), colnames(df[col2]), "correlation_matrix")
  return(cor_list)
}
