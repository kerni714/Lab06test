greedy_knapsack <- function(x,W) {
  
  n <- nrow(x)
  v <- x$v
  w <- x$w
  
  #- Calculate value per unit weight
  v_div_w <- v/w
  
  #- Order in descending order
  sort_v_div_w <- order(-v_div_w)
  
  sum_w <- 0
  sum_v <- 0
  i<- 1
  ind <- c()
  while (sum_w <= W) {
    sum_w <- sum_w + w[sort_v_div_w[i]]
    sum_v <- sum_v + v[sort_v_div_w[i]]
    ind[i] <- sort_v_div_w[i]
    i <- i +1
  }
  i<- i-1
  sum_w <- sum_w-w[sort_v_div_w[i]]
  sum_v <- sum_v-v[sort_v_div_w[i]]
  
  value <- sum_v
  #elements <- sort(ind[1:length(ind)-1])
  elements <- ind[1:length(ind)-1]
  
  structure(
    list(
      value = value,
      elements = elements
    )
  )
  #print(s)
  #print(val)
  #print(ind[1:length(ind)-1])
}
