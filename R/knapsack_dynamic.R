knapsack_dynamic <- function(x,W) {
  
  #ERROR CHECK OF INPUT
  v <- x$v
  w <- x$w
  
  n <- nrow(x)
  m <- matrix(0,(n+1),(W+1))
  
  i_v<- 0:n
  j_v <- 0:W
  
  for (i in 2:(n+1)){
    #for (i in 2:2){
    #print("i")
    #print(i)
    for (j in 1:(W+1)) {
      #for (j in 1:24) {
      #print("j")
      #print(j)
      if (w[i_v[i]] > j_v[j]) {
        m[i,j] <- m[i-1,j]
      }
      else {
        m[i,j] <- 
          max(m[i-1,j], 
              m[i-1,j-w[i_v[i]]]+v[i_v[i]])
      }
    }
  }
  
  #return(m)
  ind <- knapsack(m, n+1, W+1, w, W) - 1
  elements <- sort(ind)
  #elements <- 1
  value <- sum(v[ind])
  structure(
    list(
      value = value,
      elements = elements
    )
  )
  
}

knapsack <- function (m, i, j, w, W) {
  
  n <- length(w)
  ind <- c()
  i_v<- 0:n
  j_v <- 0:W
  
  if (i == 1) {
    #print("ind")
    #print(ind)
    return(ind)
  }
  # print("i")
  # print(i)
  # print("j")
  # print(j)
  # print("m[i, j]")
  # print(m[i, j])
  # print("m[i-1, j]")
  # print(m[i-1, j])
  if (m[i, j] > m[i-1, j]){
    
    #print("i")
    #print(i)
    #print("j")
    #print(j)
    ind <- union(i,knapsack(m,i-1, j-w[i_v[i]],w,W))
    #print("ind in if")
    #print(ind)
    return(ind)
  }
  else{
    #print("in else")
    ind <- knapsack(m,i-1, j,w,W)
    #print("ind in else")
    #print(ind)
    return(ind)
  }
}

