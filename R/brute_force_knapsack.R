#' brute_force_knapsack
#'
#' @param x data frame with two variables v and w
#' @param W knapsack size
#'
#' @return list with value and elements
#' @export
#'
#' @examples
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  )
#' x <- knapsack_objects[1:8,]
#' brute_force_knapsack(x = x, W = 3500)

brute_force_knapsack <- function(x,W){
  #- Checks of input
  stopifnot(is.data.frame(x))

  v <- x$v
  w <- x$w
  n <- nrow(x)

  n_comb <- 2^n
  #- Binary rep
  bin <- matrix(0,nrow=n_comb,ncol=n)
  sum_mat <- matrix(0,nrow=n_comb,ncol=2)
  #comb <- list()
  #comb_i <- as.vector(matrix(0,1,32))
  comb <- matrix(0,n_comb,n)
  #mat <- matrix(0,nrow=n,ncol=1)
  #mat[,1] <- 1:n
  #bin <- as.raw(matrix(0,nrow=n,ncol=32))
  for (i in 1:n_comb)  {
  #for (i in 1:10) {
    bin[i,] <- as.numeric(intToBits(i))[1:n]
    #bin[i,] <- intToBits(i)
    #print(which(bin[i,]==1))
    #comb[[i]] <- which(bin[i,]==1)
    #comb_i <- which(bin[i,]==1)
    #sum_w_i <-
    ind <- which(bin[i,]==1)
    comb[i,ind] <- 1
    sum_mat[i,1] <- sum(comb[i,]*v)
    sum_mat[i,2] <- sum(comb[i,]*w)
  }


  #- Find max v where w <= W
  ind1 <- which(sum_mat[,2] < W)
  ind <- which.max(sum_mat[ind1,1])
  ind_final <- ind1[ind]
  elements <- which(comb[ind_final,]==1)
  value <- sum_mat[ind_final,1]

  structure(
    list(
      value = value,
      elements = elements
    )
  )

}
