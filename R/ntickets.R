#' ntickets
#' @param N number of seats on plane
#' @param gamma probability of overbooking
#' @param p probability of show
#'
#' @return two plots of discrete and continuous distributions and a list of all values
#' @export
#'
#' @examples \dontrun{ntickets(N= 200, gamma = 0.02, p = 0.95}
ntickets <- function(N = 200, gamma = 0.02, p = 0.95){
  #Discrete Distribution
  myDistfunct <- function(n) { # Discrete function
    1 - gamma - pbinom(N, round(n), p)
  }
  V <- seq(N, N + N * 0.1, by = 1)
  l <- myDistfunct(V)
  index <- which.min(abs(l))
  nd <- V[index]

  #Continuous Distribution
  myContFunct <- function(n){
    l <- 1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1- p)))
    abs(l)
  }
  nc <- optimize(myContFunct, c(N, N+N*0.1))


  #Plot for Discrete Distribution
  plot(V, myDistfunct(V), xlim = c(N, N+N*0.1), xlab = "n", ylab = "Objective", type = "b", pch = 21, bg = "black")
  abline(v = nd, h = 0, col = "red")
  axis(side = 1, labels = TRUE, at = nd, col.ticks = "red", lwd = 2)
  title(main = paste0("Objective vs n to find optimal tickets sold (", nd,") gamma = ", gamma, " N=", N, " discrete"))

  #Plot for Continuous Distribution
  curve(1 - gamma - pnorm(N + 0.5, x * p, sqrt(x * p * (1- p))), xlim = c(N,N+N*0.1),lwd = 2, ylab = "Objective", xlab= "n")
  abline(v = nc , h = 0, col = "blue")
  axis(side = 1, labels = TRUE, at = nc, col.ticks = "blue", lwd = 2)
  title(main = paste0("Objective vs n to find optimal tickets sold (", nc$minimum,") gamma = ", gamma, " N=", N, " continuous"))

  #list of all the required values
  results <- list(nd = nd, nc = nc$minimum, N = N, p = p, gamma = gamma)
  print(results)
}
