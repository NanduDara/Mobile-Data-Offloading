#Declaring 10 data items initially
n <- 8
D <- data.frame(size <- c(sample(10:30,n)), prob1 <- round(runif(n,min=0,max=1),digits=1),
                size1 <- c(sample(30:40,n)), prob2 <- round(runif(n,min=0,max=1),digits=1))
colnames(D)[1] <- "size"
colnames(D)[2] <- "prob1"
colnames(D)[3] <- "size1"
colnames(D)[4] <- "prob2"

D$size <- sort(D$size)
D$prob1 <- sort(D$prob1)
D$size1 <- sort(D$size1)
D$prob2 <- sort(D$prob2, decreasing=TRUE)

#Function for Successful Offloading Probability
rho <- function(prob)
{
  l <- length(prob)
  result <- 1
  for(i in 1:l)
  {
    result <- result * (1-prob[i])
  }
  result <- 1 - result
  return(result)
}

#Offloading Utility Function
utility <- function(size, prob)
{
  result <- 0
  result <- result + (size * rho(prob))
  return(result)
}

util1 <- rep(0, times=n)
util2 <- rep(0, times=n)
k <- 0
#Calculating Utility Values
for(i in 1:n)
{
  k <- k+1
  util1[k] <- utility(D$size[i], D$prob1[i])
  util2[k] <- utility(D$size1[i], D$prob2[i])
}
print(util1)
print(util2)
data <- data.frame(D$size, D$size1, D$prob1, D$prob2, util1, util2)
#write.csv(data, "Util_vs_Size_vs_Prob.csv")
