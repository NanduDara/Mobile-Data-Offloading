#Declaring 10 data items initially
n <- 4
D <- data.frame(size <- c(sample(1:15,n)), ttl <- c(sample(15:25,n)))
colnames(D)[1] <- "size"
colnames(D)[2] <- "ttl"

#Sorting data times on the bases of their time to live(ttl)
order <- rep(0, times=n)
order <- sort(D$ttl)

#Without loss of generality, we assume that these data items 
#are organized in the ascending order of their TTLs
Sorted_D <- data.frame(size <- rep(0, times=n), ttl <- rep(0, times=n))
colnames(Sorted_D)[1] <- "size"
colnames(Sorted_D)[2] <- "ttl"
k <- 1
for(i in 1:n)
{
  for(j in 1:n)
  {
    if(order[i] == D$ttl[j])
    {
      print(D$size[j])
      Sorted_D$size[k] <- D$size[j];
      Sorted_D$ttl[k] <- D$ttl[j];
      k <- k+1;
    }
  }
}

#Number of Offloading oppurtunuties
m <- 2
W <- data.frame(tau <- c(sample(12:22,m)), 
                p <- c(round(runif(m, 0, 1),digits = 1)),
                q <- c(sample(5:20,m)))
colnames(W)[1] <- "tau"
colnames(W)[2] <- "prob"
colnames(W)[3] <- "size"

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

#Cost Calculation due to offloading
c <- 1;
C <- 4;
cost <- function(c, C, size, prob)
{
  result1 <- 0
  result1 <- result1 + (size * C)
  
  result2 <- 0
  result2 <- result2 + ((C-c)*(size*rho(prob)))
  
  result <- 0
  result <- result2 - result1
  return(result)
}

#Offloading Utility Function
utility <- function(size, prob)
{
  result <- 0
  result <- result + (size * rho(prob))
  return(result)
}

#Data Taken from the reference paper
Sorted_D <- data.frame(size <- c(8,6,5,10), ttl <- c(11,13,17,18))
colnames(Sorted_D)[1] <- "size"
colnames(Sorted_D)[2] <- "ttl"
W <- data.frame(tau <- c(10,15), 
                p <- c(0.6,0.9),
                q <- c(15,10))
colnames(W)[1] <- "tau"
colnames(W)[2] <- "p"
colnames(W)[3] <- "q"

#Offline Data Offloading
k <- 1
cost_offload <- matrix(data = 0, nrow = n, ncol = m)
pi <- matrix(data = -1, nrow = n, ncol = m)
record <- matrix(data = 0, nrow = (n*m), ncol = m)
record_for_pi <- rep(0, times = (n*m))
util <- seq(1:(n*m))

for(i in 1:n)
{
  for(j in 1:m)
  {
    if(Sorted_D$ttl[i] >= W$tau[j])
    {
      pi[i,j] <- 1   
    }
    if((pi[i,j] == 1) & (Sorted_D$size[i] <= W$q[j]))
    {
      util[k] <- utility(Sorted_D$size[i], W$p[j])
      record_for_pi[k] <- i
      record[k,1] <- j
      record[k,2] <- W$q[j]
      #print(util[k])
      k <- k+1

      cost_offload[i,j] <- cost(c, C, Sorted_D$size[i], W$q[j])
      cost_offload[i,j] <- round(cost_offload[i,j] * 0.01, digits = 1)
      W$q[j] <- W$q[j] - Sorted_D$size[i]
      #print(W$q[j])
      pi[i,j] <- 0
    }else if(pi[i,j] == 1){
      util[k] <- utility(Sorted_D$size[i], W$p[j])
      if(util[k] > util[k-1])
      {
        pi[record_for_pi[k-1], record[k-1,1]] <- 1
        W$q[record[k-1,1]] <- record[k-1,2] #restores capacity 
        if(Sorted_D$size[i] <= W$q[j])
        {
          W$q[j] <- W$q[j] - Sorted_D$size[i]
          pi[i,j] <- 0
        }
      }
      k <- k+1
    }
  }
}

for(i in 1:n)
{
  for(j in 1:m)
  {
    if(pi[i,j] == 1)
    {
      cost_offload[i,j] <- 0
    }
  }
}

#Hungarian Method
#install.packages("clue")
library("clue")

#This Function will convert matrix to square matrix if it is not
convert_square_matrix <- function(cost_offload){
  row <- nrow(cost_offload)
  col <- ncol(cost_offload)
  iter <- 0
  if(row > col){
    iter <- row - col
  }else if(col > row)
  {
    iter <- col - row
  }
  
  if(row != col){
    if(row > col){
      for(i in 1:iter){
        cost_offload <- cbind(cost_offload, rep(0, times=row))
      }
    }else if(col > row){
      for(i in 1:iter){
        cost_offload <- rbind(cost_offload, rep(0, times=col))
      }
    }
  }
  
  return(cost_offload)
}

cost_offload <- convert_square_matrix(cost_offload)
row <- nrow(cost_offload)
col <- ncol(cost_offload)

#solve_LSAP is a function to perform assignment using hungarian method
if(row == col){
  x <- solve_LSAP(cost_offload, maximum = FALSE)
}
print(x)
total_cost <- sum(cost_offload[cbind(seq_along(x), x)])
print(total_cost)