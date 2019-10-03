#For Offloading Data item from One Mobile Device to another Mobile Device
iter <- 10 #iteration number
n <- 10
v1 <- 15
data <- data.frame(cost <- sample(20:40, n), win <- rep(0, times=n),
                   cost1 <- rep(0, times=n), cost2 <- rep(0, times=n))

colnames(data)[1] <- "cost"
colnames(data)[2] <- "win"
colnames(data)[3] <- "cost1"
colnames(data)[4] <- "cost2"

while((median(data$cost)-v1)>2)
{
  v1 <- v1 + sample(1:4,1)
  print(v1)
}

for(a in 1:iter)
{
  #odd iteration
  if((a%%2) == 1)
  {
    minc <- 17
    maxc <- 42
    
    #We normalize the costs because least and higher cost are fixed 
    for(i in 1:n)
    {
      data$cost1[i] <- (data$cost[i] - min(data$cost))/(max(data$cost) - min(data$cost))
      data$cost1[i] <- ((data$cost1[i])*(maxc - minc)) + minc
      data$cost1[i] <- round(data$cost1[i], digits = 0)
    }
    
    min_cost1 <- min(data$cost1)
    winner <- 0
    for(i in 1:n)
    {
      if(min_cost1 == data$cost1[i])
      {
        winner <- i
      }
    }
    data$win[winner] <- data$win[winner]+1
  }
  
  #even iteration
  if((a%%2) == 0)
  {
    minc <- 15
    maxc <- 35
    
    #We normalize the costs because least and higher cost are fixed 
    for(i in 1:n)
    {
      data$cost2[i] <- (data$cost[i] - min(data$cost))/(max(data$cost) - min(data$cost))
      data$cost2[i] <- ((data$cost2[i])*(maxc - minc)) + minc
      data$cost2[i] <- round(data$cost2[i], digits = 0)
    }
    
    min_cost2 <- min(data$cost2)
    winner <- 0
    for(i in 1:n)
    {
      if(min_cost2 == data$cost2[i])
      {
        winner <- i
      }
    }
    data$win[winner] <- data$win[winner]+1
  }
  
  #Stopping Condition Function
  #It says no mobile is updating its cost i.e all reached equilibrium
  stop_cond <- function(data)
  {
    for(j in 1:n)
    {
      if(((data$cost1[j])-(data$cost2[j])) == 0)
      {
        break;
      }
      else
      {
        return(0)
      }
    }
    return(1)
  }
  
  if(stop_cond(data) == 1)
  {
    print("Reached Stopping Condition")
  }else{
    iteration <- function(cost){
      cost <- cost-(round(runif(1,min=1,max=4), digits=1))
      #print(cost)
      return(cost)
    }

    for(i in 1:n){
      if((i!=winner) && (data$cost[i] >= 10))
      {
        data$cost[i] <- iteration(data$cost[i])
      }
    }
  }
}

#Declaration of Winner
c <- 0
d <- 0
pos <- c()
for(i in 1:10){
  if(data$win[i] == max(data$win)){
    c <- c+1
    pos[c] <- i
    d <- c
  }
}

min_cos <- max(data$cost)
if(c == 1)
{
  sprintf("Final Mobile Device to Offload is: %d",pos[c])
}else{
  for(i in 1:d)
  {
    if(min_cos >= data$cost[pos[i]])
    {
      min_cos <- data$cost[pos[i]]
    }
  }
  for(i in 1:d)
  {
    if(min_cos == data$cost[pos[i]])
    {
      print("Final Mobile Device to Offload is")
      print(pos[i])
    }
  }
}