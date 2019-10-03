#For Offloading Data item from One Mobile Device to another Mobile Device
iter <- 10 #iteration number
n <- 10
v1 <- 15
data <- data.frame(cost <- sample(40:45, n, replace = TRUE), time <- sample(15:20, n, replace = TRUE), win <- rep(0, times=n),
                   norc <- rep(0, times=n), nort <- rep(0, times=n),
                   obj1 <- rep(0, times=n), obj2 <- rep(0, times=n))

colnames(data)[1] <- "cost"
colnames(data)[2] <- "time"
colnames(data)[3] <- "win"
colnames(data)[4] <- "norc"; colnames(data)[5] <- "nort"
colnames(data)[6] <- "obj1"; colnames(data)[7] <- "obj2"

# k <- 0
# record4 <- c()
while((median(data$cost)-v1)>2)
{
  k <- k+1
  v1 <- v1 + sample(1:4,1)
  #record4[k] <- v1
  #print(v1)
}
# while(k != 10)
# {
#   k <- k+1
#   record4[k] <- record4[k-1]
# }
#print(record4)

# data1 <- data.frame(record1,record2,record3,record4)
# write.csv(data1, "LC_vs_Cost.csv")
k <- 0
# record1 <- c()
# record2 <- c()
# record3 <- c()
# record4 <- c()
# record5 <- c()
# record6 <- c()
# record7 <- c()
# record8 <- c()
# record9 <- c()
# record10 <- c()
# record1_1 <- c()
# record2_1 <- c()
# record3_1 <- c()
# record4_1 <- c()
# record5_1 <- c()
# record6_1 <- c()
# record7_1 <- c()
# record8_1 <- c()
# record9_1 <- c()
# record10_1 <- c()
for(a in 1:iter)
{
  #odd iteration
  if((a%%2) == 1)
  {
    minc <- 17
    maxc <- 42
    
    #We normalize the cost
    for(i in 1:n)
    {
      data$norc[i] <- (data$cost[i] - min(data$cost))/(max(data$cost) - min(data$cost))
      data$norc[i] <- ((data$norc[i])*(maxc - minc)) + minc
      data$norc[i] <- round(data$norc[i], digits = 0)
    }
    mint <- 7
    maxt <- 17
    
    #We normalize the time
    for(i in 1:n)
    {
      data$nort[i] <- (data$time[i] - min(data$time))/(max(data$time) - min(data$time))
      data$nort[i] <- ((data$nort[i])*(maxt - mint)) + mint
      data$nort[i] <- round(data$nort[i], digits = 0)
    }
    
    wt <- 0.3
    wc <- 0.7
    
    for(i in 1:n)
    {
      data$obj1[i] <- (wt*data$time[i])+(wc*data$cost[i])
    }
    
    for(i in 1:n)
    {
      winner <- 0
      if(min(data$obj1) == data$obj1[i])
      {
        winner <- i
        data$win[winner] <- data$win[winner]+1
      }
    }
  }else if((a%%2) == 0)
  {
    minc <- 17
    maxc <- 42
    
    #We normalize the cost
    for(i in 1:n)
    {
      data$norc[i] <- (data$cost[i] - min(data$cost))/(max(data$cost) - min(data$cost))
      data$norc[i] <- ((data$norc[i])*(maxc - minc)) + minc
      data$norc[i] <- round(data$norc[i], digits = 0)
    }
    mint <- 7
    maxt <- 17
    
    #We normalize the time
    for(i in 1:n)
    {
      data$nort[i] <- (data$time[i] - min(data$time))/(max(data$time) - min(data$time))
      data$nort[i] <- ((data$nort[i])*(maxt - mint)) + mint
      data$nort[i] <- round(data$nort[i], digits = 0)
    }
    
    wt <- 0.3
    wc <- 0.7
    
    for(i in 1:n)
    {
      data$obj2[i] <- (wt*data$time[i])+(wc*data$cost[i])
    }
    
    for(i in 1:n)
    {
      winner <- 0
      if(min(data$obj1) == data$obj1[i])
      {
        winner <- i
        data$win[winner] <- data$win[winner]+1
      }
    }
  }
  
  #Stopping Condition Function
  #It says no mobile will update its cost and time i.e all reached equilibrium(if its value is 1)
  stop_cond <- function(data)
  {
    for(i in 1:n)
    {
      if(((data$obj1[i])-(data$obj2[i])) == 0)
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
    print(a)
    print("Reached Stopping Condition")
  }else{
    iteration_c <- function(cost){
      cost <- cost-(round(runif(1,min=1,max=4), digits=1))
      #print(cost)
      return(cost)
    }
    
    for(i in 1:n){
      if((i!=winner) && (data$cost[i] >= 10))
      {
        data$cost[i] <- iteration_c(data$cost[i])
      }
    }
    
    iteration_t <- function(time){
      time <- time-(round(runif(1,min=1,max=4), digits=1))
      #print(time)
      return(time)
    }
    
    for(i in 1:n){
      if((i!=winner) && (data$time[i] >= 9))
      {
        data$time[i] <- iteration_t(data$time[i])
      }
    }  
  }
  k <- k+1
  # record1[k] <- data$cost[1]
  # record2[k] <- data$cost[2]
  # record3[k] <- data$cost[3]
  # record4[k] <- data$cost[4]
  # record5[k] <- data$cost[5]
  # record6[k] <- data$cost[6]
  # record7[k] <- data$cost[7]
  # record8[k] <- data$cost[8]
  # record9[k] <- data$cost[9]
  # record10[k] <- data$cost[10]
  # record1_1[k] <- data$time[1]
  # record2_1[k] <- data$time[2]
  # record3_1[k] <- data$time[3]
  # record4_1[k] <- data$time[4]
  # record5_1[k] <- data$time[5]
  # record6_1[k] <- data$time[6]
  # record7_1[k] <- data$time[7]
  # record8_1[k] <- data$time[8]
  # record9_1[k] <- data$time[9]
  # record10_1[k] <- data$time[10]
}

# data1 <- data.frame(record2,record3,record5,record7,record2_1,record3_1,record5_1,record7_1)
# write.csv(data1, "Iter_vs_Cost_vs_Time.csv")
# data1 <- data.frame(record1,record4,record5,record6)
# write.csv(data1, "Iter_vs_MD_vs_Win.csv")
# data1 <- data.frame(record1_1,record2_1,record3_1,record4_1)
# data2 <- data.frame(record1_2,record2_2,record3_2,record4_2)
# write.csv(data1, "one.csv")
# write.csv(data2, "two.csv")
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

if(c == 1)
{
  sprintf("Final Mobile Device to Offload is: %d",pos[c])
}else{
  if((iter%%2)==0)
  {
    min_obj <- max(data$obj2)
    for(i in 1:d)
    {
      if(min_obj >= data$obj2[pos[i]])
      {
        min_obj <- data$obj2[pos[i]]
      }
    }
    for(i in 1:d)
    {
      if(min_obj == data$obj2[pos[i]])
      {
        print("Final Mobile Device to Offload is:")
        print(pos[i])
      }
    }
  }else if((iter%%2)==1)
  {
    min_obj <- max(data$obj1)
    for(i in 1:d)
    {
      if(min_obj >= data$obj1[pos[i]])
      {
        min_obj <- data$obj1[pos[i]]
      }
    }
    for(i in 1:d)
    {
      if(min_obj == data$obj1[pos[i]])
      {
        print("Final Mobile Device to Offload is:")
        print(pos[i])
      }
    }
  }
}
print(data$win)