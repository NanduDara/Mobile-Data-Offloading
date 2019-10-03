#Take number of mobile devices as a input
# n <- readline(prompt = "Enter Number of Mobile Devices:")
# n <- as.numeric(n)
n <- 5
#Declaring variables to Calculate cost if task
#is performed at the device itself without offloading
l_e <- runif(n, min = 0, max = 0)
l_e <- round(l_e, 3)

l_t <- runif(n, min = 0, max = 0)
l_t <- round(l_t, 3)

alpha <- runif(n, min = 0, max = 0)
alpha <- round(alpha, 2)

z <- sample(30000:40000, n)
gamma <- sample(n)
freq <- runif(n, min = 0, max = 0)

l_c <- round(runif(n, min = 0, max = 0),digits=0)

data <- data.frame(l_e, l_t, alpha, z, gamma, freq, l_c)

#Cost if Task is performed at mobile device itself
process_task <- function(z,gamma,freq){
  value <- (z*gamma)/freq
  returnValue(value)
}

record1 <- c()
record2 <- c()
record3 <- c()
record4 <- c()
record5 <- c()
k <- 0
iter <- 10
for(a in 1:iter)
{
  data$l_e <- runif(n, min = 0, max = 1)
  data$l_e <- round(data$l_e, 2)
  
  data$l_t <- runif(n, min = 0, max = 1)
  data$l_t <- round(data$l_t, 2)
  
  data$alpha <- runif(n, min = 0, max = 1)
  data$alpha <- round(data$alpha, 2)
  
  data$gamma <- sample(200:600, n)
  data$freq <- runif(n, min = 0, max = 1)
  data$freq <- round(data$freq, 4)
  
  data$l_c <- round(runif(n, min = 0, max = 0),digits=0)
  
  for(i in 1:n){
    data$l_c[i] <- ((data$l_e[i])*(data$alpha[i])*(process_task(data$z[i],data$gamma[i],data$freq[i]))) +
      ((data$l_t)*(process_task(data$z[i],data$gamma[i],data$freq[i])))
    data$l_c[i] <- data$l_c[i] / 1000000
    data$l_c[i] <- round(data$l_c[i],digits=0)
  }
  k<-k+1
  record1[k] <- data$l_c[1]
  record2[k] <- data$l_c[2]
  record3[k] <- data$l_c[3]
  record4[k] <- data$l_c[4]
  record5[k] <- data$l_c[5]
}

data1 <- data.frame(record1, record2, record3, record4, record5)
colnames(data1) <- c(1,2,3,4,5)
#o is best option
it <- c(1,2,3,4,5,6,7,8,9,10)
plot(it, data1$"1", xaxt = "none", yaxt = "none", xlab="Size od Data", ylab="Cost", type = "o", col = "green")
axis(1, seq(1, 10, 1))
axis(2, c(10,15,20))
lines(it, data1$"2", col = "red", type = "o")
lines(it, data1$"3", col = "purple", type = "o")
lines(it, data1$"4", col = "black", type = "o")
lines(it, data1$"5", col = "yellow", type = "o")
#plot(l_c, type = "l")