#Computation Time
n <- 10
m <- 3

z <- sample(30000:40000, n)
z <- sort(z)

gamma <- sample(200:600, m)
gamma <- sort(gamma)
freq <- runif(m, min = 0, max = 1)
freq <- round(freq, 3)
freq <- sort(freq,decreasing = TRUE)

c_t <- matrix(data=0, nrow=n,
              ncol=m)
for(i in 1:n)
{
  for(j in 1:m)
  {
    c_t[i,j] <- ((z[i]*gamma[j])/freq[j])
    c_t[i,j] <- c_t[i,j] / 1000000
  }
  c_t[i,] <- round(c_t[i,],digits=1)
}
data <- data.frame(c_t[,1], c_t[,2], c_t[,3], z)
data1 <- data.frame(gamma, freq)
write.csv(data, "LC1_1.csv")
write.csv(data1, "LC1_2.csv")

data <- read.csv("LC1_1.csv")
data$X <- NULL
colnames(data)[1] <- "one"
colnames(data)[2] <- "two"
colnames(data)[3] <- "three"
#Graph 2
Data <- c(1,2,3,4,5,6,7,8,9,10)
plot(Data, data$one, xlab='Data', ylab='Time', type='o', col='green')
#axis(1, seq(1,10,1))
lines(Data, data$two, type='o',col='purple')
lines(Data, data$three, type='o',col='red')