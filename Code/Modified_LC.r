#There are 10 data items in increasing order
n <- 10
m <- 3

z <- sample(30000:40000, n)
z <- sort(z)

#Local Cost
l_c <- matrix(data=0, nrow=n,
              ncol=m)

l_e <- runif(m, min = 0, max = 1)
l_e <- round(l_e, 3)
l_e <- sort(l_e)

l_t <- runif(m, min = 0, max = 1)
l_t <- round(l_t, 3)
l_t <- sort(l_t)

alpha <- runif(m, min = 0, max = 1)
alpha <- round(alpha, 2)
alpha <- sort(alpha)

gamma <- sample(200:600, m)
gamma <- sort(gamma)
freq <- runif(m, min = 0, max = 1)
freq <- round(freq, 6)
freq <- sort(freq)

process_task <- function(z,gamma,freq){
  value <- (z*gamma)/freq
  returnValue(value)
}

for(i in 1:n)
{
  for(j in 1:m)
  {
    l_c[i,j] <- ((l_e[j])*(alpha[j])*((z[i]*gamma[j])/freq[j])) +
      ((l_t[j])*((z[i]*gamma[j])/freq[j]))
    l_c[i,j] <- l_c[i,j] / 1000000
  }
}

for(j in 1:m)
{
  l_c[,j] <- round(l_c[,j],digits=2)
}
data <- data.frame(alpha,freq,gamma,l_e,l_t)
plot_data <- data.frame(l_c[,1],l_c[,2],l_c[,3],z)
write.csv(plot_data,file="one.csv")
write.csv(data,file="two.csv")

#Graph 1
data <- read.csv("one.csv")
colnames(data)[1] <- "one"
colnames(data)[2] <- "two"
colnames(data)[3] <- "three"
Data <- c(1,2,3,4,5,6,7,8,9,10)
plot(Data, data$one, xlab='Data', ylab='Cost', type='o', col='green')
axis(1, seq(1,10,1))
lines(Data, data$two, type='o',col='purple')
lines(Data, data$three, type='o',col='red')