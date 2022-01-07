y = c(1.64, 1.70, 1.72, 1.74, 1.82, 1.82, 
      1.82, 1.90, 2.08)
n <- length(y)

ybar <- mean(y)

# hypothetical posterior
post_theta <- function(theta)
{
  exp(-1/3*n*theta^2*ybar)
}

# to see
theta = seq(-3, 3, length = 51)
plot(theta,post_theta(theta),type="l",lwd=2)

#Metropolis
nsim <- 2000
theta <- numeric(nsim+1)
theta[1] <- -1

cnt <- 0

for(t in 2:nsim){
  #1 Sample theta from proposal
  theta.star <- rnorm(1,0,100)
 
  #2 Ratio
  r <- post_theta(theta.star)/post_theta(theta[t-1])
 
  #3 Accept
  u <- runif(1)
  if(u<r) { 
    theta[t] <- theta.star
    cnt <- cnt + 1
    } else { 
      theta[t] = theta[t-1]
    }
}

 
plot(theta,type="l",col="red",main=paste("Acceptance =",round(cnt/nsim*100,1),"%"))
d <- density(theta)
plot(d)
 
 
 
 



