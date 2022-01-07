CM <- 10000000
beta.sims <- rbeta(M, 3, 3)
round(sum(beta.sims)/M,4)


# define P
P <- matrix(c(0.5, 0.3, 0.2, 
0.2, 0.7, 0.1, 
0.1, 0.3, 0.6),3,3,byrow=T)
tol <- 1
cnt <- 0
Pold <- P
while(tol >= 1.0E-5){
  Pnew <- Pold%*%P
  tol<- max(as.vector(Pold-Pnew))
  Pold <- Pnew
  cnt <- cnt +1
}


  
state <- matrix(c(0.35,.45,.15),ncol=3)

state <- state%*%(mat);state




