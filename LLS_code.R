### for all data, fed
library(gradDescent)
library(survival)
library(teachingApps)
library(pracma)
library(Rcpp)
library(CVXR)

setwd("/Users/su/FL/code/data_scale_normalize")   #file path

########################################
########################################
########################################
#customized GD - Normal distribution
gradientDesc <- function(x, y, learn_rate, conv_threshold, N, max_iter) {
  sigma <- runif(1,0,1)
  #sigma <- output.fed.surv$scale
  x <- as.matrix(x)
  p <- dim(x)[2]  #feature dimension with intercept term
  beta <- c(runif(p, 0, 1))
  #beta <- output.fed.surv$coefficients
  muhat <- x %*% beta 
  lprimeold <- 0
  N <- dim(x)[1]
  lprimenew <- sum(0.5* (c(sigma)*y - muhat)^2) - (N-3) * log(sigma)
  #lprimenew <- sum(0.5* (c(sigma)*y - muhat)^2) - (N) * log(sigma)
  converged = F
  iterations = 0
  lprimevalue<-c()
  diffvalue<-c()
  while(converged == F) {
    ## Implement the gradient descent algorithm
    temp1 <- (-N+3)/sigma
    #temp1 <- (-N)/sigma
    summ <- 0
    for (i in 1:N){
      term0 <- sigma * (y[i]^2)
      term01 <- x[i,] %*% beta %*% y[i]
      summ <- summ + term0- term01
    }
    temp2 <- summ
    grad_sigma <- temp1 + temp2
    tempsum <- 0
    for (i in 1:N){
      term1 <- c(sigma) * y[i] %*% x[i,]
      term2 <- x[i,] %*% beta %*% t(x[i,])
      term <- term2 - term1
      tempsum <- tempsum + term
    }
    grad_beta <- tempsum
    sigma_new <- sigma - learn_rate * grad_sigma
    beta_new <- beta - learn_rate * grad_beta
    sigma <- sigma_new
    beta <- c(beta_new)
    lprimeold <- lprimenew
    lprimenew <- sum(0.5* (c(sigma)*y - x %*% beta)^2) - (N-3) * log(sigma)
    #lprimenew <- sum(0.5* (c(sigma)*y - x %*% beta)^2) - (N) * log(sigma)
    update_new <- abs(lprimenew-lprimeold)/N
    lprimevalue <- c(lprimevalue,lprimenew)
    diffvalue <- c(diffvalue,update_new)
    
    iterations = iterations + 1
    if(iterations > max_iter) { 
      converged = T
      list_temp <- list()
      list_temp$sigma_opt <- sigma
      list_temp$beta_opt <- beta
      list_temp$iteration_maxiter <- iterations
      list_temp$lprimevalue <- lprimevalue
      list_temp$diffvalue <- diffvalue
      return(list_temp)
    }
  }
}

########################################
########################################
########################################
### SEV
#customized GD
gradientDescSEV <- function(x, y, learn_rate, conv_threshold, N, max_iter) {
  sigma <- runif(1,0,1)
  x <- as.matrix(x)
  p <- dim(x)[2]  #feature dimension with intercept term
  beta <- c(runif(p, 0, 1))
  muhat <- x %*% beta 
  lprimeold <- 0
  N <- dim(x)[1]
  lprimenew <- -sum((c(sigma)*y - muhat)) - (N-3) * log(sigma) + sum(exp(c(sigma)*y - muhat))
  update <- abs(lprimenew-lprimeold) / N
  converged = F
  iterations = 0
  while(converged == F) {
    temp1 <- (-N+3)/sigma
    summ <- 0
    for (i in 1:N){
      term0 <- exp(c(sigma)*y[i] - x[i,]%*%beta)
      term00 <- (1-term0) * y[i]
      summ <- term00 + summ
    }
    temp2 <- summ
    grad_sigma <- temp1 - temp2
    
    tempsum <- 0
    for (i in 1:N){
      term1 <- exp(c(sigma)*y[i] - x[i,]%*%beta)
      term11 <- 1-term1
      term2 <- term11 %*% x[i,]
      term <- term2
      tempsum <- tempsum + term
    }
    grad_beta <- tempsum
    sigma_new <- sigma - learn_rate * grad_sigma
    beta_new <- beta - learn_rate * grad_beta
    sigma <- sigma_new
    beta <- c(beta_new)
    lprimeold <- lprimenew
    lprimenew <- -sum((c(sigma)*y - x%*%beta)) - (N-3) * log(sigma) + sum(exp(c(sigma)*y - x%*%beta))
    update_new <- abs(lprimenew-lprimeold) / N
    iterations = iterations + 1
    if(iterations > max_iter) { 
      converged = T
      list_temp <- list()
      list_temp$sigma_opt <- sigma
      list_temp$beta_opt <- beta
      list_temp$iteration_maxiter <- iterations
      return(list_temp)
    }
  }
}

########################################
########################################
########################################
### Logistic
#customized GD
gradientDescLogistic <- function(x, y, learn_rate, conv_threshold, N, max_iter) {
  sigma <- runif(1,0,1)
  x <- as.matrix(x)
  p <- dim(x)[2]  #feature dimension with intercept term
  beta <- c(runif(p, 0, 1))
  muhat <- x %*% beta 
  lprimeold <- 0
  N <- dim(x)[1]
  lprimenew <- -sum((c(sigma)*y - muhat)) - (N-3) * log(sigma) + 2*sum(log(1+exp(c(sigma)*y - muhat)))
  update <- abs(lprimenew-lprimeold) / N
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    temp1 <- (3-N)/sigma
    summ <- 0
    for (i in 1:N){
      term0 <- 2*exp(c(sigma)*y[i] - x[i,]%*%beta)
      term00 <- 1+exp(c(sigma)*y[i] - x[i,]%*%beta)
      term01 <- term0/term00 -1
      summ <- y[i] * term01 + summ
    }
    temp2 <- summ
    grad_sigma <- temp1 + temp2
    tempsum <- 0
    for (i in 1:N){
      term1 <- exp(c(sigma)*y[i] - x[i,]%*%beta)
      term11 <- exp(c(sigma)*y[i] - x[i,]%*%beta) +1
      term10 <- term1/term11
      term2 <- (1-2*term10)%*%x[i,]
      term <- term2
      tempsum <- tempsum + term
    }
    grad_beta <- tempsum
    
    sigma_new <- sigma - learn_rate * grad_sigma
    beta_new <- beta - learn_rate * grad_beta
    sigma <- sigma_new
    beta <- c(beta_new)
    lprimeold <- lprimenew
    lprimenew <- -sum((c(sigma)*y - x%*%beta)) - (N-3) * log(sigma) + 2*sum(log(1+exp(c(sigma)*y - x%*%beta)))
    update_new <- abs(lprimenew-lprimeold) / N
    iterations = iterations + 1
    if(iterations > max_iter) { 
      converged = T
      list_temp <- list()
      list_temp$sigma_opt <- sigma
      list_temp$beta_opt <- beta
      list_temp$iteration_maxiter <- iterations
      return(list_temp)
    }
  }
}


########################################
########################################
########################################
####  RUN  (non-fed version -  aggregate three users, just to simplification)


indx <- c(61)   #take No.61 engine as the example
for (i in indx){
  ##changeable
  sv.ps.1 <- read.table(paste0("svd_engine_",i,"_user_",1, ".txt"), header = FALSE)[,-1]
  sv.ps.2 <- read.table(paste0("svd_engine_",i,"_user_",2, ".txt"), header = FALSE)[,-1]
  sv.ps.3 <- read.table(paste0("svd_engine_",i,"_user_",3, ".txt"), header = FALSE)[,-1]
  res.value.1 <- read.table(paste0("svd_engine_res_",i,"_user_",1, ".txt"), header = FALSE)[,-1]  #test, ignore this for now
  res.value.2 <- read.table(paste0("svd_engine_res_",i,"_user_",2, ".txt"), header = FALSE)[,-1]
  res.value.3 <- read.table(paste0("svd_engine_res_",i,"_user_",3, ".txt"), header = FALSE)[,-1]
  test.dt <- read.table(paste0("test_engine_",i,"fed.txt"), header = F)[,-1]  #test, this will be used to construct X\beta, which will use the output of this step; so for the following code, this is not used
  ##
  
  y1 <- read.table(paste0("engine_",i,"_user_",1, ".response.txt"), header = FALSE)
  y2 <- read.table(paste0("engine_",i,"_user_",2, ".response.txt"), header = FALSE)
  y3 <- read.table(paste0("engine_",i,"_user_",3, ".response.txt"), header = FALSE)
  y1 <- as.vector(y1[,2])
  y2 <- as.vector(y2[,2])
  y3 <- as.vector(y3[,2])
  y1 <- log(y1)
  y2 <- log(y2)
  y3 <- log(y3)
  
  sv.ps.c <- as.matrix(rbind(sv.ps.1, sv.ps.2, sv.ps.3))
  res.value.c <- c(res.value.1, res.value.2, res.value.3)
  y.c <- c(y1, y2, y3)   #response, so log version ~ normal(for example)
  
  sv.ps.c <- cbind(c(rep(1,dim(sv.ps.c)[1])),scale(sv.ps.c[,-1]))   #data matrix (input for second step)
  
  ##run the function
  N <- 100  #this is not important
  optimal_value <- list()
  optimal_value <- gradientDesc(sv.ps.c, y.c, 1e-5, 1e-5, N, 1e5)
  
  ############################
  #### simulated data - to use, uncomment corresponding code#####
  # mu <- 1
  # sigma <- 4
  # N <- 1000
  # x <- matrix(rnorm(N*10),nrow=N)
  # x <- cbind(c(rep(1,N)), x) #add intercept term
  # y <- rsev(N, loc=mu, scale=sigma)
  #############################
  #optimal_value <- gradientDesc(x, y, 1e-5, 1e-5, N, 50000)  #for simulated data
  
  beta_optimal <- optimal_value$beta_opt
  sigma_optimal <- optimal_value$sigma_opt
  #verify
  1/sigma_optimal    
  beta_optimal/c(sigma_optimal)     #CUSTOMIZED GD
  
  output.fed.surv <- survreg(Surv(y.c)~sv.ps.c -1, dist="gaussian")
  #output.fed.surv <- survreg(Surv(y)~x -1, dist="gaussian")
  #survreg(Surv(exp(y))~x -1, dist="loggaussian")
  output.fed.surv     #SURVIVAL PACKAGE
  
  ##############
  ##############
  #try SEV
  optimal_value_sev <- gradientDescSEV(sv.ps.c, y.c, 5e-6, 5e-6, N, 50000)
  #optimal_value_sev <- gradientDescSEV(x, y, 1e-6, 1e-6, N, 30000)
  beta_optimal <- optimal_value_sev$beta_opt
  sigma_optimal <- optimal_value_sev$sigma_opt
  #verify
  1/sigma_optimal
  beta_optimal/c(sigma_optimal)
  
  output.fed.surv.sev <- survreg(Surv(y.c) ~ sv.ps.c -1, dist='extreme')
  output.fed.surv.sev <- survreg(Surv(y) ~ x -1, dist='extreme')
  output.fed.surv.sev
  
}



########################################
########################################
########################################
## CVX package

###normal check
#X <- x
#y <- y

X <- sv.ps.c   #pca score (first step output)
y <- y.c    #response value

N <- dim(X)[1]
p <- dim(X)[2]
a <- c(rep(0,p),1)
b <- cbind(-X, y)

w <- Variable(p+1)   #first p values are betas, the last value is sigma
objective <- ((-N - 3) * log(t(a)%*%w)) + (0.5* sum((b %*% w)^2))
constraints <- list(t(a) %*% w >= 0)
prob <- Problem(Minimize(objective), constraints)
result <- solve(prob)
weights <- result$getValue(w)
weights







