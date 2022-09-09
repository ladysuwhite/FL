library(FactoMineR)
library(ade4)
library(amap)
library(matrixcalc)
library(pracma)
library(plyr)
library(dplyr)
library(expm) 
library(purrr)
library(rpca)
library(survival)
library(teachingApps)
library(purrr)

load('....../Git/NonFedNormalOnly/Tryout_Step0/Step0.RData')  #it has the test.list(test data) after data cleaning
setwd("....../Git/NonFedNormalOnly/CMAPSSData")
rul <- read.table('RUL_FD001.txt', header = F)[,1][-c(49)]   #read RUL
indx <- c(1:99)  #there are 99 test engines left to be tested
for (i in indx){
  testdt <- test.list[[i]]  #testdata
  ### non-fedrated version ###
  setwd("....../Git/NonFedNormalOnly/Tryout_Step0/")
  user1 <- read.table(paste0("engine_",i,"_user_",1, ".txt"), header = FALSE)   #after data cleaning
  user2 <- read.table(paste0("engine_",i,"_user_",2, ".txt"), header = FALSE)
  user3 <- read.table(paste0("engine_",i,"_user_",3, ".txt"), header = FALSE)
  y1 <- read.table(paste0("engine_",i,"_user_",1, ".response.txt"), header = FALSE)  #response
  y2 <- read.table(paste0("engine_",i,"_user_",2, ".response.txt"), header = FALSE)
  y3 <- read.table(paste0("engine_",i,"_user_",3, ".response.txt"), header = FALSE)
  y1 <- as.vector(y1[,2])
  y2 <- as.vector(y2[,2])
  y3 <- as.vector(y3[,2])
  user1 <- as.matrix(user1[,-1])
  user2 <- as.matrix(user2[,-1])
  user3 <- as.matrix(user3[,-1])
  
  user.all <- rbind(user1, user2, user3)  #combine all data together, and does not separate with users
  
  ### non-fed version 
  candi.x <- user.all
  denom.h <- F2norm(candi.x)^2
  svdd <- svd(candi.x)$d
  FVEprop <- 0.99
  p <- dim(candi.x)[1]
  #p <- dim(candi.x)[2]
  kll <- 1
  temphere <- 0
  while(kll <= p & temphere < FVEprop){
    temphere <- sum((svdd^2)[1:kll])/denom.h
    if (temphere >= FVEprop){
      print(paste0("engine ",i," has ",kll," PCs selected."))
      ppp = kll
    }
    kll <- kll + 1
  }
 # if(kll > p){ppp = p}
  step1.op.nf <- (candi.x %*% (svd(candi.x)$v))[, 1:ppp]  
  while(is.vector(step1.op.nf) == TRUE){step1.op.nf <- as.matrix(step1.op.nf)}
  step1.op.nf <- cbind(c(rep(1,dim(step1.op.nf)[1])),step1.op.nf)  #add intercept
  
  test.2a.op <- (testdt %*% (svd(candi.x)$v))[1:ppp]    #1*...
  test.2a.op <- t(c(1,test.2a.op))
  
  #2a3b
  setwd('....../Git/NonFedNormalOnly')
  write.table(step1.op.nf, file = paste0("step1_2a3b_",i,"_user_all.txt"), col.names = F)
  write.table(test.2a.op, file = paste0("test_2a_",i,"_user_all.txt"), col.names = F)
}



####SECOND STEP - NORMAL ONLY
e.nf.normal <- c()

indx <- c(1:99)
for (i in indx){
  ##changeable
  setwd('....../Git/NonFedNormalOnly')
  y1 <- read.table(paste0("engine_",i,"_user_",1, ".response.txt"), header = FALSE)
  y2 <- read.table(paste0("engine_",i,"_user_",2, ".response.txt"), header = FALSE)
  y3 <- read.table(paste0("engine_",i,"_user_",3, ".response.txt"), header = FALSE)
  y1 <- as.vector(y1[,2])
  y2 <- as.vector(y2[,2])
  y3 <- as.vector(y3[,2])
  y1 <- log(y1)
  y2 <- log(y2)
  y3 <- log(y3)
  
  setwd('/Users/su/FL/code/data_scale_normalize/SVDoutput/SepNewHPCOutput/Tryout')
  y.c <- c(y1, y2, y3)   #response
  ###non fed
  sv.ps.nf <- read.table(paste0("step1_2a3b_",i,"_user_all", ".txt"), header = FALSE)[,-1]
  # if (dim(sv.ps.nf)[2]>dim(sv.ps.c)[2]) {
  #   kkk = dim(sv.ps.c)[2]}else {
  #     kkk <- dim(sv.ps.nf)[2]}
  sv.ps.nf <- cbind(c(rep(1,dim(sv.ps.nf)[1])),scale(sv.ps.nf[,-1]))  #data matrix
  test.dt.nf <- read.table(paste0("test_2a_",i,"_user_all.txt"), header = FALSE)[,-1]
  ##run the function
  output.nfed.normal <- survreg(Surv(y.c)~sv.ps.nf -1, dist="gaussian")
  output.nfed.normal.beta <- as.vector(output.nfed.normal$coefficients)
  output.nfed.normal.sigma <- output.nfed.normal$scale
  esti.nfed.normal <- sum(test.dt.nf * output.nfed.normal.beta)
  e.nf.normal <- c(e.nf.normal, exp(esti.nfed.normal))
} 


e.nf.normal.re <- abs(e.nf.normal - rul)/rul

par(mfrow = c(1,2))
boxplot(e.nf.normal.re)
boxplot(e.nf.normal.re, outline = FALSE)



