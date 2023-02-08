##boxplot three distributions in one figure
library(ggplot2)
require(reshape2)
BP_rela_error <- melt(data.frame(LN_test_rela_err, WB_test_rela_err, LL_test_rela_err))  #dataframe
#colnames(BP_rela_error) <- c("LogNormal", "Weibull", "LogLogistic")
colnames(BP_rela_error) <- c("distribution", "relative_error")
ggplot(BP_rela_error, aes(x=factor(distribution), y=relative_error, fill=factor(distribution))) +
  geom_boxplot()+
  #geom_boxplot(outlier.shape=NA) +
  #geom_jitter(position=position_jitter(0.2))
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1)+
  ggtitle("Relative Error of Three Distributions") +
  xlab("Distribution") +
  ylab("Relative error")


quantile(LN_test_rela_err, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(WB_test_rela_err, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(LL_test_rela_err, probs = c(0, 0.25, 0.5, 0.75, 1))


# sub-boxplot by distribution
setwd("")
test03 <- read.delim("test_FD03.txt", header = F, sep = ' ')
rul03 <-read.delim("RUL_FD03.txt", header = F)
max03 <- aggregate(test03$V2, by=list(test03$V1), max)   #max value of each engine
max03 <- max03$x
rul03 <- as.vector(rul03$V1)  #rul of engines
lifetime03 <- max03+rul03  #total lifetime(true) of engines
percent03<- max03/lifetime03*100 #given lifetime percentage
sp03 <- sort(percent03)
groups<- c(sum(sp03>=20 & sp03 <=36), sum(sp03>36 & sp03 <=52),
           sum(sp03>52 & sp03 <=68), sum(sp03>68 & sp03 <=84),
           sum(sp03>84 & sp03<=100))
#groups[2]<-groups[1]+groups[2];groups[3]<-groups[3]+groups[2];groups[4]<-groups[4]+groups[3];groups[5]<-groups[5]+groups[4]
rank_percent03 <- rank(percent03) #index of ordered percentage
group_rp03 <- split(rank_percent03, rep(1:5, groups))     ##group of index of percentage
#group_rp03 is a list

#LogNormal
LN_group1 <- data.frame(group = "group1", value = LN_test_rela_err[c(group_rp03[[1]])])
LN_group2 <- data.frame(group = "group2", value = LN_test_rela_err[c(group_rp03[[2]])])
LN_group3 <- data.frame(group = "group3", value = LN_test_rela_err[c(group_rp03[[3]])])
LN_group4 <- data.frame(group = "group4", value = LN_test_rela_err[c(group_rp03[[4]])])
LN_group5 <- data.frame(group = "group5", value = LN_test_rela_err[c(group_rp03[[5]])])
LN_group<- data.frame()
LN_group <- rbind(LN_group1, LN_group2, LN_group3, LN_group4, LN_group5)
colnames(LN_group) <- c("group", "relative_error")
ggplot(LN_group, aes(x=group, y=relative_error, fill=group)) +
  geom_boxplot()+
  #geom_boxplot(outlier.shape=NA) +
  #geom_jitter(position=position_jitter(0.2))+
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
  ggtitle("Relative Error by Group of LogNormal Distributions") +
  xlab("group") +
  ylab("Relative error")


#Weibull
  WB_group1 <- data.frame(group = "group1", value = WB_test_rela_err[c(group_rp03[[1]])])
  WB_group2 <- data.frame(group = "group2", value = WB_test_rela_err[c(group_rp03[[2]])])
  WB_group3 <- data.frame(group = "group3", value = WB_test_rela_err[c(group_rp03[[3]])])
  WB_group4 <- data.frame(group = "group4", value = WB_test_rela_err[c(group_rp03[[4]])])
  WB_group5 <- data.frame(group = "group5", value = WB_test_rela_err[c(group_rp03[[5]])])
  WB_group<- data.frame()
  WB_group <- rbind(WB_group1, WB_group2, WB_group3, WB_group4, WB_group5)
  colnames(WB_group) <- c("group", "relative_error")
  ggplot(WB_group, aes(x=group, y=relative_error, fill=group)) +
    geom_boxplot()+
    #geom_boxplot(outlier.shape=NA) +
    #geom_jitter(position=position_jitter(0.2))+
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
    ggtitle("Relative Error by Group of Weibull Distributions") +
    xlab("group") +
    ylab("Relative error")
  

#LogLogistic
  LL_group1 <- data.frame(group = "group1", value = LL_test_rela_err[c(group_rp03[[1]])])
  LL_group2 <- data.frame(group = "group2", value = LL_test_rela_err[c(group_rp03[[2]])])
  LL_group3 <- data.frame(group = "group3", value = LL_test_rela_err[c(group_rp03[[3]])])
  LL_group4 <- data.frame(group = "group4", value = LL_test_rela_err[c(group_rp03[[4]])])
  LL_group5 <- data.frame(group = "group5", value = LL_test_rela_err[c(group_rp03[[5]])])
  LL_group<- data.frame()
  LL_group <- rbind(LL_group1, LL_group2, LL_group3, LL_group4, LL_group5)
  colnames(LL_group) <- c("group", "relative_error")
  ggplot(LL_group, aes(x=group, y=relative_error, fill=group)) +
    geom_boxplot()+
    #geom_boxplot(outlier.shape=NA) +
    #geom_jitter(position=position_jitter(0.2))+
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
    ggtitle("Relative Error by Group of LogLogistic Distributions") +
    xlab("group") +
    ylab("Relative error")
  

#####
### another splitting method
####
  setwd("")
  test03 <- read.delim("test_FD03.txt", header = F, sep = ' ')
  rul03 <-read.delim("RUL_FD03.txt", header = F)
  max03 <- aggregate(test03$V2, by=list(test03$V1), max)   #max value of each engine
  max03 <- max03$x
  rul03 <- as.vector(rul03$V1)  #rul of engines
  lifetime03 <- max03+rul03  #total lifetime(true) of engines
  percent03<- max03/lifetime03*100 #given lifetime percentage
  sp03 <- sort(percent03)
  groups<- c(sum(sp03>=0 & sp03 <=40), sum(sp03>40 & sp03 <=60),
             sum(sp03>60 & sp03 <=80), sum(sp03>80 & sp03 <=100))
  #groups[2]<-groups[1]+groups[2];groups[3]<-groups[3]+groups[2];groups[4]<-groups[4]+groups[3];groups[5]<-groups[5]+groups[4]
  rank_percent03 <- rank(percent03) #index of ordered percentage
  group_rp03 <- split(rank_percent03, rep(1:4, groups))     ##group of index of percentage
  #group_rp03 is a list
  
  #LogNormal
  LN_group1 <- data.frame(group = "group1", value = LN_test_rela_err[c(group_rp03[[1]])])
  LN_group2 <- data.frame(group = "group2", value = LN_test_rela_err[c(group_rp03[[2]])])
  LN_group3 <- data.frame(group = "group3", value = LN_test_rela_err[c(group_rp03[[3]])])
  LN_group4 <- data.frame(group = "group4", value = LN_test_rela_err[c(group_rp03[[4]])])
  LN_group<- data.frame()
  LN_group <- rbind(LN_group1, LN_group2, LN_group3, LN_group4)
  colnames(LN_group) <- c("group", "relative_error")
  ggplot(LN_group, aes(x=group, y=relative_error, fill=group)) +
    #geom_boxplot()+
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position=position_jitter(0.2))+
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
    ggtitle("Relative Error by Group of LogNormal Distributions") +
    xlab("group") +
    ylab("Relative error")
  
  
  #Weibull
  WB_group1 <- data.frame(group = "group1", value = WB_test_rela_err[c(group_rp03[[1]])])
  WB_group2 <- data.frame(group = "group2", value = WB_test_rela_err[c(group_rp03[[2]])])
  WB_group3 <- data.frame(group = "group3", value = WB_test_rela_err[c(group_rp03[[3]])])
  WB_group4 <- data.frame(group = "group4", value = WB_test_rela_err[c(group_rp03[[4]])])
  WB_group<- data.frame()
  WB_group <- rbind(WB_group1, WB_group2, WB_group3, WB_group4)
  colnames(WB_group) <- c("group", "relative_error")
  ggplot(WB_group, aes(x=group, y=relative_error, fill=group)) +
    #geom_boxplot()+
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position=position_jitter(0.2))+
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
    ggtitle("Relative Error by Group of Weibull Distributions") +
    xlab("group") +
    ylab("Relative error")
  
  
  #LogLogistic
  LL_group1 <- data.frame(group = "group1", value = LL_test_rela_err[c(group_rp03[[1]])])
  LL_group2 <- data.frame(group = "group2", value = LL_test_rela_err[c(group_rp03[[2]])])
  LL_group3 <- data.frame(group = "group3", value = LL_test_rela_err[c(group_rp03[[3]])])
  LL_group4 <- data.frame(group = "group4", value = LL_test_rela_err[c(group_rp03[[4]])])
  LL_group<- data.frame()
  LL_group <- rbind(LL_group1, LL_group2, LL_group3, LL_group4)
  colnames(LL_group) <- c("group", "relative_error")
  ggplot(LL_group, aes(x=group, y=relative_error, fill=group)) +
    #geom_boxplot()+
    geom_boxplot(outlier.shape=NA) +
    geom_jitter(position=position_jitter(0.2))+
    stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="white")+
    ggtitle("Relative Error by Group of LogLogistic Distributions") +
    xlab("group") +
    ylab("Relative error")
  
  
  
 Difference<-seq(-50,50,0.1) 
 RMSE<-abs(Difference)
 di_p<-seq(0,50,0.1)
 di_n<-seq(-50,-0.1,0.1)
 score1<-exp(di_p/10)-1  #positive part
 score2<-exp(-di_n/13)-1   #negative part
 Score<-c(score2,score1)
 rmsescore<-data.frame(Difference,RMSE,Score)
 rmsescore <- reshape2::melt(rmsescore, id.var='Difference')
 names(rmsescore)[names(rmsescore) == "value"] <- "Value"
 names(rmsescore)[names(rmsescore) == "variable"] <- "Criteria"
 
 ggplot(rmsescore, aes(x=Difference, y=Value, col=Criteria)) + 
   geom_line(aes(linetype=Criteria, color=Criteria)) +
   theme_gray(base_size = 14)+ theme_bw()
 
 ggplot(df2, aes(x=time, y=bill, group=sex)) +
   geom_line(aes(linetype=sex, color=sex))+
   geom_point(aes(color=sex))+
   theme(legend.position="top")
 
 