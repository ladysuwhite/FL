setwd("....../Git/NonFedNormalOnly/CMAPSSData")

train.data <- read.table("CMAPSSData/train_FD001.txt", header = FALSE, sep=" ") 
test.data <- read.table("CMAPSSData/test_FD001.txt", header = FALSE, sep=" ")
rul.test <- read.table("CMAPSSData/RUL_FD001.txt", header = FALSE)
train.data <- train.data[,-c(3,4,5,27,28)] #remove three operation conditions and the last two null columns
test.data <- test.data[,-c(3,4,5,27,28)]  #same for test data
train.data <- train.data[,-c(3,7,8,12,18,20,21,4,5,6,13,14,15,16,17,19,22,23)] #remove 7 flat signals - 1,5,6,10,16,18,19...UPDATE: only keeep 7&9
test.data <- test.data[,-c(3,7,8,12,18,20,21,4,5,6,13,14,15,16,17,19,22,23)]

#rename the columns, there are 14 columns left
#4, 15, 17, 20
colnames(train.data) <- c("Engine", "Cycle","S7")
colnames(test.data) <- c("Engine", "Cycle", "S7")
train.lifecycle <- plyr::count(train.data[,"Engine"])[,2] #life cycle of each engine in train dataset 
#test.lifecycle <- plyr::count(test.data[,"Engine"])[,2] + c(rul.test$V1)
test.lifecycle <- plyr::count(test.data[,"Engine"])[,2]
num.feature <- dim(train.data)[2]-2 #number of remaSining sensors

function.count.life.cycle <- function(train.lifecycle_, test.lifecycle_){
  #' only keep the train sample having longer lifetime than ith test engine
  #' @train.lifecycle_: train.lifecycle
  #' @test.lifecycle_: test.lifecycle
  #' 
  count_life_cycle_ <- c()
  for (i in test.lifecycle_){
    j <- 1
    temp <- which(train.lifecycle_ >= i)
    assign(paste0("index.", j), temp)  #index.j is for the jth test engine, and see which index of engines in train data meet the requirement
    count_life_cycle_ <- c(count_life_cycle_, length(temp))   #number of total datasets that meet the requirement
    j <- j+1
  }
  return(count_life_cycle_)
}

#remove test engine that having life cycles greater than all train engines
count_life_cycle <- function.count.life.cycle(train.lifecycle, test.lifecycle)  #how many training engines have life cycle greater than the life cycle of the test engine
index.negative <- which(count_life_cycle <= 0)

#### for the index.negative data, the predicted y hat (life cycle) is assigned by the cutted length (without adding the RUL part)
k <- 1
yhat.test.negative <- c()
y.test.negative <- c()
for (j in index.negative){
  temp <- plyr::count(test.data[,"Engine"])[,2]
  yhat.test.negative[k] <- temp[j]  #the predicted (cutted) length
  y.test.negative[k] <- test.lifecycle[k] #the real test life cycle (length)
  k <- k+1
}
rm(k)

## for the test engine, who only has less or equal than 3 (6) training engines meet the requirement, the mean of the life cycles of
## the training engines is assigned as y hat.
index.insufficient <- which(count_life_cycle > 0 & count_life_cycle <= 6)
# for the index.insufficient data, get y hat
yhat.test.insufficient <- c()
y.test.insufficient <- c()
k<- 1
for (j in index.insufficient){
  yhat.test.insufficient[k] <- mean(c(train.lifecycle[c(which(train.lifecycle >= test.lifecycle[j]))]))  #the predicted (cutted) length
  y.test.insufficient[k] <- test.lifecycle[j] #the real test life cycle (length)
  k <- k+1
}
rm(k)

####
##IT'S FINE IF THERE IS AN ERROR HERE
####

## remove the index.negative data (index) for the test data
if(length(index.negative) != 0) {test.lifecycle = c(test.lifecycle[-c(index.negative)])} #y here
test.data <- dplyr::filter(test.data, !(test.data[,1] %in% c(index.negative)))
## & index.insufficient
if(length(index.insufficient) != 0) {test.lifecycle = c(test.lifecycle[-c(index.insufficient)])}  #y here
test.data <- dplyr::filter(test.data, !(test.data[,1] %in% c(index.insufficient)))
print(paste0("There are ", length(index.negative), " engines with life cycle greater than all training engines."))
print(paste0("There are ", length(index.insufficient), " engines with the number of life cycle greater than that of training engines is less of equal than 6."))


#here we assign three users, with 1-10th for the first user, 11-40th for the second user, and 41-100th engine for the third user
train.data.user1 <- train.data[1:sum(train.lifecycle[1:10]),]
train.data.user2 <- train.data[sum(train.lifecycle[1:10])+1:sum(train.lifecycle[11:40]),]
train.data.user3 <- train.data[sum(train.lifecycle[1:40])+1:sum(train.lifecycle[41:100]),]
train.data.list <- list(train.data.user1, train.data.user2, train.data.user3)
train.lifecycle.user1 <- train.lifecycle[1:10]
train.lifecycle.user2 <- train.lifecycle[11:40]
train.lifecycle.user3 <- train.lifecycle[41:100]
train.lifecycle.list <- list(train.lifecycle.user1, train.lifecycle.user2, train.lifecycle.user3)



################################
################################
#    Normalized scaling #
################################
################################
# for training data
# each user
mean.train.user <- matrix(unlist(lapply(train.data.list, function(x){apply(x[, -c(1,2)], 2, mean)})), nrow = 3, byrow = TRUE)
mean2.train.user <- matrix(unlist(lapply(train.data.list, function(x){apply((x[, -c(1,2)])^2, 2, mean)})), nrow = 3, byrow = TRUE) #the mean of x square
# server
mean.train.server <- apply(mean.train.user, 2, mean)  #global mean for each feature
mean2.train.server<- apply(mean2.train.user, 2, mean)   #global square mean for each feature
meansq.train.server <- mean.train.server^2
variance.train.server <- mean2.train.server - meansq.train.server
sd.train.server <- sqrt(variance.train.server)

### scaling, this whole part needs to be done together!!!!
train.data.list.scale <- lapply(train.data.list, function(x){sweep(sweep(x[, -c(1,2)], 2, mean.train.server), 2, sd.train.server, FUN = '/')})
test.data.scale <- sweep(sweep(test.data[, -c(1,2)], 2, mean.train.server), 2, sd.train.server, FUN = '/')
train.data.list.scale[[1]] <- cbind(train.data.user1[,c(1,2)], train.data.list.scale[[1]])
train.data.list.scale[[2]] <- cbind(train.data.user2[,c(1,2)], train.data.list.scale[[2]])
train.data.list.scale[[3]] <- cbind(train.data.user3[,c(1,2)], train.data.list.scale[[3]])
test.data.scale <- cbind(test.data[,c(1,2)], test.data.scale)

################################
#next, for each test engine, extract engines for each user, and construct a time series
   train.data.list.scale_ = train.data.list.scale
   test.data.scale_ = test.data.scale
   test.lifecycle_ = test.lifecycle
   train.lifecycle.list_ = train.lifecycle.list
  #' for each test engine, extract corresponding training engines for each user, and construct a time series
  #' @train.data.scale.list_: input training data, splitted version (in a list, each df is a dataframe representing a user)
  #' @test.data.scale_: test dataset
  #' @test.lifecycle_: life cycles for testing dataset
  #' @train.lifecycle.list_: training data lifecycles, splitted version (into users)
  #' 
  user.num <- length(train.data.list.scale_) #number of users
  # for (i in user.num){assign(paste0("usertemp",i,"_"), train.data.list_[[i]])}
  #                     assign(paste0("train.lifecycle.user",i,"_"), train.lifecycle.list_[[i]])}
  test.engine.num <- length(test.lifecycle_)
  which.index.left_ <- list()
  avec <- c(1:test.engine.num)
  for (j in avec){
    i <- test.lifecycle_[j]
    temp.which.index.left_ <- lapply(train.lifecycle.list_, function(x){which(x >= i)})
    #for example; 1,4,7,... is for user1; 2,5,8,... is for user2; 3,6,9,... is for user 3 for this particular dataset
    which.index.left_ <- c(which.index.left_, temp.which.index.left_)
  }
  avec2 <- sequence(user.num*length(avec),1,1)
  # which engines in the training dataset meet the requirement for each user. index.left.useri_
  for (i in 1:user.num){assign(paste0("index.left.user",i,"_"), which.index.left_[(((avec2+3-i) %% 3) == 0)])} 
  # create ni*(cycle*features) matrix for each user, where ni is the number of life cycles of ith test engine
  data.list.scale_ = train.data.list.scale_; data.lifecycle_ = which.index.left_; list_tf_ = TRUE 
  test.lifecycle_ = test.lifecycle_ 
  user.num_ = user.num
  train.lifecycle.list_ = train.lifecycle.list

#which index (engine) is left
index.left <- setdiff(c(1:100), index.negative)
index.left <- setdiff(index.left, index.insufficient)

#this function is to create ni*(cycle*features) matrix for each user
#also, for each test engines.
# function.new.matrix <- function(data.list.scale_ = train.data.list.scale_, data.lifecycle_ = which.index.left_, list_tf_ = TRUE, 
#                                 test.lifecycle_ = test.lifecycle_, user.num_ = user.num, train.lifecycle.list_ = train.lifecycle.list){
  #' Create a new matrix to get a high dimensional data matrix
  #' @data.list.scale_: input data, this could be training data list, or test data matrix
  #' @data.lifecycle_: the lifecycle of each engine, could come from training or testing set
  #' @list_tf_: the data.list_ is a list if TRUE, otherwise it is a matrix
  #' @which.index.left_2: index of remaining engines that meets the requirement 
  #' @test.lifecycle_: life cycles for testing dataset
  #' @user.num_: number of users
  #'
  setwd("....")
  testengine <- length(data.lifecycle_)/user.num_
  user.sample_ <- unlist(lapply(data.list.scale_, function(x){length(unique(x[,1]))}))
  if (list_tf_ == TRUE){
    user.num <- length(data.list.scale_) #number of users
    for (i in 1:length(index.left)){ #testengine from 1:99 for tryout
      ni <- test.lifecycle_[i]  #life cycle of ith test engine
      list.1 <- list()
      list.2 <- list()
      list.3 <- list()
      matrix.1 <- c()
      matrix.2 <- c()
      matrix.3 <- c()
      #y
      response.list <- list()
      response.1 <- c()
      response.2 <- c()
      response.3 <- c()

      temp <- (i-1)*3
      for (j in 1:user.num_){ #user.num_ from 1:3 in this case
        temp <- temp + 1
        if (length(data.lifecycle_[[temp]]) == 0){
          print(paste0("No valid sample from user ", j, ", for test engine ", i))
        } else {
          for (k in data.lifecycle_[[temp]]){
            ######
            # if(dplyr::between(k, 1, 10) == TRUE){ user <- 1; p <- k
            #   } else if(dplyr::between(k, 11, 40) == TRUE){ user <- 2; p <- 10 + k
            #   } else if (dplyr::between(k, 41, 100) == TRUE){ user <- 3; p <- 40 + k
            #   }
            ######
            if(j ==1){user <- 1; p <- k
            } else if(j == 2){user <- 2; p <- 10 + k
            } else if (j == 3){user <- 3; p <- 40 + k
            }
            templist <- as.data.frame(data.list.scale_[[user]])
            tempmatrix <- subset(templist, templist[,1] == p)
            tempmatrix <- tempmatrix[c(1:test.lifecycle_[i]),]  #cutted
            tempts <- as.data.frame(as.vector(as.matrix(tempmatrix[,-c(1,2)])))
            ###y
            tempy <- as.data.frame(train.lifecycle.list_[[user]])$`train.lifecycle.list_[[user]]`
            temphere <- tempy[k]
            if(j == 1){list.1 <- c(list.1, tempts); matrix.1 <- rbind(matrix.1, as.vector(as.matrix(tempmatrix[,-c(1,2)]))); response.1 <- c(response.1, temphere)
            } else if (j == 2){list.2 <- c(list.2, tempts); matrix.2 <- rbind(matrix.2, as.vector(as.matrix(tempmatrix[,-c(1,2)]))); response.2 <- c(response.2, temphere)
            } else if (j == 3){list.3 <- c(list.3, tempts); matrix.3 <- rbind(matrix.3, as.vector(as.matrix(tempmatrix[,-c(1,2)]))); response.3 <- c(response.3, temphere)
            }
          }
        }
        #temp <- temp + 1
      }
      #assign(paste0("list_engine_",i),c(list.1,list.2,list.3))
      #capture.output(summary(list.1$Value), file = paste0("list_engine_",i,"_user_",j))
      assign(paste0("engine_",i,"_user_",1,"_matrix"), matrix.1)
      assign(paste0("engine_",i,"_user_",2,"_matrix"), matrix.2)
      assign(paste0("engine_",i,"_user_",3,"_matrix"), matrix.3)
      write.table(matrix.1, file = paste0("engine_",i,"_user_",1, ".txt"), col.names = F)
      write.table(matrix.2, file = paste0("engine_",i,"_user_",2, ".txt"), col.names = F)
      write.table(matrix.3, file = paste0("engine_",i,"_user_",3, ".txt"), col.names = F)
      #y
      assign(paste0("engine_",i,"_user_",1,"_response"), response.1)
      assign(paste0("engine_",i,"_user_",2,"_response"), response.2)
      assign(paste0("engine_",i,"_user_",3,"_response"), response.3)
      write.table(response.1, file = paste0("engine_",i,"_user_",1, ".response.txt"), col.names = F)
      write.table(response.2, file = paste0("engine_",i,"_user_",2, ".response.txt"), col.names = F)
      write.table(response.3, file = paste0("engine_",i,"_user_",3, ".response.txt"), col.names = F)
      # l <- l+1
    }
  }



## create new version matrix for test dataset
test.list <- list()
for (i in index.left){
  matrixtemp <- subset(test.data.scale, test.data.scale[,1] == i)
  tstemp <- as.data.frame(as.vector(as.matrix(matrixtemp[,-c(1,2)])))
  test.list <- c(test.list, tstemp)   #100 test engines, scaled version -- no, actually 99 left
}

