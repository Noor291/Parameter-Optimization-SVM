library(dplyr)
library(caret)
library(kernlab)

data <- read.csv("C:/Users/noord/Desktop/Parameter_optimization_SVM/letter-recognition.data")
colnames(data)<-c("letter","x-box","y-box","width","high","onpix","x-bar","y-bar","x2bar","y2bar","xybar","x2ybr","xy2br","x-ege","xegvy","y-ege","yegvx")
data$letter <- factor(data$letter)
x<-list()

for(i in 1:10){
  set.seed(i+100)  
  y<- sample_n(data,size=500,replace=FALSE)
  x[[i]]<-data.frame(y) 
}

iteration=1000
kernelList=c("vanilladot", "polydot", "tanhdot")
finalacc=0
cols<-c('Sample','Best Accuracy','Kernel','Nu','Epsilon')
df<-data.frame(matrix(nrow=0,ncol=length(cols)))
colnames(df)=cols

for(i in 1:10){
  current<-data.frame(x[i])
  # print(current)
  trainIndex <- createDataPartition(current$letter,times=1, p = 0.7, list = FALSE)
  trainDataset <- current[trainIndex, ]
  testDataset <- current[-trainIndex, ]
  
  
  bestAccuracy=0
  bestKernel=""
  bestNu=0
  bestEpsilon=0
  acc<-c()
  iter<-c()
  
  for(j in 1:iteration){
    k=sample(kernelList,1)
    n=runif(1)
    e=runif(1)
    
    X_train=trainDataset[,2:ncol(trainDataset)]
    Y_train=trainDataset[,1]
    X_test=testDataset[,2:ncol(testDataset)]
    Y_test=testDataset[,1]
    
    model<-ksvm(data.matrix(X_train),Y_train,kernel=k,nu=n,epsilon=e,kpar=list())
    
    
    predicted<-predict(model,X_test)
    accuracy<-round(mean(Y_test==predicted)*100,2)
    # print(accuracy)
    if(j%%50==0){
      acc<-append(acc,accuracy)
      iter<-append(iter,j)
    }
    
    
    
    if(accuracy>bestAccuracy){
      bestKernel=k
      bestNu=n
      bestEpsilon=e
      bestAccuracy=accuracy
    }
    
  }
  if(bestAccuracy>finalacc){
    finalacc=bestAccuracy
    xlim <- c(0, 1000)
    ylim <- c(0, 100)
    spacing1<-10
    spacing2<-100
    graph<-plot(NULL, xlim =xlim, ylim =ylim, xlab = "iterations", ylab = "Accuracy", type = "n")
    
    axis(1, at = seq(xlim[1], xlim[2], by = spacing2))
    axis(2, at = seq(ylim[1], ylim[2], by = spacing1))
    title(main = "Fitness(BestAcc)")
    lines(iter,acc)
    abline(h = seq(0,100,10), lty = "dashed", col = "gray30")
    
  }
  
  df[nrow(df)+1,]<-c(i,bestAccuracy,bestKernel,round(bestNu,2),round(bestEpsilon,2))
  
  
}  

print(df)

