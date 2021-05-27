rm(list = ls())
library(tictoc)
setwd("/N/u/liuruiq/BigRed3/DKNN/adult")
args <- commandArgs(TRUE)
parameters = as.numeric(args)
gamma = parameters[1]
seed = parameters[2]
set.seed(1)

df=read.csv("train.csv")
test_df=read.csv(paste0("./test_data/test_",seed,".csv"))


N = dim(df)[1]
d=dim(df)[2]-1 

fac=2
m=ceiling(N^gamma)
n=ceiling(N/m)
df.list = list()
index = 1:N

for (i in 1:m){
  if (length(index)>=n){
    temp = sample(index, 1 * n)
    index = setdiff(index, temp)
    df.list[[length(df.list) + 1]] = temp
  }else{
    df.list[[length(df.list) + 1]] = index
    index = setdiff(index, index)
  }
}

nn=function(x,k=1,dfx,dfy){
  s=0
  if (length(dfy)==1){
    s=dfy
    return(s)
  }
  if (k==0){
    s=0.5
  }else{
    num=length(dfy)
    d=dim(dfx)[2]
    temp=dfx-matrix(rep(x,num),ncol=d,byrow=T)
    
    temp=rowSums(temp^2)
    index=order(temp)
    s=mean(dfy[index[1:k]])
  }
  return(s)
}

x=as.matrix(df[,1:d])
y=as.numeric(df[,d+1])
result=c()
test_num=dim(test_df)[1]
for (ii in 1:test_num){
  newx=as.numeric(test_df[ii,1:d])
  newy=test_df[ii,d+1]
  
  n1=length(df.list[[1]])
  
  
  myfun=function(i,rd=0){
    index=df.list[[i]]
    px=x[index,]
    py=y[index]
    nj=length(index)
    if (rd==1){
      kj=floor(k*nj/n1)
      
    }else{
      kj=ceiling(k*nj/n1)
    }
    
    pehat=nn(newx,kj,px,py)
    return(c(pehat,kj))
  }
  myfun=Vectorize(myfun,"i")
  
  th=n1
  k=0
  r=0
  tic()
  while(k<th & r^2<(d+2)*log(N)/fac){
    k=k+1
    ehat.sim=c()
    temp=unlist(lapply(1:length(df.list), myfun,rd=0))
    ehat.sim=matrix(temp,ncol = 2, byrow = T)
    ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
    r=sqrt(sum(ehat.sim[,2]))*abs(ehat-1/2)
  }
  
  k1=k
  dhat1=ehat
  time1=toc()
  time1=time1$toc-time1$tic
  
  
  th=max(1, n1*log(N)/N^(d/(d+2)))
  k=0
  r=0
  tic()
  while(k<th & r^2<(d+2)*log(N)/fac){
    k=k+1
    ehat.sim=c()
    temp=unlist(lapply(1:length(df.list), myfun,rd=0))
    ehat.sim=matrix(temp,ncol = 2, byrow = T)
    ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
    r=sqrt(sum(ehat.sim[,2]))*abs(ehat-1/2)
  }
  
  k2=k
  dhat2=ehat
  time2=toc()
  time2=time2$toc-time2$tic
  
  tic()
  k=ceiling(n1/N^(d/(d+2)))
  temp=unlist(lapply(1:length(df.list), myfun,rd=0))
  ehat.sim=matrix(temp,ncol = 2, byrow = T)
  ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
  k3=k
  dhat3=ehat
  time3=toc()
  time3=time3$toc-time3$tic
  
  tic()
  k=1
  temp=unlist(lapply(1:length(df.list), myfun,rd=0))
  ehat.sim=matrix(temp,ncol = 2, byrow = T)
  ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
  k4=k
  dhat4=ehat
  time4=toc()
  time4=time4$toc-time4$tic
  
  df=data.frame(dhat1=dhat1,dhat2=dhat2,dhat3=dhat3,dhat4=dhat4,time1=time1,time2=time2,time3=time3,time4=time4,k1=k1,k2=k2,k3=k3,k4=k4, newy=newy, gamma=gamma)
  result=rbind(result,df)
}


write.table(result,paste0('./data/seed',seed,'.txt'),append = TRUE,col.names = FALSE,row.names = FALSE)
