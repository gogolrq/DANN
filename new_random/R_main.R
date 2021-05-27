rm(list = ls())
setwd("/home/ruiqliu/DKNN/new_random")

library(tictoc)
library(mvtnorm)
options(width=200)
args <- commandArgs(TRUE)
parameters = as.numeric(args)
Sys.sleep(0.1)
print(parameters)


N = parameters[1]
gamma = parameters[2]

kappa = parameters[3]  #pmax=0.6
pmax = kappa
a = 0.7
RandomSeed = parameters[4]


m=ceiling(N^gamma) #number of machines
d=3
fac=2

set.seed(1)
prob=runif(m,0,10)
prob=seq(1:m)
nums=rmultinom(1,N,prob)
nums=sort(nums,decreasing = T)
set.seed(RandomSeed)

f=function(z){
  y1=0.5*(1-z)/(1-a)
  y2=pmax
  #print(x^3)
  y=min(y1,y2)
  return(y)
}
f=Vectorize(f,"z")
x=seq(0,1,length.out = 100)

plot(x,f(x), ylim=c(0,1),type="l")
abline(h=0.5,col="red")

x=cbind(runif(N), runif(N), runif(N))
dx=sqrt(rowSums(x^2)/d)
px=f(dx)
y=sapply(px, rbinom,n=1,size=1)
print(mean(px==pmax))


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

r=0
k=0
newx=cbind(runif(1), runif(1), runif(1))
bayes=f(sqrt(sum(newx^2)/d))
newy=rbinom(1,1,bayes)





df.list = list()
index = 1:N


for (i in 1:m){
  if (length(index)>=nums[i]){
    temp = sample(index, nums[i])
    index = setdiff(index, temp)
    if (length(temp)>0){
      df.list[[length(df.list) + 1]] = temp
    }
  }
}




n1=length(df.list[[1]])
k=0
r=0

myfun=function(i,rd=1){
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
tic()
while(k<th & r^2<(d+2)*log(N)/fac){
  k=k+1
  ehat.sim=c()
  # for (i in 1:length(df.list)){
  #   index=df.list[[i]]
  #   px=x[index,]
  #   py=y[index]
  #   nj=length(index)
  #   kj=floor(k*nj/n1)
  #   pehat=nn(newx,kj,px,py)
  #   ehat.sim=rbind(ehat.sim,c(pehat,kj))
  # }
  temp=unlist(lapply(1:length(df.list), myfun,rd=0))
  ehat.sim=matrix(temp,ncol = 2, byrow = T)
  ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
  r=sqrt(sum(ehat.sim[,2]))*abs(ehat-1/2)
}

k1=k
dhat=ehat
time1=toc();
time1=time1$toc-time1$tic;
ehat.sim1=ehat.sim


n1=length(df.list[[1]])
th=max(1, n1*log(N)/N^(d/(d+2)))
k=0
r=0
tic()
while(k<th & r^2<(d+2)*log(N)/fac){
  k=k+1
  ehat.sim=c()
  # for (i in 1:length(df.list)){
  #   index=df.list[[i]]
  #   px=x[index,]
  #   py=y[index]
  #   nj=length(index)
  #   kj=floor(k*nj/n1)
  #   pehat=nn(newx,kj,px,py)
  #   ehat.sim=rbind(ehat.sim,c(pehat,kj))
  # }
  temp=unlist(lapply(1:length(df.list), myfun,rd=0))
  ehat.sim=matrix(temp,ncol = 2, byrow = T)
  ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
  r=sqrt(sum(ehat.sim[,2]))*abs(ehat-1/2)
}

k2=k
dhat2=ehat
time2=toc();
time2=time2$toc-time2$tic;
ehat.sim2=ehat.sim


tic()
k=n1^(2/(2+d))
k=k/(m^(d/(2+d)))
k=trunc(k)+1
temp=unlist(lapply(1:length(df.list), myfun, rd=0))
ehat.sim=matrix(temp,ncol = 2, byrow = T)
ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
qiaok1=k
qiaohat=ehat
time3=toc();
time3=time3$toc-time3$tic;


tic()
k=n1^(2/(2+d))
k=k/(m^(d/(2+d)))
k=1
temp=unlist(lapply(1:length(df.list), myfun, rd=0))
ehat.sim=matrix(temp,ncol = 2, byrow = T)
ehat=sum(ehat.sim[,1]*ehat.sim[,2])/sum(ehat.sim[,2])
nn1hat=ehat
time4=toc();
time4=time4$toc-time4$tic;



result=data.frame(dhat=dhat,dhat2=dhat2,qiaohat=qiaohat,nn1hat=nn1hat,k1=k1,k2=k2,qiaok1=qiaok1,time1=time1,time2=time2,qiaotime=time3,N=N,n1=n1,bayes=bayes,newy=newy, kappa=kappa, gamma=gamma)

print(result)
filename=paste("data/Seed", RandomSeed, ".txt", sep = "")

write.table(result, file = filename, append = TRUE, quote = TRUE, sep = " ", col.names=FALSE)