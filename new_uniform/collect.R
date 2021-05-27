rm(list = ls())
setwd("/home/ruiqliu/DKNN/new_uniform")
options(width = 200)
n.sim = c(10000, 30000, 50000)
gamma.sim = round(seq(0,0.9,0.1),1) 
n.sim = c(20000,40000,60000)
kappa.sim=c(0.55,0.6,0.65)
output = c()

   # print(c(N,gamma,kappa))
result = c() 
for (i in 1:500) {
  filename = paste("data/Seed",
                   i,
                   ".txt",
                   sep = "")
  
  if (file.exists(filename)) {
    tryCatch({
      
      df = read.table(filename, sep = " ")
      result = rbind(result, df)
    }, error = function(e) {
      
    })
  }
}
df=result[,-1]
#


#result=data.frame(dhat=dhat,dhat2=dhat2,qiaohat=qiaohat,nn1hat=nn1hat,k1=k1,k2=k2,qiaok1=qiaok1,time1=time1,time2=time2,qiaotime=time3,N=N,n1=n1,bayes=bayes,newy=newy, kappa=kappa, gamma=gamma)
colnames(df)=c("dhat","dhat2","qiaohat","nn1hat","k1","k2","qiaok1","time1","time2","qiaotime","N","n1","bayes", "newy","kappa", "gamma")
df$dhat=(df$dhat>=1/2)
df$dhat2=(df$dhat2>=1/2)
df$qiaohat=(df$qiaohat>=1/2)
df$nn1hat=(df$nn1hat>=1/2)
df$bayes=df$bayes>=1/2


df$dhat=(df$dhat==df$newy)
df$dhat2=(df$dhat2==df$newy)
df$qiaohat=(df$qiaohat==df$newy)
df$nn1hat=(df$nn1hat==df$newy)
df$bayes=(df$bayes==df$newy)
for (N in n.sim) {
  for (kappa in kappa.sim) {
    for (gamma in gamma.sim) {
      index=which(df$N==N & df$kappa==kappa & df$gamma==gamma)
      print(paste(N," ",kappa," ", gamma,":",length(index),sep=""))
      tempdf=df[index,]
      re=colMeans(tempdf)
      #re=c(n,p,re)
      output=rbind(output,re)
      
    }
  }
}

 



# for (N in n.sim){
#   for (kappa in kappa.sim){
#     for (gamma in gamma.sim) {
#       subdf=df[which(df$N==N & df$kappa==kappa & df$gamma==gamma),]
#       re=colMeans(subdf)
#       #re=c(n,p,re)
#       result=rbind(result,re)
#     }
#     
#   }
# }