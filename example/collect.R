rm(list = ls())
setwd("/home/ruiqliu/DKNN/revision/example")
options(width = 200)
n.sim = c(10000)
gamma.sim = c(0.5,0.6)
kappa.sim=c(0.8) 

output = c()
result = c()
for (N in n.sim) {
	for (gamma in gamma.sim) {
	  filename=paste("data/example_N", N, "_gamma", gamma*10, ".txt", sep = "")
	  if (file.exists(filename)) {
		  tryCatch({
			df = read.table(filename, sep = " ")
			result = rbind(result, df)
		  }, error = function(e) {
			
		  })
	  }
    }
}  

data=result

#data.frame(myhat=myhat,qiaohat=qiaohat,myk1=myk1,qiaok1=qiaok1,mytime=time2,qiaotime=time3,N=N,n1=n1,bayes=bayes,newy=newy, kappa=kappa, gamma=gamma)
colnames(result)=c("myhat", "qiaohat", "myk1", "qiaok1", "mytime", "qiaotime", "N", "n1", "bayes", "newy", "kappa", "gamma")
result$myhat=(result$myhat>=1/2)
result$qiaohat=(result$qiaohat>=1/2)
result$bayes=(result$bayes>=1/2)

result$myhat=(result$myhat==result$bayes)
result$qiaohat=(result$qiaohat==result$bayes)

df=result
output=c()
for (N in n.sim){
  for (kappa in kappa.sim){
    for (gamma in gamma.sim) {
			subdf=df[which(df$N==N  & df$gamma==gamma & df$kappa==kappa),]
			re=colMeans(subdf)
			#re=c(n,p,re)
			output=rbind(output,re)
    }
    
  }
}

print(output)

  # for (gamma in gamma.sim) {
    # for (kappa in kappa.sim) {
   # # print(c(N,gamma,kappa))
      # result = c() 
      # for (i in 1:201) {
		# filename=paste("data/example_N", N, ".txt", sep = "")
        

      # }
      # #df=result
      
      # #df$dhat=(df$dhat>=1/2)
      # #df$dhat2=(df$dhat2>=1/2)
      # #df$qiaohat=(df$qiaohat>=1/2)
      
      
      # #df$dregret=df$dhat*df$bayes+(1-df$dhat)*(1-df$bayes)
      # #df$dregret2=df$dhat2*df$bayes+(1-df$dhat2)*(1-df$bayes)
      # #df$qiaoregret=df$qiaohat*df$bayes+(1-df$qiaohat)*(1-df$bayes)
      # #df$bayesregret=(df$bayes>=1/2)*df$bayes+(df$bayes<1/2)*(1-df$bayes)
      
      # #df$dregret=df$bayesregret-df$dregret
      # #df$dregret2=df$bayesregret-df$dregret2
      # #df$qiaoregret=df$bayesregret-df$qiaoregret
      # #df$bayes=df$bayes>=1/2
      
      # #df$dhat=(df$dhat==df$bayes)
      # #df$dhat2=(df$dhat2==df$bayes)
      # #df$qiaohat=(df$qiaohat==df$bayes)
      # #re=colMeans(df)
      # #re=c(n,p,re)
      # output=rbind(output,re)
    # }
    
  # }
# }
 



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