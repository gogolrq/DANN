rm(list = ls())
setwd("/home/ruiqliu/DKNN/puniform")
options(width = 200)
n.sim = c(10000, 20000)
#n.sim=c(10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000)
#n.sim=c(60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000)
gamma.sim = seq(0,0.3)
#kappa.sim = seq(0.1,0.5,0.1)
kappa.sim=c(0.65) 
#kappa.sim=c(0.75,0.95)
#kappa.sim=c(0.501, 0.502, 0.503, 0.504, 0.52, 0.54, 0.56, 0.58, 0.6)
#kappa.sim=c(0.55,0.6)
output = c()

for (N in n.sim) {
  for (gamma in gamma.sim) {
    for (kappa in kappa.sim) {
   # print(c(N,gamma,kappa))
      result = c() 
      for (i in 1:201) {
        filename = paste("data/N",
                         N,
                         "_gama",
                         gamma,
                         "_kap",
                         kappa,
                         "_Seed",
                         i,
                         ".txt",
                         sep = "")
        
        if (file.exists(filename)) {
          tryCatch({
            is = c(is, i)
            
            df = read.table(filename, sep = " ")
            df$kappa=kappa
            df$gamma=gamma
            result = rbind(result, df)
          }, error = function(e) {
            
          })
        }
      }
      df=result
      
      df$dhat=(df$dhat>=1/2)
      df$dhat2=(df$dhat2>=1/2)
      df$qiaohat=(df$qiaohat>=1/2)
      
      
      df$dregret=df$dhat*df$bayes+(1-df$dhat)*(1-df$bayes)
      df$dregret2=df$dhat2*df$bayes+(1-df$dhat2)*(1-df$bayes)
      df$qiaoregret=df$qiaohat*df$bayes+(1-df$qiaohat)*(1-df$bayes)
      df$bayesregret=(df$bayes>=1/2)*df$bayes+(df$bayes<1/2)*(1-df$bayes)
      
      df$dregret=df$bayesregret-df$dregret
      df$dregret2=df$bayesregret-df$dregret2
      df$qiaoregret=df$bayesregret-df$qiaoregret
      df$bayes=df$bayes>=1/2
      
      df$dhat=(df$dhat==df$bayes)
      df$dhat2=(df$dhat2==df$bayes)
      df$qiaohat=(df$qiaohat==df$bayes)
      re=colMeans(df)
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