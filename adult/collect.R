rm(list = ls())
setwd("/N/u/liuruiq/BigRed3/DKNN/adult")
gamma.sim=(0:9)/10

result=c()
for (i in 1:296){
  filename=paste0('./data/seed',i,'.txt')
  
	if (file.exists(filename)) {
			tryCatch({
				df = read.table(filename,header=FALSE, sep=' ')
				result=rbind(result,df)


			}, error = function(e) {
			})
		}
}
colnames(result)=c("dhat1","dhat2","dhat3","dhat4","time1","time2","time3","time4","k1","k2","k3","k4","newy","gamma")
for (i in 1:4){
  result[,i]=(result[,i]>=0.5)
  result[,i]=result[,i]==result$newy
}

output=c()
for (gamma in gamma.sim){
  index=which(result$gamma==gamma)
  subdf=result[index,]
  print(paste0(gamma,':',length(index)))
  output=rbind(output,colMeans(subdf))
}