rm(list=ls())
library(tidyr)

set.seed(4)
setwd("/N/u/liuruiq/BigRed3/DKNN/adult")



df=read.csv("adult.csv")
data=cbind(age=df$age,fnlwgt=df$fnlwgt,edu=df$education.num,gain=df$capital.gain,loss=df$capital.loss,hour=df$hours.per.week)
y=factor(df$income)
levels(y)=c(0,1)
y=as.numeric(y)-1
data=data.frame(data,y=y)
data=drop_na(data)
for (i in 1:dim(data)[2]){
  rg=range(data[,i])
  data[,i]=(data[,i]-rg[1])/(rg[2]-rg[1])
}
N=dim(data)[1]
index_train=sample(1:N,round(0.8*N),replace = FALSE)
index_test=setdiff(1:N,index_train)

train_df=data[index_train,]
test_df=data[index_test,]

write.csv(train_df,"train.csv",row.names = FALSE)
write.csv(test_df,"test.csv",row.names = FALSE)

for (i in 1:296){
  index=((i-1)*22+1):(i*22)
  tempdf=test_df[index,]
  write.csv(tempdf,paste0('./test_data/test_',i,'.csv'),row.names = FALSE)
}