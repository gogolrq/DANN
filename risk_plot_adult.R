rm(list = ls())
setwd("E:/Dropbox/Revision/Distributed KNN/SC_Revision/code")
options(width = 200)
n.sim = c(60000)
gamma.sim = c(0,0.3)
gamma.sim = 0:8/10
#gamma.sim=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)
kappa.sim=c(0.8) 
name="random"
output = c()
result = c()
filename=paste("adult_result_",name,".csv",sep="")

data=read.csv(filename)
data=data[,-1]

#data.frame(myhat=myhat,qiaohat=qiaohat,myk1=myk1,qiaok1=qiaok1,mytime=time2,qiaotime=time3,N=N,n1=n1,bayes=bayes,newy=newy, kappa=kappa, gamma=gamma)
#result=data.frame(dhat=dhat, myhat=myhat,qiaohat=qiaohat,nn1hat=nn1hat,dk1=dk1,myk1=myk1,qiaok1=qiaok1,dtime=time1, mytime=time2,qiaotime=time3,nn1time=time4,N=N,n1=n1,bayes=bayes,newy=newy, kappa=kappa, gamma=gamma)



#colnames(result)=c("DA","DAES", "DK","D1","dk1","myk1", "qiaok1","dtime", "mytime", "qiaotime","nn1time", "N", "n1", "bayes", "newy", "kappa", "gamma")

df=data




library("tidyverse")
library("latex2exp")
df=select(data,dhat1, dhat2, dhat3, dhat4,gamma)
df=df[1:9,]
colnames(df)=c("DA","DAES","DK","D1","gamma")
df=gather(df,key = "Classifier", value = "Risk", -gamma)
head(df)
df$Classifier=factor(df$Classifier, levels=c("DAES","DA","DK","D1"))


titlename=""
if (grepl("uniform",name)){
  titlename="Equally Split"
}

if (grepl("random",name)){
  titlename="Unequally Split"
}

ps.options(horizontal = F)
ps.options(height=3.5, width=4.5)
postscript(paste("../figure/adult_risk_",name,".eps",sep=""))
par(mar=c(2,4,2,2))
p1=ggplot(data = df, mapping = aes(x = gamma, y = Risk, group = Classifier, color=Classifier, pch=Classifier)) +
  geom_line()+
  geom_point(size = 3)+
  xlab(TeX(r'($\epsilon$)'))+
  ylab("")+
  theme(text=element_text(size=15))+
  theme(axis.title.x=element_text(size=20))+
  theme(plot.title = element_text(size=13))+
  ggtitle(titlename)+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title=element_blank())
  #scale_x_continuous(labels = paste0(xlab, "k"),
  #                   breaks = 10^3 * xlab)
print(p1)
dev.off()

print(p1)