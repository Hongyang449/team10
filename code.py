


x=read.csv("CNPC.csv")
dim(x) #[1] 325199     26

01/14-09/2015
238 courses


predict MOOC completion
user cluster
recommendation


number of courses as extra features
registered all 1
final_cc_cname_DI all *
gender

viewed ​: H-M, administrative, 0/1, =1 if the number of interactions？
explored​: H-M, administrative, 0/1, =1 if the user interacted with or viewed >=50% of the course
modules (i.e. chapters).?
completed_%​: administrative, 0.0 - 1.0, percent of total required content modules completed if
number of content modules with requirements is greater than an internal threshold (else blank).?


colnames(x)
 [1] "course_id_DI"        "discipline"          "userid_DI"          
 [4] "registered"          "viewed"              "explored"           
 [7] "grade"               "grade_reqs"          "completed_."        
[10] "course_reqs"         "final_cc_cname_DI"   "primary_reason"     
[13] "learner_type"        "expected_hours_week" "LoE_DI"             
[16] "age_DI"              "gender"              "start_time_DI"      
[19] "course_start"        "course_end"          "last_event_DI"      
[22] "nevents"             "ndays_act"           "ncontent"           
[25] "nforum_posts"        "course_length"   



onehot <- function(vec){
	ids=names(table(vec))
	tmp=NULL
	for (i in ids){
		tmp=c(tmp,as.numeric(vec==i))
	}
	output=matrix(tmp,ncol=length(ids),byrow=F)
	colnames(output)=ids
	return (output)
}

x=read.csv("CNPC.csv",stringsAsFactors=F,check.names=F)
dim(x) #[1] 325199     26

#x1=x[,c(9,3:7,12:16,18,21:25)]

x1=x[,c(9,3:7,22:25)]
x1=cbind(x1,onehot(x[,12]))
x1=cbind(x1,onehot(x[,13]))
x1=cbind(x1,onehot(x[,14]))
x1=cbind(x1,onehot(x[,15]))
x1=cbind(x1,onehot(x[,16]))
x1=cbind(x1,onehot(x[,18]))
x1=cbind(x1,onehot(x[,21]))

x1=x1[,which(colnames(x1)!="{}" & colnames(x1)!="V1")]
write.csv(x1, file = "dat_person.csv", row.names = FALSE)


#x2=x[,c(9,1,8,10,26,2,19:20)]

x2=x[,c(9,1,8,10,26)]
x2=cbind(x2,onehot(x[,2]))
x2=cbind(x2,onehot(x[,19]))
x2=cbind(x2,onehot(x[,20]))

x2=x2[,1:33] # exclude repeated names
write.csv(x2, file = "dat_course.csv", row.names = FALSE)

x3=cbind(x1,x2[,-1])
write.csv(x3, file = "dat_person_course.csv", row.names = FALSE)

y=x3[,-c(2,54)]
y=y[!is.na(y[,1]),]
for(i in 1:ncol(y)){
	ind=is.na(y[,i])
	y[ind,i]=mean(y[!ind,i])
}
write.table(y, file="dat_full.txt",sep="\t",col.names=F,row.names=F)

set.seed(449)
ind=sample(nrow(y),nrow(y)*0.2)

write.table(y[-ind,], file="train.txt",sep="\t",col.names=F,row.names=F)
write.table(y[ind,], file="test.txt",sep="\t",col.names=F,row.names=F)



## frequency

y=x3
y=y[!is.na(y[,1]),]

tmp=table(y[,2]) # max 23
y[,2]=tmp[as.character(y[,2])]

tmp=table(y[,54])
y[,54]=tmp[as.character(y[,54])]

for(i in 1:ncol(y)){
	ind=is.na(y[,i])
	y[ind,i]=mean(y[!ind,i])
}
write.table(y, file="dat1.txt",sep="\t",col.names=F,row.names=F)


set.seed(449)
ind=sample(nrow(y),nrow(y)*0.2)

write.table(y[-ind,], file="train1.txt",sep="\t",col.names=F,row.names=F)
write.table(y[ind,], file="test1.txt",sep="\t",col.names=F,row.names=F)



## recommendation
ind3=which(y1[,2]==2)

write.table(y1[-ind3,], file="train3.txt",sep="\t",col.names=F,row.names=F)
write.table(y1[ind3,], file="test3.txt",sep="\t",col.names=F,row.names=F)


pred=scan("output_rf3.txt")
pred=matrix(pred,ncol=10,byrow=T)


y=x3
y=y[!is.na(y[,1]),]
y=y[ind3,]


ids=unique(y[,2])

sort_y=NULL
sort_pred=NULL
for (i in 1:length(ids)){
	tmp=y[,2]==ids[i]
	sort_y=rbind(sort_y, y[tmp,c(1,2,58:67)])
	sort_pred=rbind(sort_pred, pred[tmp,])
}

diff_y=rep(NA,2934)
diff_pred=rep(NA,2934)
for (i in seq(1,2934,2)){
	if(any(sort_y[i,3:12]-sort_y[i+1,3:12])){
		tmp=sort_pred[i,]+sort_pred[i+1,]
		diff_y[i]=sort_y[i,1]-sort_y[i+1,1]
		diff_pred[i]=tmp[sort_y[i,3:12]==1] - tmp[sort_y[i+1,3:12]==1]
	}
}

diff_pred[ind]







## baselearner - plot

library(ggplot2)
library(reshape2)

source("~/function/my_palette.r")
source("~/function/multiplot.R")

gold=scan("gold.txt")
x0=scan("output_svm.txt")
x1=scan("output_ridge.txt")
x2=scan("output_linear.txt")
x3=scan("output_rf.txt")

num=1000
set.seed(449)
cor0=cor1=cor2=cor3=NULL
e0=e1=e2=e3=NULL
for (i in 1:num){
	ind=sample(length(gold),3000)
	cor0=c(cor0,cor(gold[ind],x0[ind]))
	cor1=c(cor1,cor(gold[ind],x1[ind]))
	cor2=c(cor2,cor(gold[ind],x2[ind]))
	cor3=c(cor3,cor(gold[ind],x3[ind]))
	e0=c(e0,(sum((gold[ind]-x0[ind])^2)/3000)^0.5)
	e1=c(e1,(sum((gold[ind]-x1[ind])^2)/3000)^0.5)
	e2=c(e1,(sum((gold[ind]-x2[ind])^2)/3000)^0.5)
	e3=c(e3,(sum((gold[ind]-x3[ind])^2)/3000)^0.5)
}

wilcox.test(cor0,cor1) # p-value < 2.2e-16
wilcox.test(cor1,cor2) # p-value = 0.2579
wilcox.test(cor2,cor3) # p-value < 2.2e-16

wilcox.test(e0,e1) # p-value < 2.2e-16
wilcox.test(e1,e2) # p-value = 0.9946
wilcox.test(e2,e3) # p-value < 2.2e-16


tbl=cbind(cor0,cor1,cor2,cor3)
colnames(tbl)=c("SVM","Ridge","Linear","Random Forest")
tbl.m=melt(tbl)
colnames(tbl.m)=c("sample","model","correlation")
tmp_col=tmp_col=c(p_jco[c(10,2)],p_d3[2],p_jco[4])
names(tmp_col)=colnames(tbl)

p1 = ggplot(tbl.m, aes(model, correlation, fill=model)) + 
    geom_violin(width=1,aes(fill=model),colour="white",alpha=1,linetype=1,lwd=0.5)+
    scale_fill_manual(values=tmp_col) +
    geom_boxplot(width=0.1)+
    stat_summary(fun.y=mean, geom="point", aes(shape="Mean"), size=1.5,color="red")+
    scale_shape_manual("", values=rep(10,13))+
    labs(x='Model', y="Pearson's Correlation",title="Correlation comparison") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
    ylim(0.74,0.96) +
    theme(axis.line.x = element_line(color="black", size = 0.5))+
    theme(axis.line.y = element_line(color="black", size = 0.5))+
    theme(axis.title.x = element_text(colour="black", size=12))+
    theme(axis.title.y = element_text(colour="black", size=12))+
    theme(axis.text.x = element_text(colour="black",angle = 45, hjust = 1,size=12))+
    theme(axis.text.y = element_text(colour="black",size=12)) +
    annotate("text", x = 1:4, y = 0.9, label=format(c(mean(cor0),mean(cor1),mean(cor2),mean(cor3)),digits=4),size=4)+
    annotate("text", x=c(1.5,2.5,3.5), y=0.88, label=c("p<2.2e-16","p=0.2579","p<2.2e-16"),size=4)
p1


tbl=cbind(e0,e1,e2,e3)
colnames(tbl)=c("SVM","Ridge","Linear","Random Forest")
tbl.m=melt(tbl)
colnames(tbl.m)=c("sample","model","RMSE")
tmp_col=tmp_col=c(p_jco[c(10,2)],p_d3[2],p_jco[4])
names(tmp_col)=colnames(tbl)

p2 = ggplot(tbl.m, aes(model, RMSE, fill=model)) + 
    geom_violin(width=1,aes(fill=model),colour="white",alpha=1,linetype=1,lwd=0.5)+
    scale_fill_manual(values=tmp_col) +
    geom_boxplot(width=0.1)+
    stat_summary(fun.y=mean, geom="point", aes(shape="Mean"), size=1.5,color="red")+
    scale_shape_manual("", values=rep(10,13))+
    labs(x='Model', y="RMSE",title="RMSE comparison") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
    ylim(0.07,0.2) +
    theme(axis.line.x = element_line(color="black", size = 0.5))+
    theme(axis.line.y = element_line(color="black", size = 0.5))+
    theme(axis.title.x = element_text(colour="black", size=12))+
    theme(axis.title.y = element_text(colour="black", size=12))+
    theme(axis.text.x = element_text(colour="black",angle = 45, hjust = 1,size=12))+
    theme(axis.text.y = element_text(colour="black",size=12)) +
    annotate("text", x = 1:4, y = 0.2, label=format(c(mean(e0),mean(e1),mean(e2),mean(e3)),digits=4),size=4)+
    annotate("text", x=c(1.5,2.5,3.5), y=0.17, label=c("p<2.2e-16","p=0.9946","p<2.2e-16"),size=4)
p2

list_p=list()
list_p[[1]]=p1
list_p[[2]]=p2

png(filename="baselearner.png",width=10,height=8,units='in',res=300) # unit inch
mat_layout=matrix(1:2,nrow=2,byrow=T)
multiplot(plotlist=list_p,layout = mat_layout)
dev.off()

## plot - scatter

gold=scan("gold.txt")
x3=scan("output_rf.txt")
dat=data.frame(gold_standard=gold,prediction=x3)
p1=ggplot(dat, aes(gold_standard, prediction)) + 
    geom_point(size=2,colour=p_jco[6],pch=19,alpha=0.1) + 
    geom_smooth(method="lm", level=0.95, size=0.5, colour='black', fill=paste0(p_jco[2],10)) +
    theme_light() +
    ylim(0,1) +
    xlim(0,1) +
    theme(axis.line.x = element_line(color="black", size = 0.5))+
    theme(axis.line.y = element_line(color="black", size = 0.5))+
    theme(axis.title.x = element_text(colour="black", size=12))+
    theme(axis.title.y = element_text(colour="black", size=12))+
    theme(axis.text.x = element_text(colour="black",size=12))+
    theme(axis.text.y = element_text(colour="black",size=12))+
    labs(x="Gold standard", y="Prediction") +
    annotate("text", x = 0.25, y = 0.9, label=paste0("Pearson's correlation = ",format(cor(gold,x3),digits=3), "\np-value < 2.2e-16"),size=5)
p1

png(filename="performance.png",width=10,height=4,units='in',res=150) # unit inch
p1
dev.off()


## plot - add feature & sample

library(ggplot2)
library(reshape2)

source("~/function/my_palette.r")
source("~/function/multiplot.R")

gold=scan("gold.txt")
x0=scan("output_rf.txt")
x1=scan("output_rf1.txt")
x2=scan("output_rf2.txt")
x3=(x0+x1+x2)/3

num=1000
set.seed(449)
cor0=cor1=cor3=NULL
e0=e1=e3=NULL
for (i in 1:num){
	ind=sample(length(gold),3000)
	cor0=c(cor0,cor(gold[ind],x0[ind]))
	cor1=c(cor1,cor(gold[ind],x1[ind]))
	cor3=c(cor3,cor(gold[ind],x3[ind]))
	e0=c(e0,(sum((gold[ind]-x0[ind])^2)/3000)^0.5)
	e1=c(e1,(sum((gold[ind]-x1[ind])^2)/3000)^0.5)
	e3=c(e3,(sum((gold[ind]-x3[ind])^2)/3000)^0.5)
}

wilcox.test(cor0,cor1) # p-value = 1.344e-06
wilcox.test(cor1,cor3) # p-value = 3.576e-12

wilcox.test(e0,e1) # p-value = 9.101e-07
wilcox.test(e1,e3) # p-value = 5.937e-12


tbl=cbind(cor0,cor1,cor3)
colnames(tbl)=c("RF","RF_feature","RF_feature_sample")
tbl.m=melt(tbl)
colnames(tbl.m)=c("sample","model","correlation")
tmp_col=tmp_col=c(p_jco[c(10,2)],p_d3[2])
names(tmp_col)=colnames(tbl)

p1 = ggplot(tbl.m, aes(model, correlation, fill=model)) + 
    geom_violin(width=0.5,aes(fill=model),colour="white",alpha=1,linetype=1,lwd=0.5)+
    scale_fill_manual(values=tmp_col) +
    geom_boxplot(width=0.1)+
    stat_summary(fun.y=mean, geom="point", aes(shape="Mean"), size=1.5,color="red")+
    scale_shape_manual("", values=rep(10,13))+
    labs(x='Model', y="Pearson's Correlation",title="Correlation comparison") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
    ylim(0.945,0.965) +
    theme(axis.line.x = element_line(color="black", size = 0.5))+
    theme(axis.line.y = element_line(color="black", size = 0.5))+
    theme(axis.title.x = element_text(colour="black", size=12))+
    theme(axis.title.y = element_text(colour="black", size=12))+
    theme(axis.text.x = element_text(colour="black",angle = 45, hjust = 1,size=12))+
    theme(axis.text.y = element_text(colour="black",size=12)) +
    annotate("text", x = 1:3, y = 0.962, label=format(c(mean(cor0),mean(cor1),mean(cor3)),digits=4),size=4)+
    annotate("text", x=c(1.5,2.5), y=0.96, label=c("p=1.344e-06","p=3.576e-12"),size=4)
p1


tbl=cbind(e0,e1,e3)
colnames(tbl)=c("RF","RF_feature","RF_feature_sample")
tbl.m=melt(tbl)
colnames(tbl.m)=c("sample","model","RMSE")
tmp_col=tmp_col=c(p_jco[c(10,2)],p_d3[2])
names(tmp_col)=colnames(tbl)

p2 = ggplot(tbl.m, aes(model, RMSE, fill=model)) + 
    geom_violin(width=0.5,aes(fill=model),colour="white",alpha=1,linetype=1,lwd=0.5)+
    scale_fill_manual(values=tmp_col) +
    geom_boxplot(width=0.1)+
    stat_summary(fun.y=mean, geom="point", aes(shape="Mean"), size=1.5,color="red")+
    scale_shape_manual("", values=rep(10,13))+
    labs(x='Model', y="RMSE",title="RMSE comparison") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),legend.position="none") +
    ylim(0.07,0.1) +
    theme(axis.line.x = element_line(color="black", size = 0.5))+
    theme(axis.line.y = element_line(color="black", size = 0.5))+
    theme(axis.title.x = element_text(colour="black", size=12))+
    theme(axis.title.y = element_text(colour="black", size=12))+
    theme(axis.text.x = element_text(colour="black",angle = 45, hjust = 1,size=12))+
    theme(axis.text.y = element_text(colour="black",size=12)) +
    annotate("text", x = 1:3, y = 0.095, label=format(c(mean(e0),mean(e1),mean(e3)),digits=4),size=4)+
    annotate("text", x=c(1.5,2.5), y=0.09, label=c("p=9.101e-07","p=5.937e-12"),size=4)
p2

list_p=list()
list_p[[1]]=p1
list_p[[2]]=p2

png(filename="rf_feature_sample.png",width=10,height=8,units='in',res=300) # unit inch
mat_layout=matrix(1:2,nrow=2,byrow=T)
multiplot(plotlist=list_p,layout = mat_layout)
dev.off()



# plot - fi & correlation

cor_feature=cor(y,use="pairwise.complete.obs")
cor_feature[,1]


fi=scan("fi.txt")
tmp=readLines("fi_names.txt")
names(fi)=tmp[-1]


png(filename="fi_top5.png",width=6,height=4,units='in',res=300) # unit inch
tmp=sort(fi,decreasing=T)[1:5]
mypalette = colorRampPalette(p_jco[c(5,6)]) # rainbow(21,end=4/6)
barplot(tmp,col=mypalette(5),names.arg=names(tmp), ylim=c(0,0.36),xlab="features", ylab="feature importance")
dev.off()

fi1=c(sum(fi[c(1:8,46:51)]), sum(fi[9:19]), sum(fi[20:33]), 
	sum(fi[34:42]), sum(fi[43:45]), sum(fi[55:64]), sum(fi[c(52:54,65:82)]))
names(fi1)=c("player_load","primary_reason","learner_type",
	"education","age","discipline","course_reqs")

png(filename="fi_cate.png",width=10,height=4,units='in',res=300) # unit inch
tmp=sort(fi1,decreasing=T)[1:7]
mypalette = colorRampPalette(p_jco[c(4,2)]) # rainbow(21,end=4/6)
barplot(tmp,col=mypalette(7),names.arg=names(tmp), ylim=c(0,0.88),
	xlab="feature category", ylab="feature importance")
dev.off()


#pie(fi1,labels=names(fi1),col=p_jco[1:7], clockwise=T, init.angle=0, main="distribution of feature importance")





y1=y

## extra sample

y=x3
y=y[is.na(y[,1]),]
y=y[!is.na(y[,"ncontent"]),]

#set.seed(449)
#ind=sample(nrow(y),30000)
#y=y[ind,]

tmp=table(y[,2]) # max 23
y[,2]=tmp[as.character(y[,2])]

tmp=table(y[,54])
y[,54]=tmp[as.character(y[,54])]

for(i in 1:ncol(y)){
	ind=is.na(y[,i])
	y[ind,i]=mean(y[!ind,i])
}

y2=y

y[,"nevents"]=(y[,"nevents"] - min(y[,"nevents"])) / (max(y[,"nevents"]) - min(y[,"nevents"]))
y[,"ncontent"]=(y[,"ncontent"] - min(y[,"ncontent"])) / (max(y[,"ncontent"]) - min(y[,"ncontent"]))

y2[,1]=0.2*y[,"nevents"] + 0.3*y[,"ncontent"] + 0.25*y[,"explored"] + 0.25*y[,"grade"]



write.table(rbind(y1,y2), file="dat2.txt",sep="\t",col.names=F,row.names=F)


set.seed(449)
ind=sample(nrow(y1),nrow(y1)*0.2)

write.table(rbind(y1[-ind,],y2), file="train2.txt",sep="\t",col.names=F,row.names=F)
write.table(y1[ind,], file="test2.txt",sep="\t",col.names=F,row.names=F)


write.table(y2, file="train_extra.txt",sep="\t",col.names=F,row.names=F)






## freq-prediction

import glob
import sklearn
import numpy as np
from numpy import genfromtxt
from sklearn import ensemble
import pickle
import sys 

num_tree=1000
max_depth=15

train=np.genfromtxt('train1.txt',delimiter='\t')
test=np.genfromtxt('test1.txt',delimiter='\t')

X=train[:,1:]
Y=train[:,0]

est = sklearn.ensemble.RandomForestRegressor(n_estimators=num_tree, max_depth=max_depth, random_state=0).fit(X,Y)
filename = 'rf1.model'
pickle.dump(est, open(filename, 'wb'))
if test.ndim==1:
    test=test.reshape((1,test.shape[0]))

pred=est.predict(test[:,1:])
np.corrcoef(pred,test[:,0])[0,1]

np.savetxt('output_rf1.txt', pred, delimiter='\n')



## freq+sample prediction


import glob
import sklearn
import numpy as np
from numpy import genfromtxt
from sklearn import ensemble
import pickle
import sys 

num_tree=1000
max_depth=15

train=np.genfromtxt('train2.txt',delimiter='\t')
test=np.genfromtxt('test2.txt',delimiter='\t')

X=train[:,1:]
Y=train[:,0]

est = sklearn.ensemble.RandomForestRegressor(n_estimators=num_tree, max_depth=max_depth, random_state=0).fit(X,Y)
filename = 'rf2.model'
pickle.dump(est, open(filename, 'wb'))
if test.ndim==1:
    test=test.reshape((1,test.shape[0]))

pred=est.predict(test[:,1:])
np.corrcoef(pred,test[:,0])[0,1]

np.savetxt('output_rf2.txt', pred, delimiter='\n')

## recommendation

import glob
import sklearn
import numpy as np
from numpy import genfromtxt
from sklearn import ensemble
import pickle
import sys 

num_tree=1000
max_depth=15

train=np.genfromtxt('train3.txt',delimiter='\t')
test=np.genfromtxt('test3.txt',delimiter='\t')

X=train[:,1:66]
Y=train[:,0]

est = sklearn.ensemble.RandomForestRegressor(n_estimators=num_tree, max_depth=max_depth, random_state=0).fit(X,Y)
filename = 'rf3.model'
pickle.dump(est, open(filename, 'wb'))

tmp_test=np.zeros((0,65))
for i in range(test.shape[0]):
	print(i)
	tmp=np.repeat(test[i,1:66],10).reshape((65,10)).T
	tmp[:,55:66]=0
	np.fill_diagonal(tmp[:,55:66], 1)
	tmp_test=np.vstack((tmp_test,tmp))
	

pred=est.predict(tmp_test)
np.savetxt('output_rf3.txt', pred, delimiter='\n')
















> sum((x-y)^2)/5332
[1] 0.007055268
> sum((x/3+x2/3+x1/3-y)^2)/5332
[1] 0.006919285




## prediction

import glob
import sklearn
import numpy as np
from numpy import genfromtxt
from sklearn import ensemble
import pickle
import sys 

num_tree=1000
max_depth=15

train=np.genfromtxt('train.txt',delimiter='\t')
test=np.genfromtxt('test.txt',delimiter='\t')

X=train[:,1:]
Y=train[:,0]

est = sklearn.ensemble.RandomForestRegressor(n_estimators=num_tree, max_depth=max_depth, random_state=0).fit(X,Y)
filename = 'rf.model'
pickle.dump(est, open(filename, 'wb'))
if test.ndim==1:
    test=test.reshape((1,test.shape[0]))
pred=est.predict(test[:,1:])

np.corrcoef(pred,test[:,0])[0,1]
np.savetxt('output_rf.txt', pred, delimiter='\n')
np.savetxt('fi.txt', est.feature_importances_, delimiter='\n')


from sklearn.linear_model import LinearRegression
linear=LinearRegression().fit(X,Y)
pred=linear.predict(test[:,1:])
np.corrcoef(pred,test[:,0])[0,1]
np.savetxt('output_linear.txt', pred, delimiter='\n')


from sklearn.linear_model import Ridge
ridge=Ridge().fit(X,Y)
pred=ridge.predict(test[:,1:])
np.corrcoef(pred,test[:,0])[0,1]
np.savetxt('output_ridge.txt', pred, delimiter='\n')


from sklearn.svm import SVR
svr=SVR().fit(X,Y)
pred=svr.predict(test[:,1:])
np.corrcoef(pred,test[:,0])[0,1]
np.savetxt('output_svm.txt', pred, delimiter='\n')





X=train[:,1:66]
Y=train[:,0]

est = sklearn.ensemble.RandomForestRegressor(n_estimators=num_tree, max_depth=max_depth, random_state=0).fit(X,Y)

pred=est.predict(test[:,1:66])

np.corrcoef(pred,test[:,0])[0,1]



tmp=np.repeat(test[0,1:66],10).reshape((65,10)).T
tmp[:,55:66]=0
np.fill_diagonal(tmp[:,55:66], 1)
pred=est.predict(tmp)

tmp=np.repeat(test[6,1:66],10).reshape((65,10)).T
tmp[:,55:66]=0
np.fill_diagonal(tmp[:,55:66], 1)
pred=est.predict(tmp)


#filename = 'rf1.model'
#pickle.dump(est, open(filename, 'wb'))
#if test.ndim==1:
#    test=test.reshape((1,test.shape[0]))

#filename = 'rf1.model'
#est= pickle.load(open(filename, 'rb'))

pred=est.predict(test[:,1:])
np.savetxt('output.txt', pred, delimiter='\n')

