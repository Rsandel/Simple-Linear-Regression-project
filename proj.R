setwd("C:/Users/Robert/Desktop/Math 4753/PROJ")
HEADBRAIN=read.table("C:/Users/Robert/Desktop/Math 4753/PROJ/HEADBRAIN.csv",header=TRUE,sep=",")
HEADBRAINF=read.table("C:/Users/Robert/Desktop/Math 4753/PROJ/HEADBRAINFEMALE.csv",header=TRUE,sep=",")
HEADBRAINM=read.table("C:/Users/Robert/Desktop/Math 4753/PROJ/HEADBRAINMALE.csv",header=TRUE,sep=",")
sd(HEADBRAIN$Head.size)
mean(HEADBRAIN$Head.size)
library(s20x)
#determine the best way to compare the data
windows()
pairs(HEADBRAIN)
#create a trend and scatter plot of the data with head size vs brain weight in relation to Males and Female
plot(Brain.weight~Head.size,pch=19,data=HEADBRAIN,col=HEADBRAIN$Gender,main="Brain weight vs Head size")
legend("topleft",legend=c("Male","Female"),col = c("red","black"),pch = 19)
windows()
trendscatter(Brain.weight~Head.size,data=HEADBRAIN,main="Brain weight vs Head size")


#create a linear regression model for males and females
HEADBRAIN.lm=with(HEADBRAIN,lm(Brain.weight~Head.size))
windows()
layout(matrix(c(1,2,3,4),ncol=2,nrow=2))
plot(HEADBRAIN.lm)
summary(HEADBRAIN.lm)
HEADBRAINM.lm=with(HEADBRAINM,lm(Brain.weight~Head.size))
HEADBRAINF.lm=with(HEADBRAINF,lm(Brain.weight~Head.size))
summary(HEADBRAINM.lm)
summary(HEADBRAINF.lm)

#plot with trend line for male, female, and both
windows()

HEADBRAIN.lm.plot=function(x){325.57342+0.26343*x}
plot(Brain.weight~Head.size,data=HEADBRAIN,main="Brain weight vs Head size") 
curve(HEADBRAIN.lm.plot,add = TRUE,col="blue",lwd=2)

HEADBRAINM.lm.plot=function(x){430.3+.23736*x}
plot(Brain.weight~Head.size,data=HEADBRAINM,main="Male Brain weight vs Head size") 
curve(HEADBRAINM.lm.plot,add = TRUE,col="red",lwd=2)

HEADBRAINF.lm.plot=function(x){286.08702+.27280*x}
plot(Brain.weight~Head.size,data=HEADBRAINF,main="Female Brain weight vs Head size") 
curve(HEADBRAINF.lm.plot,add = TRUE,col="green",lwd=2)



#creating the residuals
HEADBRAIN.res=residuals(HEADBRAIN.lm)
HEADBRAINM.res=residuals(HEADBRAINM.lm)
HEADBRAINF.res=residuals(HEADBRAINF.lm)

HEADBRAIN.fit=fitted(HEADBRAIN.lm)
HEADBRAINM.fit=fitted(HEADBRAINM.lm)
HEADBRAINF.fit=fitted(HEADBRAINF.lm)



windows()
trendscatter(HEADBRAIN.res~HEADBRAIN.fit, main = "Residuals vs Fitted", xlab = "Fitted",ylab = "Residuals")

trendscatter(HEADBRAINM.res~HEADBRAINM.fit)
trendscatter(HEADBRAINF.res~HEADBRAINF.fit)


#normality check
windows()
layout(matrix(c(1:2),ncol=2,nrow=1))
normcheck(HEADBRAIN.lm,shapiro.wilk = TRUE)
normcheck(HEADBRAINM.lm,shapiro.wilk = TRUE)
normcheck(HEADBRAINF.lm,shapiro.wilk = TRUE)
ciReg(HEADBRAIN.lm)
summary(HEADBRAIN.lm)

#Sum of squares
windows()
layout((matrix(c(1,2,3,4),2,2,byrow=TRUE)))
layout.show(4)
#plot1 - fitted line
{with(HEADBRAIN,plot(Brain.weight~Head.size,data=HEADBRAIN ,main="Scatter Plot with Fitted Line", xlab="Head Size", ylab="Brain Weight", bg="Blue", pch=21))
abline(HEADBRAIN.lm)
#plot2 - RSS
yhat=fitted(HEADBRAIN.lm)
with(HEADBRAIN,plot(Brain.weight~Head.size, main="RSS:Residual sum of Squares", xlab="Head Size", ylab="Brain Weight", bg="Blue", pch=21))
abline(HEADBRAIN.lm)
with(HEADBRAIN,{segments(HEADBRAIN$Head.size,HEADBRAIN$Brain.weight,HEADBRAIN$Head.size,yhat)})
#plot 3 - MSS
with(HEADBRAIN,plot(HEADBRAIN$Brain.weight~HEADBRAIN$Head.size,  main="MSS: Model Sum of Squares", xlab="Head Size", ylab="Brain Weight", bg="Blue", pch=21))
curve(HEADBRAIN.lm.plot,add=TRUE)
with(HEADBRAIN,abline(h=mean(Brain.weight)))
with(HEADBRAIN, segments(HEADBRAIN$Head.size,HEADBRAIN$Brain.weight,HEADBRAIN$Head.size,mean(HEADBRAIN$Brain.weight),col="Red"))
#plot4 - TSS
with(HEADBRAIN,plot(HEADBRAIN$Brain.weight~HEADBRAIN$Head.size, main="Total Sum of Squares", xlab="Head Size", ylab="Brain Weight", bg="Blue", pch=21))
with(HEADBRAIN,abline(h=mean(HEADBRAIN$Brain.weight)))
with(HEADBRAIN, segments(HEADBRAIN$Head.size,HEADBRAIN$Brain.weight,HEADBRAIN$Head.size,mean(HEADBRAIN$Brain.weight),col="Green"))
}

summary(HEADBRAIN.lm)
ciReg(HEADBRAIN.lm)
#Cooks
windows()
cooks20x(HEADBRAIN.lm)
abline(h=.02)
cooks20x(HEADBRAINM.lm)
cooks20x(HEADBRAINF.lm)


#prediction values
windows()
layout(1,1)
prediction=predict(HEADBRAIN.lm, data.frame(Head.size=c(3000,3250,3500,3750,4000)))

prediction

HB=c(3000,3250,3500,3750,4000)
plot(Brain.weight~Head.size,bg="Blue",pch=21,cex=1.2, main="Brain weight prediction",data=HEADBRAIN)
curve(HEADBRAIN.lm.plot, lwd=2, col="green",add=TRUE)
points(HB,prediction,col='red',cex=1,pch=19)






