x <- read.csv("CompleteDataset.csv", header=TRUE)
x<-x[,-1]# delete x
x<-x[,-3]# delete photo
x<-x[,-4]# delete flag
x<-x[,-7]# delete logo
table(x$Nationality)

x$Value<-as.character(x$Value)

class(x$Value)

#wage
x$Wage<-substr(x$Wage,start=4,stop=8)
x$Wage<-gsub("K",replacement="",x$Wage) 
x$Wage<-as.numeric(x$Wage)





#hist Age
hist(x$Age)
par(mfrow=c(1,1))
h1<-hist(x$Age,las=TRUE ,col="cadetblue",
         density = 30, angle=50, border="black",
         main="players Age",xlab="player's Age(years)",
         ylab="frequency",cex.lab=0.8)
x1<-seq(min(x$Age),max(x$Age),length=40) 
y1<-dnorm(x1,mean=mean(x$Age),sd=sd(x$Age)) 
y1 <- y1*diff(h1$mids[1:2])*length(x$Age) 
lines(x1, y1, col="blue", lwd=2)

#gk
gk<-x[which(x$Preferred.Positions=="GK "),]
rownames(gk)<-1:nrow(gk)
table(x$Club)
table(gk$Club)


hist(x$Overall)
par(mfrow=c(1,1))
h2<-hist(x$Overall,las=TRUE ,col="cadetblue",
         density = 30, angle=50, border="black",
         main="players Overall",xlab="player's Overall rate",
         ylab="frequency",cex.lab=0.8)
x2<-seq(min(x$Overall),max(x$Overall),length=40) 
y2<-dnorm(x2,mean=mean(x$Overall),sd=sd(x$Overall)) 
y2 <- y2*diff(h1$mids[1:2])*length(x$Overall) 
lines(x2, y2, col="blue", lwd=2)

