library(ggplot2)

##Import Data: daily stock returns from ten different stocks

d10 <- as.data.frame(read.csv("/home/anibal/Mis documentos/INTA/SENSORIAL/PANEL/2022/PCA-in-time-series/Data/WW4a.csv")[,-1])
d10 <- t(t(d10) - colMeans(d10))
rownames(d10) <- seq(as.Date("2016/8/2"), by="day",
                     length=107)

cov(d10)
cor(d10)

##Plot Time Series Data

plot(seq(as.Date("2016/8/2"), by="day", length=107),d10[,1],
     type='l',ylab="Stock Returns",xlab="Day")
for(i in 2:10){
  lines(seq(as.Date("2016/8/2"), by="day", length=107),
        d10[,i],type='l',col=i)
}
legend("topleft",legend=colnames(d10),col=1:10,lty=1)


##Principal Component Analysis
pca <- princomp(d10)
pca <- princomp(d10,cor=F)
lds <- pca$loadings
screeplot(pca,type="lines",main="screeplot")
pca <- princomp(d10,cor=T)
lds <- pca$loadings
scs <- pca$scores
screeplot(pca,type="lines",main="screeplot")

##Plot: Sectors by Their First 2 Loadings
library(ggplot2)
C <- as.data.frame(cbind(lds[,1],lds[,2]))
ggplot(C,aes(C[,1],C[,2],label=rownames(C))) +
  geom_point(size=4,col=3) +
  geom_text(vjust=0,hjust=0,angle = 10,size=5) +
  xlab("Loading 1") +
  ylab("Loading 2")


##Plot: Time Points by Their Scores on First 2 Components
C <- as.data.frame(cbind(scs[,1],scs[,2]))
palette(rainbow(400))
ggplot(C,aes(C[,1],C[,2],label=substring(rownames(C),1,7))) +
  geom_point(size=4,col=1:nrow(C)) +
  geom_text(vjust=0,hjust=0,angle = 10,size=5) +
  xlab("Scoring 1") +
  ylab("Scoring 2")
palette("default")

##Plot: Time Points by Their Scores on First 2 Components
C <- as.data.frame(cbind(scs[,1],scs[,2]))
palette(rainbow(400))
ggplot(C,aes(C[,1],C[,2],label=substring(rownames(C),1,4))) +
  geom_path() +
  geom_point(size=4,col=1:nrow(C)) +
  geom_text(vjust=0,hjust=0,angle = 10,size=5) +
  xlab("Scoring 1") +
  ylab("Scoring 2")
palette("default")
plot(seq(as.Date("2009/4/1"), by="month", length=107),C[,1],
     type="l",ylab="Scoring 1",xlab="Date")
plot(seq(as.Date("2009/4/1"), by="month", length=107),C[,2],
     type="l",ylab="Scoring 2",xlab="Date")
ag <- aggregate(C[,1:2],list(substr(rownames(C),1,4)),mean)
C <- as.data.frame(ag)
ggplot(C,aes(C[,2],C[,3],label=C[,1])) +
  geom_point(size=4,col=3) +
  geom_text(vjust=0,hjust=0,angle = 10,size=5) +
  xlab("Scoring 1") +
  ylab("Scoring 2")
q()
