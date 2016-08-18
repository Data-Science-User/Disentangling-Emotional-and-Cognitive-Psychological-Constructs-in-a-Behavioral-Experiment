# Read Table
PRE <- read.csv("PANAS(PRE).csv", header=TRUE, sep=",")
NEU <- read.csv("PANAS(NEUTRAL).csv", header=TRUE, sep=",") 
ANG <- read.csv("PANAS(ANGER).csv", header=TRUE, sep=",") 
ANX <- read.csv("PANAS(ANXIETY).csv", header=TRUE, sep=",") 

# Correlation Matrix
# PANAS in Conditions (Pre, Anger, Anxiety, Neutral)
cor(PRE[,4:23], use="complete.obs")
cor(NEU[,4:23], use="complete.obs")
cor(ANG[,4:23], use="complete.obs")
cor(ANX[,4:23], use="complete.obs")

# PRE-PANAS
pre <- PRE[,4:23]
neu <- NEU[,4:23]
ang <- ANG[,4:23]
anx <- ANX[,4:23]
cor(pre, use="complete.obs")
cor(neu, use="complete.obs")
cor(ang, use="complete.obs")
cor(anx, use="complete.obs")
require(lattice)
levelplot(cor(pre, use="complete.obs"), scales=list(x=list(rot=90)))
levelplot(cor(neu, use="complete.obs"), scales=list(x=list(rot=90)))
levelplot(cor(ang, use="complete.obs"), scales=list(x=list(rot=90)))
levelplot(cor(anx, use="complete.obs"), scales=list(x=list(rot=90)))

m.pre<-as.matrix(pre)
m.neu<-as.matrix(neu)
m.ang<-as.matrix(ang)
m.anx<-as.matrix(anx)
#skip down to alpha (no need to S$dataframe...)
#load psych package then call alpha
#within CTT package there is a function 'alpha'
install.packages("CTT")
library(CTT)
alpha.pre<-reliability(m.pre)
alpha.neu<-reliability(m.neu)
alpha.ang<-reliability(m.ang)
alpha.anx<-reliability(m.anx)
alpha.pre
alpha.neu
alpha.ang
alpha.anx
install.packages ('psych', 'GPArotation')
library('psych', 'GPArotation')
omega.pre<-omega(m.pre,2)
omega.neu<-omega(m.neu,2)
omega.ang<-omega(m.ang,2)
omega.anx<-omega(m.anx,2)
omega.pre
omega.neu
omega.ang
omega.anx
m.post<-rbind(m.neu,m.ang,m.anx)
omega.post<-omega(m.post,2)
omega.post
omega.diagram(omega.pre,sl=TRUE,sort=TRUE,labels=NULL,simple=TRUE,
              errors=FALSE, digits=1,e.size=.15,rsize=.15,side=3,
              main="PANAS (pre-test)",cex=.6,
              color.lines=TRUE,marg=c(.5,.5,1.5,.5),adj=2)
omega.diagram(omega.post,sl=TRUE,sort=TRUE,labels=NULL,simple=TRUE,
              errors=FALSE, digits=1,e.size=.15,rsize=.15,side=3,
              main="PANAS (post-test)",cex=.6,
              color.lines=TRUE,marg=c(.5,.5,1.5,.5),adj=2)
omega.diagram(omega.neu,sl=TRUE,sort=TRUE,labels=NULL,simple=TRUE,
              errors=FALSE, digits=1,e.size=.15,rsize=.15,side=3,
              main="PANAS (post-neutral)",cex=.6,
              color.lines=TRUE,marg=c(.5,.5,1.5,.5),adj=2)
omega.diagram(omega.ang,sl=TRUE,sort=TRUE,labels=NULL,simple=TRUE,
              errors=FALSE, digits=1,e.size=.15,rsize=.15,side=3,
              main="PANAS (post-anger)",cex=.6,
              color.lines=TRUE,marg=c(.5,.5,1.5,.5),adj=2)
omega.diagram(omega.anx,sl=TRUE,sort=TRUE,labels=NULL,simple=TRUE,
              errors=FALSE, digits=1,e.size=.15,rsize=.15,side=3,
              main="PANAS (post-anxiety)",cex=.6,
              color.lines=TRUE,marg=c(.5,.5,1.5,.5),adj=2)
# Subscale PANAS(PRE)
pre.pos <- pre[,c("panaspreInterested", "panaspreExcited", "panaspreStrong", 
                    "panaspreEnthusiastic", "panaspreProud", "panaspreAlert", "panaspreInspired", 
                    "panaspreDetermined", "panaspreAttentive", "panaspreActive")]
pre.neg <- pre[,c("panaspreDistressed", "panaspreUpset", "panaspreGuilty", "panaspreScared", 
                    "panaspreHostile", "panaspreIrritable", "panaspreAshamed", "panaspreNervous", 
                    "panaspreJittery", "panaspreAfraid")]
a.pre.pos <-reliability(pre.pos)
a.pre.neg <-reliability(pre.neg)
o.pre.pos <-omega(pre.pos)
o.pre.neg <-omega(pre.neg)
# Subscale PANAS(NEU)
neu.pos <- neu[,c("Interested", "panasneuExcited", "panasneuStrong", 
                  "panasneuEnthusiastic", "panasneuProud", "panasneuAlert", "panasneuInspired", 
                  "panasneuDetermined", "panasneuAttentive", "panasneuActive")]
colnames(neu.pos)[1]<- "panasneuInterested"
neu.neg <- neu[,c("panasneuDistressed", "panasneuUpset", "panasneuGuilty", "panasneuScared", 
                  "panasneuHostile", "panasneuIrritable", "panasneuAshamed", "panasneuNervous", 
                  "panasneuJittery", "panasneuAfraid")]
a.neu.pos <-reliability(neu.pos)
a.neu.neg <-reliability(neu.neg)
o.neu.pos <-omega(neu.pos)
o.neu.neg <-omega(neu.neg)
# Subscale PANAS(ANG)
ang.pos <- ang[,c("panasangInterested", "panasangExcited", "panasangStrong", 
                  "panasangEnthusiastic", "panasangProud", "panasangAlert", "panasangInspired", 
                  "panasangDetermined", "panasangAttentive", "panasangActive")]
ang.neg <- ang[,c("panasangDistressed", "panasangUpset", "panasangGuilty", "panasangScared", 
                  "panasangHostile", "panasangIrritable", "panasangAshamed", "panasangNervous", 
                  "panasangJittery", "panasangAfraid")]
a.ang.pos <-reliability(ang.pos)
a.ang.neg <-reliability(ang.neg)
o.ang.pos <-omega(ang.pos)
o.ang.neg <-omega(ang.neg)
# Subscale PANAS(ANX)
anx.pos <- anx[,c("panasanxInterested", "panasanxExcited", "panasanxStrong", 
                  "panasanxEnthusiastic", "panasanxProud", "panasanxAlert", "panasanxInspired", 
                  "panasanxDetermined", "panasanxAttentive", "panasanxActive")]
anx.neg <- anx[,c("panasanxDistressed", "panasanxUpset", "panasanxGuilty", "panasanxScared", 
                  "panasanxHostile", "panasanxIrritable", "panasanxAshamed", "panasanxNervous", 
                  "panasanxJittery", "panasanxAfraid")]
a.anx.pos <-reliability(anx.pos)
a.anx.neg <-reliability(anx.neg)
o.anx.pos <-omega(anx.pos)
o.anx.neg <-omega(anx.neg)


# create a new data.frame to include the total scores
q.PRE<-pre
q.NEU<-neu
q.ANG<-ang
q.ANX<-anx

#rowSums() calculates the sum of the variables row-wise  (total test score for each person)
q.PRE$total<-rowSums(q.PRE, na.rm=T)
q.PRE$pos<-rowSums(pre.pos, na.rm=T)
q.PRE$neg<-rowSums(pre.neg, na.rm=T)
q.NEU$total<-rowSums(q.NEU, na.rm=T)
q.NEU$pos<-rowSums(neu.pos, na.rm=T)
q.NEU$neg<-rowSums(neu.neg, na.rm=T)
q.ANG$total<-rowSums(q.ANG, na.rm=T)
q.ANG$pos<-rowSums(ang.pos, na.rm=T)
q.ANG$neg<-rowSums(ang.neg, na.rm=T)
q.ANX$total<-rowSums(q.ANX, na.rm=T)
q.ANX$pos<-rowSums(anx.pos, na.rm=T)
q.ANX$neg<-rowSums(anx.neg, na.rm=T)
#examine a table of total score freqencies
lapply(q.PRE[,21:23], table)
q.PRE[ q.PRE == 99 ] <- c("NA")
lapply(q.NEU[,21:23], table)
q.NEU[ q.NEU == 99 ] <- c("NA")
lapply(q.ANG[,21:23], table)
q.ANG[ q.ANG == 99 ] <- c("NA")
lapply(q.ANX[,21:23], table)
q.ANX[ q.ANX == 99 ] <- c("NA")

#plot the table of frequencies
library(ggplot2)
library(reshape2)
library(data.table)

# pre-test --> post-neutral
t.test(q.PRE$total, q.NEU$total)
aa <- hist(q.PRE$pos, freq=T)                     
bb <- hist(q.NEU$pos, freq=T)
plot(aa, col="gray67",main="Total",xlab="Total Score", ylab="Frequency")  # first histogram
plot(bb, col="navyblue", add=T)  # second
# pre-neutral --> post-neutral

# Positive & Negative
a <- hist(q.PRE$pos, freq=T)                     
b <- hist(q.NEU$pos, freq=T)                       
c <- hist(q.ANG$pos, freq=T)                     
d <- hist(q.ANX$pos, freq=T)
e <- hist(q.PRE$neg, freq=T)                     
f <- hist(q.NEU$neg, freq=T)                       
g <- hist(q.ANG$neg, freq=T)                     
h <- hist(q.ANX$neg, freq=T) 
plot(a, col="gray67",main="Total",xlab="Total Score", ylab="Frequency")  # first histogram
plot(b, col="navyblue", add=T)  # second
plot(c, col="red4", add=T)  # third
plot(d, col="green4", add=T)  # fourth
plot(e, col="gray83", add=T)  # fifth
plot(f, col="mediumblue", add=T)  # sixth
plot(g, col="red", add=T)  # seventh
plot(h, col="green", add=T)  # eighth

#while this is helpful, it might be better to view the relative frequencies as a traditional line plot
plot(density(q.PRE$total), title="Density plot of total scores", xlab="total score", ylab="density")

#or as a histogram
hist(q.PRE$total, freq=T)

#to look at relative frequencies (densities), change freq to FALSE
hist(q$total, freq=F)

#you can change the number of breaks by specifying either their number or exact placement
#e.g. this code specifies the number of breaks (assummed equidistant)
hist(q$total, breaks=21, freq=F)

# using the histogram, you can comment on normality, min and max, range, mean, etc. of the total scores
#Item Reponse Frequencies

#instead of creating a separate histogram for each item, the hist_matrix function creates a "table" of histograms to include all of the items (created by Michael Chajewski, revised by Ying Liu, Nov, 11, 2009)
#define the function

hist_matrix<-function(X,nrow,ncol,breaks,count,color,title)
{
  par(mfrow=c(nrow,ncol),mar=c(1.5,1.5,2,1.5), oma=c(1,1,3,1))
  for(i in 1:dim(X)[2]) {
    hist(X[,i],na.rm=T,br=breaks,freq=count,right=T,main=paste("Item",i,sep=" "),col=color,)
  }
  mtext(title,outer=T,line=1,cex=1.2)
}

#use the function on the prca data
hist_matrix(m.pre,nrow=6,ncol=4,breaks=3,count=T,color="lightgray",title="Item Frequencies")

#Item difficulty (severity)

#to be able to interpret continuous items using rules of thumb for binary items, we'll first rescale each item to [0,1]
q1<-((pre-1)/(4-1))
q2<-((neu-1)/(4-1))
q3<-((ang-1)/(4-1))
q4<-((anx-1)/(4-1))

#find the mean of the difficulties
Ave.pre <- lapply(q1, mean, na.rm=TRUE)
Ave.pre<-unlist(Ave.pre)
Ave.neu <- lapply(q2, mean, na.rm=TRUE)
Ave.neu<-unlist(Ave.neu)
Ave.ang <- lapply(q3, mean, na.rm=TRUE)
Ave.ang<-unlist(Ave.ang)
Ave.anx <- lapply(q4, mean, na.rm=TRUE)
Ave.anx<-unlist(Ave.anx)

#find the standard deviation of the difficulties
StDeV.pre <- lapply(q1, sd, na.rm=TRUE)
StDeV.pre<-unlist(StDeV.pre)
StDeV.neu <- lapply(q2, sd, na.rm=TRUE)
StDeV.neu<-unlist(StDeV.neu)
StDeV.ang <- lapply(q3, sd, na.rm=TRUE)
StDeV.ang<-unlist(StDeV.ang)
StDeV.anx <- lapply(q4, sd, na.rm=TRUE)
StDeV.anx<-unlist(StDeV.anx)

#combine the mean and sd into one table using the function cbind to combine the vectors as columns (rbind would combine them as rows)
desc.pre<-cbind(Ave.pre, StDeV.pre)
desc.neu<-cbind(Ave.neu, StDeV.neu)
desc.ang<-cbind(Ave.ang, StDeV.ang)
desc.anx<-cbind(Ave.anx, StDeV.anx)

#write the table to a format suitable for word processing
write.csv(q1, file="q1.csv")

#explore other pertinent descriptive statistics
#e.g range, overall mean, overall sd, etc.
describe(desc.pre)
describe(desc.neu)
describe(desc.ang)
describe(desc.anx)

#Item discrimination
SSd.pre<-pre
SSd.pre$prca2 <- ifelse(SSd.pre[,1] >= 1, 1, 0)
SSd.neu<-neu
SSd.neu$prca2 <- ifelse(SSd.neu[,1] >= 1, 1, 0)
SSd.ang<-ang
SSd.ang$prca2 <- ifelse(SSd.ang[,1] >= 1, 1, 0)
SSd.anx<-anx
SSd.anx$prca2 <- ifelse(SSd.anx[,1] >= 1, 1, 0)

#first, install the packages we'll need
install.packages('ltm', 'psych', 'psychometric')
library('ltm', 'psych', 'psychometric')

#then we can look at the discrimination value
discrim(SSd.pre$prca2)
discrim(SSd.neu$prca2)
discrim(SSd.ang$prca2)
discrim(SSd.anx$prca2)

#Item-total correlation
#the correlation of the item with the total score

item.total(q1) #note that missing data are removed casewise
item.total(q2)
item.total(q3)
item.total(q4)

#Item-rest correlation
#the correlation of the item with the total score minus the item
#returned as part of the item.exam function

item.exam(q1)
item.exam(q2)
item.exam(q3)
item.exam(q4)

#psy package code
require(psy)

#cohen's kappa
ckappa(pre) # Cohen's kappa for binary diagnosis
ckappa(neu)
ckappa(ang)
ckappa(anx)

#cronbach's alpha
cronbach(pre) #alpha's the data
cronbach(neu)
cronbach(ang)
cronbach(anx)

#icc
icc(pre)
icc(neu)
icc(ang)
icc(anx)
icc(cbind(neu,ang,anx)) #post

#lkappa
lkappa(pre) # Light's kappa for non binary diagnosis
lkappa(pre, type="weighted") # Light's kappa for non binary ordered diagnosis
lkappa(neu)
lkappa(neu, type="weighted")
lkappa(ang)
lkappa(ang, type="weighted")
lkappa(anx)
lkappa(anx, type="weighted")

