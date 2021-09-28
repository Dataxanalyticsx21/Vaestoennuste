
###########

#We model the system in discrete time, t=0,1,2...
#The system includes individuals, who age according to the time index.
#We have the variable b and new individuals enter by it
#We also have survival probability p which removes individuals from the system.
#The distribution of growth can be observed while the cumulative growth can be changed pr assumptions


rm(list=ls(all=TRUE)) 
getwd()
setwd("C:/Users/FI003711/Documents/Muut projektit")

#Input tables

#Survival numbers
A<-read.csv2("Tilastoluvuta.csv",sep=";",stringsAsFactors = FALSE)

#Population distribution
A2<-read.csv2("Tilastoluvut2a.csv",sep=";",stringsAsFactors = FALSE)
#A2<-read.csv2("Tilastoluvut2b.csv",sep=";",stringsAsFactors = FALSE)

#Mortality decline
A4<-read.csv2("Tilastoluvut4.csv",sep=";",stringsAsFactors = FALSE)

#Birth distribution
A11<-read.csv2("Tilastoluvut6.csv",sep=";",stringsAsFactors = FALSE)
A11<-as.numeric(A11$x)

#############################


#Simulation function

lol<-function(n,tf,mltp){
  
  
  #Population by sex
  Population_vectorm<-cbind(c(0:100),A2[,2])
  Population_vectorf<-cbind(c(0:100),A2[,3])
  
  colnames(Population_vectorm)<-c("ikä","määrä")
  colnames(Population_vectorf)<-c("ikä","määrä")
  
  popm<-Population_vectorm[,2]
  popw<-Population_vectorf[,2]

  #Migration
  migm<-(15000/14815)*mltp*c(rep(203,5),rep(127,5),rep(107,5),rep(163,5),rep(293,5),rep(401,5),rep(259,5),
  rep(139,5),rep(59,5),rep(47,5),rep(4,5),rep(0,5),rep(0,41))
  migw<-(15000/14815)*mltp*c(rep(167,5),rep(127,5),rep(89,5),rep(90,5),rep(172,5),rep(212,5),rep(162,5),
  rep(81,5),rep(41,5),rep(26,5),rep(0,5),rep(0,5),rep(0,41))
  
  #Births
  B2<-A11*tf
  
  #Deaths
  dtm<-1-c(diff(100000-A[,2]),1444)/A[,2]
  dtw<-1-c(diff(100000-A[,3]),1444)/A[,3]
  Multiplier=A4$Yearly.Multiplier
  Multiplier<-as.double(Multiplier)
  
if(n>0){
for (i in 1:n){

  
  
#Amount of people who have entered the model
btb<-0.512*sum(B2*popw)
btg<-0.488*sum(B2*popw)


#The amount of people who survived
popm<-popm*dtm
popw<-popw*dtw

#Survival probabilities increase
dtm<-1-(1-dtm)*Multiplier
dtw<-1-(1-dtw)*Multiplier

#The amount of people moving in or out
popm<-popm+migm
popw<-popw+migw

#Year passes and we age everyone with 1 year:
popm<-c(btb,popm)[1:101]
popw<-c(btg,popw)[1:101]

}
  
  
return(data.frame("Men"=round(popm,0),"Women"=round(popw,0)))
  } else {}
  return(data.frame("Men"=round(popm,0),"Women"=popw))}


#We simulate the outcomes:

#This function returns total population
lol2<-function(c,tf,mltp){
  for (i in 1:length(c)){
  c[i]<-(sum(lol(i-1,tf,mltp)))}
  return(c)
}

#Distribution seems to be okay
#Now to plotting:

#lol returns population distribution while lol2 returns the sum

plot(c(2018:2070),lol2(c(2018:2070),1.45,0.02),type="l",xlab="Vuosi",ylab="Väestön määrä")
plot(c(2018:2070),lol2(c(2018:2070),2.1,0.02),type="l",xlab="Vuosi",ylab="Väestön määrä")
plot(c(2018:2070),lol2(c(2018:2070),4.2,0.02),type="l",xlab="Vuosi",ylab="Väestön määrä")

abline(v=2034,col="red")
lol2(c(2017:2070),1.45,1)
lol(55,1.45,1.01)
abline(h=6*10^6)
abline(v=2068)

lines(c(2017:2070),lol2(c(2017:2070),1.45,0),type="l")
plot.window(xlim=c(2017,2070),ylim=c(4000000,6000000))


#Testing different age ranges 
#This can be useful in determining certain demographic rations
sum(lol(0,2.1,0)[16:65,])
sum(lol(0,2.1,0)[66:101,])
3.44/1.12
sum(lol(50,2.1,0)[16:65,])
sum(lol(50,2.1,0)[66:101,])
3.17/1.59

sum(lol(50,1.45,0)[16:65,])
sum(lol(50,1.45,0)[66:101,])
2.34/1.59
sum(lol(50,1.7,1)[16:65,])
sum(lol(50,1.7,1)[66:101,])
3.36/1.74

sum(lol(50,1.45,1)[16:65,])
sum(lol(50,1.45,1)[66:101,])
3.03/1.74

sum(lol(50,0.99,0)[16:65])
sum(lol(50,0.99,0)[66:101])
plot(lol(10,1.7,0),type="h")
1.82/1.59

