rm(list=ls(all=TRUE))
require(expm)
require(ggplot2)
require(popbio)
require(grid)


require(gridExtra)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## parameter values

m1 <- 0.5 # number off offspring produced by a1 year old
m2 <- 1.5
m3 <- 2.5
m4 <- 7
p1 <- 0.2 #
p2 <- 0.25
p3 <- 0.5

#######A)
L <- rbind(c(m1,m2,m3,m4),c(p1,0,0,0),c(0,p2,0,0),c(0,0,p3,0))
colnames(L)=rownames(L)=c("Age1","Age2","Age3","Age4")
L
str(L)

n.init <- c(10,10,10,10)
T <- 20


n.hist <- array(dim=c(T,4))

#project population
library(expm)
for (i in 1:T) n.hist[i,]<-(L%^%(i-1))%*%n.init  # leslie matrix to the power of i times the initial population vector to calculate population at time t=i

clz <- as.factor(rep(c("Age1","Age2","Age3","Age4"),each=T))
d1 <- data.frame(time=rep(1:T,4),n=as.vector(n.hist),Class=clz) # data frame of population during 20 iterations 

#plot population projection
## Leslie matrix

library(ggplot2)

#plot absolute population
p1 <- ggplot(data=d1,aes(x=time,y=n,color=Class))+
  geom_line(size=1.2)+
  theme_bw()+
  #guides(color=F)+
  labs(y="")+
  scale_x_continuous(expand=c(0,0),limits=c(0,T),breaks=seq(0,T,5))+
  theme(axis.text.x=element_blank())+theme(axis.title.x=element_blank())
print(p1)
x11()
#plot proportion (sAtBLE AGE DISTRIBUTION)
n.tot <- apply(n.hist,1,sum) # 
d2 <- data.frame(time=rep(1:T,4),n=as.vector(n.hist/n.tot),Class=clz) # weired proportion

p2 <- ggplot(data=d2,aes(x=time,y=n,color=Class))+
  geom_line(size=1.2)+
  theme_bw()+
  labs(y="")+
  #guides(color=T)+
  scale_x_continuous(expand=c(0,0),limits=c(0,T),breaks=seq(0,T,5))+
  scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,0.1))+
  theme(axis.text.x=element_blank())+theme(axis.title.x=element_blank())
print(p2)
eigen.analysis(L)$stable.stage # alternatively using the popbio package

#### B) 

#calculate lambda
lambda <- n.tot[2:T]/n.tot[1:(T-1)] 
d3 <- data.frame(time=1:(T-1),lambda=lambda)
eigen.analysis(L)$lambda # alternatively use the popbio package

p3 <- ggplot(data=d3,aes(x=time,y=lambda))+
  geom_line(size=1.2)+
  theme_bw()+
  labs(x="Time (years)",y="")+
  scale_x_continuous(expand=c(0,0),limits=c(0,T),breaks=seq(0,T,5))
print(p3)
library(grid)
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1,heights=c(0.33,0.33,0.33))

#sad see aboce
#age specific reproductive values v1,v2,v3,v4
eigen.analysis(L)$repro.value







#####right whale code
T=20

# Five different initial populations
n.init.ref <- rep(10,4) # standard pop
n.init.1 <- n.init.ref+c(10,0,0,0) #add ten calves
n.init.2 <- n.init.ref+c(0,10,0,0)
n.init.3 <- n.init.ref+c(0,0,10,0)
n.init.4 <- n.init.ref+c(0,0,0,10)

# For storing 5 timeseries:
n.ref <- array(dim=c(T,4))
n.1 <- array(dim=c(T,4))
n.2 <- array(dim=c(T,4))
n.3 <- array(dim=c(T,4))
n.4 <- array(dim=c(T,4))

# Iterate all 5 by tmax:
for (i in 1:T){ 
  n.ref[i,]<-(L%^%(i-1))%*%n.init.ref
  n.1[i,]<-(L%^%(i-1))%*%n.init.1
  n.2[i,]<-(L%^%(i-1))%*%n.init.2
  n.3[i,]<-(L%^%(i-1))%*%n.init.3
  n.4[i,]<-(L%^%(i-1))%*%n.init.4
}

# Calc total pop sizes
n.tot.ref <- apply(n.ref,1,sum)
n.tot.1 <- apply(n.1,1,sum)
n.tot.2 <- apply(n.2,1,sum)
n.tot.3 <- apply(n.3,1,sum)
n.tot.4 <- apply(n.4,1,sum)


### plot results
clz <- as.factor(rep(c("None","Age1","Age2","Age3","Age4"),each=T))
#clz <- factor(clz,levels=levels(clz)[c(4,1,2,3,5)])
d.rv <- data.frame(x=rep(1:T,5),
                   y=c(n.tot.ref,n.tot.1,n.tot.2,n.tot.3,n.tot.4),
                   Class=clz)

n = 4
cols = c("black",gg_color_hue(n))

ggplot(data=d.rv,aes(x=x,y=y,color=Class))+
  geom_line(size=1.2)+
  theme_bw()+
  labs(x="\nTime (years)",y="Total population size\n",color="10 extra fish class:")+
  #scale_x_continuous(expand=c(0,0),limits=c(0,T),breaks=seq(0,T,5))+
  #scale_y_continuous(expand=c(0,0),breaks=seq(35,60,5))+
  scale_colour_manual(values=cols)
x11()
### pop numbers
n.tot.ref[T]
n.tot.1[T]
n.tot.2[T]
n.tot.3[T]
n.tot.4[T]



### Calculate relative effects: these are the RV: # tot.1 = reference
v2=((n.tot.2-n.tot.ref)/(n.tot.1-n.tot.ref))[T]
v3=((n.tot.3-n.tot.ref)/(n.tot.1-n.tot.ref))[T]
v4=((n.tot.4-n.tot.ref)/(n.tot.1-n.tot.ref))[T]
c(1,v2,v3,v4)
eigen.analysis(L)$repro.value
#####





require(popbio)
pro=pop.projection(L,c(10,10,10,10),100)
eigen.analysis(L)
elas=sensitivity(L,zero=T)
elas= elasticity(L)
image2(elas,mar=c(1,6,6.5,1))
x11()
n0=c(200,200,200,200)


#exercise 1 
#parameters
P1=0.2
m1=0.5
m2=3
S=rbind(c(m1,m2),c(P1,0))
colnames(S)=rownames(S)=c("Age1","Age2")
require(popbio)
elas=sensitivity(S,zero=T)
image2(elas,mar=c(1,6,6.5,1))

elas=elasticity(S)


### tutorial
### http://ecovirtual.ib.usp.br/doku.php?id=en:ecovirt:roteiro:pop_str:pstr_mtr

# The growth of a population with an age structure can the 
# projected using matrix algebra. Leslie matrices have the
# information about birth and death rates of different age 
# classes of a population and are a robust way of figuring 
# out the population growth and make projections for different
# scenarios. A generalization of the Leslie matrix occurs when 
# the population is classified due to development stages instead
# of age classes (Lefkovitch matrices). In this scenario, an
# individual may reproduce, die, grow from one stage to another,
# or stay in the same stage. In this generalization, the basic
# vital rates are built into the transition matrix elements,
# which are used to figure out the effect that individuals from
# having a number of individuals in each class on the number of
# individuals in each class at the next time step.

# Leslie matrix
A <- matrix(c(0, 0.5, 20, 0.3, 0, 0, 0, 0.5, 0.9), nr = 3, byrow = TRUE)
A
# initial population vector N  
N0 <- matrix(c(100, 250, 50), ncol = 1)

#To project the population for the next time step (in our case, next year), we need to calculate: 
#N(t+1)=An(t)
N1 <- A %*% N0
N1

#project more time intervals
years <- 10
N.projected <- matrix(0, nrow = nrow(A), ncol = years+1)
N.projected[, 1] <- N0

for (i in 1:years)
{
  N.projected[, i + 1] <- A %*% N.projected[,i]
}

#alternatively see above
(A%^%10)%*%N0


par(mfrow=c(1,2))
matplot(0:years, t(N.projected), type = "l", lty = 1:3, ylab = "n(t)", xlab = "Time (t)", xlim=)
legend("topleft", legend = c("Seed", "Juvenile", "Adult"),lty = 1:3, col = 1:3, bty = "n")
matplot(0:years, log(t(N.projected)), type = "l", lty = 1:3, ylab = "n(t)", xlab = "Time (t)", xlim=)
legend("topleft", legend = c("Seed", "Juvenile", "Adult"),lty = 1:3, col = 1:3, bty = "n")
par(mfrow=c(1,1))

#Here goes a function that project populations from 
#the transition matrix and initial state (tmax is the
#maximum time). That's basically what we did above, but
#now wrapped up as a function.
########################
## Matrix projection
########################
proj.mat<-function(n0, matproj, tmax)
{
  res.mat<-matrix(NA,nrow=tmax+1,ncol=length(n0))
  res.mat[1,]<-n0
  for(i in 2:(tmax+1))
  {
    res.mat[i,]=matproj %*% res.mat[(i-1),]
  }
  return(res.mat)
}
######################
## running the function ##
######################
nEst<-proj.mat(n0=N0, matproj=A , tmax=10)
matplot(1:11, nEst, type="l")
#########################
# population size ##
########################
nPop<-apply(nEst,1, sum)
plot(1:11, nPop)


#growth rate  lamda = n(t+1)/n(t)
# Population growth ##
#############################
lambPop<-nPop[2:11]/nPop[1:10]
matplot(1:10, lambPop, type="b", pch=1)

##########################
# stage proportion ##
##########################
propEst<-nEst/nPop
apply(propEst,1,sum) # 1 by row, 2 by column
matplot(1:11, propEst, type="l")

###########
#Perturbation analyses
############
# We can infer the contribution of each matrix element to the
# total population growth rate by doing perturbation analyses
# on the matrix. The logic behind them is simple: if we change
# only one of the transition values, keeping everything else 
# constant, the change in λ that we will see is a reflex of the
# change in the element we are looking at. This way, we can assess
# the contribution of each transition element, and consequently 
# of the vital rates for each life stage, to the growth of the
# population as a whole. In our example matrix, the transition 
# (or more accurately permanence) in the adult stage corresponds
# to the survival rate in this stage. We can then ask the
# following question:
#   
# If some external factor changed the adult survival rate, what would be the consequence for the population as a whole?
# Let's answer that question?
###########################################
## Disturbing the adult survival rate ##
###########################################
pert=seq(0,1, by=.05)
resAd=rep(NA, length(pert)) 
names(resAd)<-paste("p", pert, sep="_")
for(i in 1:length(resAd))
{
  Ai<-A
  Ai[3,3]<-pert[i] #change entry 3,3
  projAi= proj.mat(n0=N0,matproj=Ai, tmax=100) # project 100 years
  nAi=apply(projAi, 1, sum) # summ all individuals in the population across all timesteps
  lambi=nAi[101]/nAi[100] # calculate lambda at last timestep
  resAd[i]<-lambi # store all the lambdas for the perturbed parameter 
}
resAd

