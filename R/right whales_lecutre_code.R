#### Examples with right whales
#clear memory
rm(list=ls(all=TRUE))
par(ask=F)
devAskNewPage(ask = FALSE)


install.packages('ggplot2')
install.packages("expm")
## For matrix powers we use the package expm,
## which has an %^% operator.
## Google cran expm to see the package manual
library(expm)

## need color codes:
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

## Parameter values
b <- 0.3 # fertility 
s.IC <- 0.92 #transition probability calf to immature
s.II <- 0.86 # probability to stay immature
s.MI <- 0.08 #trans prob immature to mature
s.MM <- 0.8 # prob to stay mature
mu <- 0.12  # was 0.12 orig data from paper 0.17  death rate of reproductive ind
s.MR <- 1-mu 
s.RI <- 0.02 # transition immature to reproductive
s.RM <- 0.19 #transition probability mature to reproductive
s.RR <- 0 # ptob to stay reproductive
## /Parameter values

## Define the transition matrix
## bind the four rows:
A <- rbind(c(0,0,0,b),c(s.IC,s.II,0,0),c(0,s.MI,s.MM,s.MR),c(0,s.RI,s.RM,s.RR))
str(A)
rownames(A)=c("Calv","Immature","Mature","Reproductive")
colnames(A)=c("Calv","Immature","Mature","Reproductive")


## Iterate the model, t.max times:

t.max <- 50

# Define array to store iterations
# 4 classes per iteration
n <- array(dim=c(t.max,4))

# Initial population:
n.init <- rep(100,4)

# Iterate:
for (i in 1:t.max) n[i,]<-(A%^%(i-1))%*%n.init
# This was actually rather primitive (no pun). Can you think of
# a more efficient way to do the iterations? recursive equation


####### Produce graphs for slide 40 ###########################################
clz <- as.factor(rep(c("Calf","Immature","Mature","Reproductive"),each=t.max))
d1 <- data.frame(time=rep(1:t.max,4),n=as.vector(n),Class=clz)
library(ggplot2)

p1 <- ggplot(data=d1,aes(x=time,y=n,color=Class))+
geom_line(size=1.2)+
theme_bw()+
#guides(color=F)+
labs(y="")+
scale_x_continuous(expand=c(0,0),limits=c(0,t.max),breaks=seq(0,t.max,5))+
scale_y_continuous(expand=c(0,0),limits=c(0,300),breaks=seq(0,300,50))+
theme(axis.text.x=element_blank(),legend.position = "bottom")+theme(axis.title.x=element_blank())
print(p1)

n.tot <- apply(n,1,sum) # calculate total population at each time step
d2 <- data.frame(time=rep(1:t.max,4),n=as.vector(n/n.tot),Class=clz)



p2 <- ggplot(data=d2,aes(x=time,y=n,color=Class))+
geom_line(size=1.2)+
theme_bw()+
labs(y="")+
scale_x_continuous(expand=c(0,0),limits=c(0,t.max),breaks=seq(0,t.max,5))+
scale_y_continuous(expand=c(0,0),limits=c(0,0.65),breaks=seq(0,0.65,0.1))+
theme(axis.text.x=element_blank(),legend.position = "bottom")+theme(axis.title.x=element_blank())
print (p2)

# calculate growth rate lambda
lambda <- n.tot[2:t.max]/n.tot[1:(t.max-1)]
d3 <- data.frame(time=1:(t.max-1),lambda=lambda)

p3 <- ggplot(data=d3,aes(x=time,y=lambda))+
geom_line(size=1.2)+
theme_bw()+
labs(x="Time (years)",y="")+
scale_x_continuous(expand=c(0,0),limits=c(0,t.max),breaks=seq(0,t.max,5))+
scale_y_continuous(expand=c(0,0),limits=c(0.98,1.02),breaks=seq(0.98,1.02,0.01))
print(p3)
library(grid)
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=1,heights=c(0.33,0.33,0.33))

### /graphs  #######################################################################


## Eigen analysis:
##
## The standard function eigen calculates eigenvalues and eigenvectors

whale.eig <- eigen(A) # gives eigenvevalues and eigenvectors. the dominatne eigenvavue correnponds with the eingevector

#slide 41ff NA
 # U=whale.eig$vectors
 # AL= rbind(c(whale.eig$values[1],0,0,0),c(0,whale.eig$values[2],0,0),c(0,0,whale.eig$values[3],0),c(0,0,0,whale.eig$values[4]))
 # V=eigen(t(A))$vectors
 # u1=whale.eig$vectors[,1]
 # v1=eigen(t(A)$vectors)[,1]
 # lam1=whale.eig$values[1]
 # 

#slide 44 45
## Is A primitive? 
## If raised to a sufficiently high power makes all
## entries positive, then A is primitive.
# raise A to second power:
A%^%2
# still contains zeroes
A%^%3
# All entries>0 ==> A primitive ==> A has real positive eigenvalue
# with strictly larger magnitude than all others
#also irreducible becuase all non 0 entries

#slide46
# Pick the leading eigenvalue:
lambda1 <- whale.eig$values[1]
# Pick the leiding right eigenvector and standardize it:
(u <- whale.eig$vectors[,1]/sum(whale.eig$vectors[,1]))
# This is, of course, the stable class distribution
# Check that it sums to 1
sum(u)
 A%*%u
 lambda1%*%u




##### Reproductive values #############################

## Numerical experiment slide 47
## Add 10 whales to standard population (10,10,10,10)
## Either 10 calves, 10 imm, 10 mat or 10 rep,
## and see how that affects long-run growth


# Five different initial populations
n.init.1 <- rep(10,4) # standard pop
n.init.C <- n.init.1+c(10,0,0,0) #add ten calves
n.init.I <- n.init.1+c(0,10,0,0)
n.init.M <- n.init.1+c(0,0,10,0)
n.init.R <- n.init.1+c(0,0,0,10)

# For storing 5 timeseries:
n.1 <- array(dim=c(t.max,4))
n.C <- array(dim=c(t.max,4))
n.I <- array(dim=c(t.max,4))
n.M <- array(dim=c(t.max,4))
n.R <- array(dim=c(t.max,4))

# Iterate all 5 by tmax:
for (i in 1:t.max){ 
  n.1[i,]<-(A%^%(i-1))%*%n.init.1
  n.C[i,]<-(A%^%(i-1))%*%n.init.C
  n.I[i,]<-(A%^%(i-1))%*%n.init.I
  n.M[i,]<-(A%^%(i-1))%*%n.init.M
  n.R[i,]<-(A%^%(i-1))%*%n.init.R
}

# Calc total pop sizes
n.tot.1 <- apply(n.1,1,sum)
n.tot.C <- apply(n.C,1,sum)
n.tot.I <- apply(n.I,1,sum)
n.tot.M <- apply(n.M,1,sum)
n.tot.R <- apply(n.R,1,sum)


### plot results
clz <- as.factor(rep(c("None","Calf","Immature","Mature","Reproductive"),each=t.max))
clz <- factor(clz,levels=levels(clz)[c(4,1,2,3,5)])
d.rv <- data.frame(x=rep(1:t.max,5),
                   y=c(n.tot.1,n.tot.C,n.tot.I,n.tot.M,n.tot.R),
                   Class=clz)

n = 4
cols = c("black",gg_color_hue(n))

ggplot(data=d.rv,aes(x=x,y=y,color=Class))+
geom_line(size=1.2)+
theme_bw()+
labs(x="\nTime (years)",y="Total population size\n",color="10 extra whales of class:")+
scale_x_continuous(expand=c(0,0),limits=c(0,t.max),breaks=seq(0,t.max,5))+
scale_y_continuous(expand=c(0,0),limits=c(35,60),breaks=seq(35,60,5))+
scale_colour_manual(values=cols)
### pop numbers
n.tot.1[t.max]
n.tot.C[t.max]
n.tot.I[t.max]
n.tot.M[t.max]
n.tot.R[t.max]



### Calculate relative effects: these are the RV:
vI=((n.tot.I-n.tot.1)/(n.tot.C-n.tot.1))[t.max]
vM=((n.tot.M-n.tot.1)/(n.tot.C-n.tot.1))[t.max]
VR=((n.tot.R-n.tot.1)/(n.tot.C-n.tot.1))[t.max]

###slide 48
### Now calc reproductive values as left eigenvector
### i.e. right eigenvector of the transpose of A:

(v <- eigen(t(A))$vectors[,1])

# One way to standardize RVs: (set first element to 1)
(v <- v/v[1])

# Other way: set average RV to 1:
(v <- v/sum(v*u))

##slide49
u*v
sum(u*v)


##slide 50 sensitivities
#### Vary b and mu and plot lambda:

## Cycle through mu values
n.mu <- 100 # how many values?
mus <- seq(0.5*mu,2*mu,length=n.mu) # from to
lambdas1 <- numeric(n.mu) # to store results
for (i in 1:n.mu){
  A[3,4] <- 1-mus[i] # replace row 3 col 4 sMR
  lambdas1[i]<-max(eigen(A)$values)
}
A[3,4]<-1-mu  # reset to default

#same for fertility
bs <- seq(0.5*b,2*b,length=n.mu) # change fertility b
lambdas2 <- numeric(n.mu)
for (i in 1:n.mu){
  A[1,4]<-bs[i]
  lambdas2[i]<-max(eigen(A)$values)
}
A[1,4]<-b # restore to default

##set up results for plotting
d.lambda <- data.frame(x=c(mus,bs),y=c(lambdas1,lambdas2),
                       par=as.factor(rep(c("mu","b"),each=n.mu)))

#### plot slide 50
n = 2
cols = gg_color_hue(n)
ggplot(data=d.lambda,aes(x=x,y=y,color=par))+
geom_abline(intercept=1,slope=0,color="black",lty=1)+
geom_abline(intercept=1.003446,slope=0,color="grey",lty=2)+
geom_vline(xintercept=0.12,lty=2,color=cols[2])+
geom_vline(xintercept=0.3,lty=2,color=cols[1])+
geom_line(size=1.2)+
theme_bw()+
labs(color="Parameter",x="\nParameter values",y="Leading eigenvalue\n")+
scale_x_continuous(expand=c(0,0),limits=c(0,0.6),breaks=seq(0,0.6,0.1))+
scale_y_continuous(expand=c(0,0),limits=c(0.97,1.03),breaks=seq(0.97,1.03,0.01))
#### /varying b and mu and plotting the results

########sensitivity
#slide 54
#### Varying s.MI and s.IC and plotting lambda (same as above)
n.s <- 100
s.MI.seq <- seq(0.5*s.MI,2*s.MI,length=n.s)
lambdas1 <- numeric(n.s)
for (i in 1:n.s){
  A[3,2]<-s.MI.seq[i]
  lambdas1[i]<-max(eigen(A)$values)
}
A[3,2]<-s.MI

s.IC.seq <- seq(0.5*s.IC,2*s.IC,length=n.s)
lambdas2 <- numeric(n.s)
for (i in 1:n.s){
  A[2,1]<-s.IC.seq[i]
  lambdas2[i]<-max(eigen(A)$values)
}
A[2,1]<-s.IC
#preparing data
d.lambda <- data.frame(x=c(s.MI.seq,s.IC.seq),y=c(lambdas1,lambdas2),
                par=as.factor(rep(c("sMI","sIC"),each=n.s)))

#plot
n = 2
cols = gg_color_hue(n)
ggplot(data=d.lambda,aes(x=x,y=y,color=par))+
geom_abline(intercept=1,slope=0,color="black",lty=1)+
geom_abline(intercept=1.003446,slope=0,color="grey",lty=2)+
geom_vline(xintercept=s.MI,lty=2,color=cols[2])+
geom_vline(xintercept=s.IC,lty=2,color=cols[1])+
geom_line(size=1.2)+
theme_bw()+
labs(color="Parameter",x="\nParameter values",y="Leading eigenvalue\n")+
scale_x_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1.6,0.2))+
scale_y_continuous(expand=c(0,0),limits=c(0.98,1.03),breaks=seq(0.98,1.03,0.01))
### /varying s.MI and s.IC same as above but following the lecture formulas

#slide52-53
## Sensitivity and elasticity analysis:  result on slide 54
(sensMI <- u[2]*v[3]) # use the vc=1 normalized reproductive values (left eigenvector)
(sensIC <- u[1]*v[2])
ssMI=(u[2]*v[3])/(sum(v*u)) #sMI following slide 52
ssIC=(u[1]*v[2])/sum(v*u)#sIC slide 53
#slide 54
sensMI/sensIC
ssMI/ssIC
# 9 times better to increase s.MI by one unit absolute values


#slide 56 elasticities
## elasticity normalize by parameter 
(eMI <- s.MI/lambda1*u[2]*v[3])
(eIC <- s.IC/lambda1*u[1]*v[2])
eMI/eIC
# But better to increase sIC by one percent


#other things
### Root finding: 
#get lambda from euler lotka eq? 

chareq <- function(lambda) lambda^2-0.5*lambda-0.6
uniroot(chareq,c(0,2))$root



