##  p17-18
n<-500
diet<-0.1 
effect<-c(0,0.95)
names(effect)<-c('FA', 'FB') 
f.chance<-runif(n) 
f<-ifelse(f.chance<0.9, 'FA', 'FB')
group<-runif(n)
group<-ifelse(group<0.5,'control','drug')
diet.chance<-runif(n)
drug.chance<-runif(n)
outcome<-((diet.chance<diet)|(drug.chance<effect[f]*(group=='drug')))
trail<-data.frame( group=group,F=f,treatment =outcome) 
summary(trail) 

with( trail [group == 'control',],table( F,treatment))
#treatment

with( trail [group == 'drug.',],table( F,treatment))
#treatment

chisq.test(treat.group)


# p 18 1.1.3
library(ggplot2)
p<-ggplot(data.frame(x= c(0,1)), aes(x =x))
p+stat_function( fun =dbeta,args = list( 0.1, 0.9), colour = 'red') 

betad.mean<-function(alpha,beta){alpha/(alpha + beta)}
betad.mode<-function(alpha,beta){(alpha + 1) /( alpha + beta -2) }
alpha<-0.1
beta<-0.9 
false.control<-treat.group [1,1]
true.control<-treat.group [1,2]
false.drug<-treat.group [2,1]
true.drug< treat.group[2,2]

alpha.control<-alpha + true.control 
beta.control<-beta + false.control
alpha.drug<-alpha + true.drug 
beta.drug<-beta + false.drug 
p<-ggp1ot( data.frame( x = c( 0,.3)) ,aes(x = x)) 

p + stat_function(fun=dbeta,args= list(alpha.drug,beta.drug),co1our = Iredl) + annotate("text" ,x =.03,Y= 20,label= "contro1") +
  annotate("text" ,x = .23, Y = 15,label="drug" )
            betad.mean(alpha.contro1,beta.control)
            betad.mode(a1pha.contro1,beta.control)
            betad.mean(a1pha.drug,beta.drug) 
            betad.mode(a1pha.drug,beta.drug) 
            
            
      
            
            
#2.3.5
library(LearnBayes)
library( ggplot2) 
p<-seq(0.05, 0.95, by=0.1)
prior<-c(1, 5, 8, 7, 4.5, 2, 1, 0.7, 0.5, 0.2) 
prior<-prior/sum( prior) 
data<-c(12, 18) 
post<-pdisc( p, prior, data) 
prob<-c(prior, post) 
type<-factor(rep(c( "prior", "posterior"), each= 10)) 
n<-as.numeric( rep( 1: 10, times = 2) ) 
d.prior<-data.frame(prob, type, n) 
ggplot(d.prior, aes(x=n,y=prob, fill=type))+geom_bar(stat = "identity" , position="dodge")
         
quantile2 = list(p = 0.9, x = 0.5) 
quantile1=list(p=0.5,x=0.3)
beta.prior <-beta.select(quantile1,quantile2 )
a <-beta.prior[1] 
b <-beta.prior[2] 
print (c(a, b)) 

s=12
f=18
ggplot(data.frame(x = c(0, 1)),aes(x = x)) + stat_function(fun = dbeta,args= list (shape1 = a, shape2 = b), geom= "area", fill = "blue", alpha = 0.3, colour = "blue", lwd = 1) + stat_function(fun = dbeta, args = list(shape1 = s + a, shape2 = f + b), geom = "area",fill ="red",alpha=0.3,cplour= "red", lwd=1)+ annotate("text",x=0.25,y=3, label = "prior")+annotate("text",x=0.37,y=5.3,label="posterior")




#3.2.3
alpha<-438 
beta<-544 
postmean<-alpha/(alpha + beta)
print("The posterior mean is")
print(postmean)
poststd<-sqrt(alpha*beta/(alpha+ beta)/(alpha+ beta)/(alpha + beta + 1))
print("The posterior standard deviation is") 
print(poststd) 
postmedian<-qbeta(0.5, alpha,beta)
print( "The median ba.sed on posterior distribution is")
print(postmedian) 
CI_95<-c(qbeta(0.025,alpha, beta), qbeta(0.975, alpha,beta)) 
print( "the 95% posterior confidence interval is") 
print(CI_95) 


alpha<-438;beta<-544 
theta<-rbeta( 1000, alpha, beta) 
sort_theta<-sort ( theta) 
spostmean<-mean(theta)
spoststd<-sd(theta) 
spostmedian<-sum(sort_theta[500:501])/2 
approxCI_95<-c( spostmean -1.96*spoststd,spostmean+ 1.96*spoststd)
print(spostmean)
print(spoststd) 
print(spostmedian)
print("The 95 % confidence interval of theta ba.sed on nomal approximation is")
print(approxCI_95) 


alpha<-438; beta<-544 
theta<-rbeta(1000,alpha,beta) 
logit_theta<-log(theta/( 1-theta) ) 
sort_logit_theta<-sort(logit_theta)
slogit_median<-sum( sort_1ogit_theta[500: 501]) 2
slogit_postmean< -mean(logit_theta)
slogit_poststd<-sd(logit_theta)
L<-slogit_postmean-1.96*slogit_poststd
U<-slogit_postmean+1.96*slogit_poststd
approx_logit=c(L,U) 
approx_CI = c(exp(L)/(1+ exp( L)), exp(U)/( 1 + exp( U))) 
print(slogit_postmean)
print(slogit_poststd)
print(slogit_median)
print("the 95% confidence interval of logit{ theta) based on normal approximation is") 
print(approx_logit)
print("the 95% confidence interval of theta based on normal approximation is") 
print(approx_CI)
phi<-(1-theta)/theta
sort_phi<-sort(phi)
sphi_median<-sum(sort_phi[500:501])/2
sphi_postmean<-mean(phi)
sphi_poststd<-sd(phi)
L<-sphi_postmean-1.96*sphi_poststd
U<-sphi_postmean+1.96*sphi_poststd
approxphi_CI<-c(L,U) 
print(sphi_postmean)
print(sphi_poststd) 
print(sphi_median) 
print( "The 95% confidence interval of phi = (1- theta) /theta is") 
print(approxphi_CI) 


alpha<-438;beta<-544
theta<-rbeta(1000,alpha,beta)
par(mfrow=c(1,3))
par(mar = c(5,4,2,1)) 
hist(theta,breaks=seq(0.35,0.55,0.005),
     xlim=c(0.35,0.55),
     main="",xlab=quote(theta),probability="T")
logit_theta<-log(theta/(1-theta))
breaks<-quantile(logit_tjeta,0:20/20)
par(mar=c(5,4,2,1))
hist(logit_theta,breaks=seq(-0.5,0.1,0.01),
     xlim=c(-0.5,0.1),main="",
     xlab=quote(logit(theta)==log(theta/(1-theta))),
     probability=T)
phi=(1-theta)/theta
breaks<-quantile(phi,0:20/20)
par(mar=c(5,4,2,1))
hist(phi,breaks=seq(0.8,1.6,0.01),
     xlim=c(0.8,1.6),main="",
     xlab=quote(phi==(1-theta)/theta),
     probability=T)

#3.3.8
library(MCMCpack)
swiss.posterior1<-MQMCregress (Fertility~ Agriculture + Examination + Education + Catholic + Infant. Mortality,data = swiss, marginal.likelihood="Chib95 " , b0 = 0, B0 =0.1, c0 = 2, d0 = 0.11) 
swiss.posterior2<-MQMCregress(Fertility~ Agriculture +Education+ Catho1ic + Infant. Mortality, data = swiss, marginal.likelihood="Chib95 " , b0 = 0, B0 =0.1, c0 = 2, d0 = 0.11) 
swiss.posterior3<-MQMCregress (Fertility~ Agriculture + Examination +Catholic + Infant. Mortality,data = swiss, marginal.likelihood="Chib95 " , b0 = 0, B0 =0.1, c0 = 2, d0 = 0.11) 
bf<-BayesFactor(swiss.posterior1, swiss.posterior2, swiss.posterior3) 
summary(swiss.posterior2) 