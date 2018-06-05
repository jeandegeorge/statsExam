### Simulation

mse1=c()
mse2=c()
CRB=c()


#these two for loops enable tocalculate the MSEs of the estimator as well as the CRB
# We simulate the Gamma 10 times for every n (n goes from 10 to 1000 by increments of 10)

for(j in seq(10,1000,10)){
  
  
  results=c()
  est1=c()
  est2=c()
  se1=0
  se2=0
  

  #10 simulations of gamma(2,2) (n=j)

  for(i in 1:10){
    sim=rgamma(j,shape=2,rate=2)
    sim=unlist(sim)
    mean=mean(sim)
    results[i]=mean
  
    #calculate two estimators
    est1[i]=(2/mean)
    est2[i]=(2*j-1)/(j*mean)
  
    #MSE
    se1=se1+(2-est1[i])^2
    se2=se2+(2-est2[i])^2
  }

mse1[j]=se1/10
mse2[j]=se2/10
CRB[j]=(2^2)/(2*j)

}

#y1 is the mse of theta-bar and y2 is the mse of the mle (mme)
y1=mse1[seq(10,1000,10)]
y2=mse2[seq(10,1000,10)]
CRB=CRB[seq(10,1000,10)]
x=seq(10,1000,10)
plot(x,CRB)

#Plots of the theta-bar and the mle (equal to method of moments) estimator respectively
# black line is CRB
# plotted against x = n (sample size of simulations)

#Estimator 1 plot
par(mar= c(5,4,4,5) + 0.1)
plot(x,CRB, col = "black", type="l", xlab="", ylab="inflation", ylim=c(0,0.15))
par(new = T)
plot(x,y1, col = "red", type="p", xlab="", ylab="inflation", ylim=c(0,0.15))
par(new = F)

#Estimator 2 plot
par(mar= c(5,4,4,5) + 0.1)
plot(x,CRB, col = "black", type="l", xlab="", ylab="inflation", ylim=c(0,0.15))
par(new = T)
plot(x,y2, col = "green", type="p", xlab="", ylab="inflation", ylim=c(0,0.15))
par(new = F)


### Test 1

# Theta0=2 Alpha=0.05 n=100

# t test

#calculation of t statistic
set.seed(1)
sim=rgamma(100,shape=2,rate=2)
sim=unlist(sim)

#x bar
mean=mean(sim)

mean
var

#compute standard error of x bar
se=0
for(i in 1:100){
  se=se+((sim[i]-mean)^2)
}
se=se/100

#compute z stat
zstat= abs(1-mean)/sqrt(se)
zstat

### Test 2

#Wald Test

#calculate the ksi statistic and perform test like in question 12
mle=(2/mean)

stat=2*100*(mle-2)/(mle^2)
stat

