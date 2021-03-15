library(rmutil)
library(mombf)

#######Setting hyper parameters#####
seq <- seq(-5,5,length=1000)
#####Normal-Spike-and-MOM-Slab####
l1_MOM=0.284215
l0_MOM=.1/qnorm(.975)^2
#####Normal-Spike-and-Slab####
l0=l0_MOM
l1=3*(l1_MOM)

#####Plots Non-local####
#####Normal-pMoM####
Spike<- dnorm(seq,0,sqrt(l0))
Slab<-dnorm(seq,0,sqrt(l1))
SlabpMoM<-dmom(seq,tau=l1_MOM)

plot(seq, Spike, type="l", xlab="" , ylab="", lwd=2,
     main="Prior Normal-spike",cex.main=2)
mtext(text = expression(m[jk]),
      side = 1,line = 2, cex=1.25)
mtext(text = expression(paste("P(",m[jk],"|.)")),
      side = 2,line = 2, cex=1.25)
lines(seq, SlabpMoM,col="indianred3", lwd=2)
lines(seq, Slab,lty=3, col="darkblue", lwd=2)
