library(deSolve)
library(ggplot2)
library(reshape2)

time <- seq(0, 1000, by = 1)
state <- c(M=100,E=25,TR=1000,P=80,Y=20)


rm(out.m)
parameterslist<-list()
list.of.results<-list()
list3<-list()
#sample(seq(20, 200, by = 10),size=1))
#sample(seq(0, 200, by = 10),size=1)

for(i in 1:50){

  parameters <- c(c=.05,#convertion rate of eggs
                  d=.001,#convercio rate of fruit
                  b=.1,#death rate off eggs by tucans
                  he=1,#handling time eggs by tucans
                  gamma=1/10,#rate of eggs becoming adults
                  alpha=.1,#dispersal of trees by tucans
                  r=.8,#dispersal efficiency
                  s=.5,#groth rate by tucans
                  ht=.1,#handling time fruit by tucans
                  kp=150,#carryng capacity tucans
                  omega=.1,#death rate of macows
                  beta=2, # rate(?) of eggs per net
                  n=1/(21300),#
                  m=.0001,#death rate of trees
                  k=10)# carryng capacity trees 20
  
  
parameterslist[[i]]<-parameters

Our.Model <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dE = ((beta*TR*M)/ (TR+M))  - (b*P*E)/(1+b*he*E)- gamma*E
    dM = gamma*E-omega*M
    dTR = -m*TR+n*Y
    dP = s*P*(1-P/kp)+(c*b*P*E)/(1+b*he*E)+(d*P*TR)/(1+ht*TR)
    dY = r*alpha*P*TR/(1+ht*TR)*(1/(1+(Y+TR)/k))-n*Y
    return(list(c(dM, dE,dTR,dP,dY)))
  })
}

out <- ode(y = state, times = time, func = Our.Model, parms = parameters)

## Ploting
out.df = as.data.frame(out) # required by ggplot: data object must be a data frame

out.m = melt(out.df, id.vars='time') # this makes plotting easier by puting all variables in a single column

list.of.results[[i]] = melt(out.df, id.vars='time')
out.m$log<-log10(out.m$value+1)
p <- ggplot(out.m, aes(time, log, color = variable)) + geom_point()
p


#plot(out.m$time,out.m$value,col=out.m$variable,pch=20)#,log='y')
#legend('topright', legend=c('E','TR','E','TR'),
     #  col=as.factor(levels(out.m$variable)), pch=20, cex=1)
#mtext(paste(i),at=1)

list3[[i]]<-subset(list.of.results[[i]]$value,list.of.results[[i]]$variable=='M')
}
#parameterslist[[22]]

plot(unlist(list3),type='l')


#### Bifurcation Diagram #####






## this block calculates solutions for many K's, it should take some time
KK = seq(from = 10, to=100, by=5)
rminmax = matrix(NA, ncol=2, nrow=length(KK))#resource minimum and maximum
cminmax = matrix(NA, ncol=2, nrow=length(KK))#consumer minimux ans maximum

## Loop over all values of K andd get min and max population sizes
for(i in 1:length(KK)){
  parmsi = c(c=.05,#convertion rate of eggs
             d=.001,#convercio rate of fruit
             b=0.1,#death rate off eggs by tucans
             he=1,#handling time eggs by tucans
             gamma=1/10,#rate of eggs becoming adults
             alpha=.1,#dispersal of trees by tucans
             r=.8,#dispersal efficiency
             s=.5,#groth rate by tucans
             ht=.1,#handling time fruit by tucans
             kp=150,#carryng capacity tucans
             omega=.1,#death rate of macows
             beta=2, # rate(?) of eggs per net
             n=1/(21300),#
             m=.0001,#death rate of trees
             k=10)# carryng capacity trees 20
  
  
  
  y0 = c(M=100,E=25,TR=1000,P=80,Y=20)
  out3 = ode(y=y0, times = time <- seq(0, 1000, by = 10), func = Our.Model, parms = parmsi)
  rminmax[i,] = range(out3[(nrow(out3)-101):nrow(out3),2])
  cminmax[i,] = range(out3[(nrow(out3)-101):nrow(out3),3])
}
plot(x=KK, y=rminmax[,2], type="l", lwd=2, col="blue", 
     xlab="Tucans", ylab="Max population")
























points(x=KK, y=rminmax[,2], type="l", lwd=2, col="blue")
points(x=KK, y=cminmax[,1], type="l", lwd=2, col="darkgreen",ylim=range(rminmax))
points(x=KK, y=cminmax[,2], type="l", lwd=2, col="darkgreen",ylim=range(rminmax))

out.df = as.data.frame(out3) # required by ggplot: data object must be a data frame

out.m = melt(out.df, id.vars='time') # this makes plotting easier by puting all variables in a single column

list.of.results[[i]] = melt(out.df, id.vars='time')
out.m$log<-log10(out.m$value+1)
p <- ggplot(out.m, aes(time, log, color = variable)) + geom_point()
p

