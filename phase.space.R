## this block calculates solutions for many K's, it should take some time
KK = seq(from = 1 ,to=20, by=0.1)
rminmax = matrix(NA, ncol=2, nrow=length(KK))#resource minimum and maximum
cminmax = matrix(NA, ncol=2, nrow=length(KK))#consumer minimux ans maximum

## Loop over all values of K andd get min and max population sizes
for(i in 1:length(KK)){
  parmsi =  c(c=.05, #conversion assimilation from eggs to tucan
               d=.001,#conversion assimilation from seeds to tucan
               b=0.0001, # rate of ingestion tucans to eggs
               he=5, # handling time of tucans to eggs
               gamma=1/120, #1/120, # 1/gamma mean time og the eggs tour into adults
               alpha=5, #
               r=1,#dispersal efficiency
               s=0.005, # bird-death rate of tucans
               ht=.1,
               kp=1000,
               omega=.01,#death rate of macows
               beta=5, # rate(?) of eggs per net
               n=1/23000,#
               m=1/23000,#death rate of trees
               k=1/50)## porportional to the number of trees tha t can be growth
  
  
  
  y0 = c(M=100,E=25,TR=1000,P=KK[i],Y=20)
  out3 = ode(y=y0, times = time <- seq(0, 1000, by = 10), func = Our.Model, parms = parmsi)
  rminmax[i,] = range(out3[(nrow(out3)-101):nrow(out3),2])
  cminmax[i,] = range(out3[(nrow(out3)-101):nrow(out3),3])
}

resultados8<-matrix(nrow=length(KK),ncol=3)
resultados8[,1]<-as.matrix(KK)
resultados8[,2]<-as.matrix(rminmax[,2])
resultados8[,3]<-as.matrix((rep(8,length(KK))))


  plot(resultados8)
