install.packages(c("deSolve", "ggplot2", "reshape2"))
library(ggplot2)

# time sequence 
time <- seq(0, 50, by = 0.01)

# parameters: a named vector
parameters <- c(c = 0.1,
                
                d = 0.1,
                
                a = 0.2,
                
                B = 5,
                
                b = 0.53,
                
                hE = 0.001,
                
                y = 0.8,
                
                w = 0.01,
                
                r = 0.2,
                
                alfa = 0.83,
                
                KT = 50 ,
                
                KP = 10,
                
                S = 0.6,
                
                hT = 0.1)

# initial condition: a named vector
state <- c(M = 2, P = 3, G = 10, E=2)
state <- c(M = 10, P = 1, G = 10, E=2)


# R function to calculate the value of the derivatives at each time value
# Use the names of the variables as defined in the vectors above
lotkaVolterra <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    dE<- (B*M*a*G/M+a*G)-(b*P*E)/(1+b*hE-y*E)
    dM<- y*E - w*M
    dG<- (r*alfa*P*G)*(1-(G/KT))
    dP<- (c*(b*P*E)/(1+b*hE) + ((d*P*G/1+hT) + S*P)) * (1-(P/KP))
    
    
    return(list(c(dE, dM,dG,dP)))
  })
}
## Integration with 'ode'
out <- ode(y = state, times = time, func = lotkaVolterra, parms = parameters)

## Ploting
out.df = as.data.frame(out) # required by ggplot: data object must be a data frame
library(reshape2)
out.m = melt(out.df, id.vars='time') # this makes plotting easier by puting all variables in a single column

p <- ggplot(out.m, aes(time, value, color = variable)) + geom_point()
print(p)
