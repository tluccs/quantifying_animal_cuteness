halfnormal=function(y, label=F, ...)
{
  n=length(y)
  x=seq(0.5+0.25/n, 1.0-0.25/n, by=0.5/n)
  x = qnorm(x)
  y = sort(abs(y))
  qqplot(x, y, xlab="half-normal quantiles", ylab="absolute effects", ...)
  if(label) text(x,y, names(y))
}


dat=read.table("animal2.dat", h=T, sep=",")

A = dat$Adult 
B = dat$Dog 
C = dat$Sleep 
D = dat$Paw 
E = dat$Set 

#get mean
ybar =  (apply(dat[,6:17], 1, mean)  )
ybar

#log of variance, if needed
lns2 = log( apply(dat[,6:17], 1, var) )
lns2

### main effects model
g=lm(ybar~ (A+B+C+D+E) ) 
#full model using main and 2fi's
#g=lm(ybar~ (A+B+C+B:C+A:C) ) 
round(2*g$coef[-1],3) #*2 for factorial effects
summary(g)
halfnormal(2*g$coef[-1], label=T) # remove intercept
g=lm(ybar~ (A+C) ) #using only main effects
summary(g)
plot(g, 1:2) #residuals

xlist = c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)

#Transform all data to have 0 mean and 1 variance,
#reduce impact of individual + find out what is relatively good/bad
dat=read.table("animal2.dat", h=T, sep=",")
for (val in xlist) {
   tmp = dat[, val] -mean(dat[, val])
   dat[, val] = ((tmp/(var(tmp))^0.5)   )
  
}

A = dat$Adult 
B = dat$Dog 
C = dat$Sleep 
D = dat$Paw 
E = dat$Set 
#mean
ybar =  (apply(dat[,6:17], 1, mean)  )
ybar
#log var, not used here
lns2 = log( apply(dat[,6:17], 1, var) )
lns2


#Main effects model with normalised data
#g=lm(ybar~ (A+B+C+D+E) ) 

#Main effects and 2,3fi's with normalised data
g=lm(ybar~ (A+B+C+D+E)^3 ) 
halfnormal(2*g$coef[-1], label=T) # remove intercept

#Full model with 2fi's
#g=lm(ybar~ (A+B+C+B:C+A:C) ) 
round(2*g$coef[-1],3)
summary(g)
halfnormal(2*g$coef[-1], label=T) # remove intercept
plot(g, 1:2)


