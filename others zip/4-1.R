demand=function(p,a=200,b=35){
  e=rnorm(1,0,20)
  return (a-b*p+e)
}

demand_exp=function(p,a=200,b=35){
  e=rexp(1,10)
  return (a-b*p+e)
}


profit.function=function(x){
  S=5000
  p=x[1]
  v=x[2]
  s=x[3] 
  c=x[4]
  q=x[5]
  quantity=profit=price=rep(0,S)
  for(i in 1:S){
    d=demand(p)
    profit[i]=p*min(q,d)-c*q+v*max(q-d,0)-max(d-q,0)
  }
  return(mean(profit))
  
}

#demand e using exp
profit.function.exp=function(x){
  S=5000
  p=x[1]
  v=x[2]
  s=x[3] 
  c=x[4]
  q=x[5]
  quantity=profit=price=rep(0,S)
  for(i in 1:S){
    d=demand_exp(p)
    profit[i]=p*min(q,d)-c*q+v*max(q-d,0)-max(d-q,0)
  }
  return(mean(profit))
  
}


#a,b,c
#1.hookie jeeves
library(dfoptim)
library(pracma)
res1=fminsearch(profit.function,
                c(3,0.5,1,1,30),
               lower=c(1,0.5,1,1,10), 
               upper=c(5.5,0.5,1,1,200),
               method=c("Hooke-Jeeves"),
               minimize=FALSE)

res=hjkb(par=c(3,0.5,1,1,30),fn=profit.function,
           lower=c(1,0.5,1,1,10), upper=c(5.5,0.5,1,1,200),
           control=list(maxfeval=2000000, 
                        maximize=TRUE, 
                        info=TRUE))

#2.GA
library(GA)
library(MASS)

ga_res = ga(type="real-valued", 
            function(x){profit.function(x)}, 
            names = c("price",'v','c','s',"quantity"),
            lower = c(1,0.5,1,1,10),
            upper=c(5.5,0.5,1,1,200), 
            maxiter = 50000,
            run=50, 
            parallel=TRUE,# Exploit multi-core properties of your CPU
            monitor=TRUE,
            seed=1)

#abc print answer
a=c(res1[1])
f=c(res[2])
cat("if search by hookie jeeves:\n the best price : ",a$xmin[1],'\n the best quantity :'
    ,a$xmin[5],"\n profit :",f$value)
cat("if search by GA:\n the best price : ",ga_res@solution[1],'\n the best quantity :'
    ,ga_res@solution[5],"\n profit :", ga_res@fitnessValue)


#d change to exp


#1.hookie jeeves exp

res1_exp=fminsearch(profit.function.exp,
                c(3,0.5,1,1,30),
                lower=c(1,0.5,1,1,10), 
                upper=c(5.5,0.5,1,1,200),
                method=c("Hooke-Jeeves"),
                minimize=FALSE)

res_exp=hjkb(par=c(3,0.5,1,1,30),fn=profit.function.exp,
         lower=c(1,0.5,1,1,10), upper=c(5.5,0.5,1,1,200),
         control=list(maxfeval=2000000, 
                      maximize=TRUE, 
                      info=TRUE))

#2.GA


ga_res_exp = ga(type="real-valued", 
            function(x){profit.function.exp(x)}, 
            names = c("price",'v','c','s',"quantity"),
            lower = c(1,0.5,1,1,10),
            upper=c(5.5,0.5,1,1,200), 
            maxiter = 50000,
            run=50, 
            parallel=TRUE,# Exploit multi-core properties of your CPU
            monitor=TRUE,
            seed=1)

#abc print answer
a=c(res1_exp[1])
f=c(res_exp[2])
cat("if search by hookie jeeves (with e in exp):\n the best price : ",a$xmin[1],'\n the best quantity :'
    ,a$xmin[5],"\n profit :",f$value)
cat("if search by GA (with e in exp):\n the best price : ",ga_res_exp@solution[1],'\n the best quantity :'
    ,ga_res_exp@solution[5],"\n profit :", ga_res_exp@fitnessValue)






