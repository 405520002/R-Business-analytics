
#3


#3-1
r_cost=11
set.seed(220)
glr=function(){
  n=rnorm(1, mean = 3000, sd = 1000)
  mp=sample(c(20,18.5,16.5,15),
            1,replace=T,
            prob=c(0.25,0.35,0.30,0.10))
  cost=sample(seq(5040, 6860, 1), 1)
  revenue = n * (mp-r_cost) - 3995 - cost
  return(c(revenue))
}

S = 10000
income= replicate(S,glr())

mean(income)
c=80000/12
c
income 



#3-2

partnership = rep(NA, S)
for (i in 1:length(income)) {
  if (income[i] < 3500) {
    partnership[i] = 3500
  }
  else if (income[i] >= 3500 & income[i] <= 9000) {
    partnership[i] = income[i]
  }
  else {
    partnership[i] = 0.1 * income[i]
  }
}

mean(partnership)


###3-2-2
partnership_r=function(){
  n=rnorm(1, mean = 3000, sd = 1000)
  mp=sample(c(20,18.5,16.5,15),
            1,replace=T,
            prob=c(0.25,0.35,0.30,0.10))
  cost=sample(seq(5040, 6860, 1), 1)
  revenue = n * (mp-r_cost)- 3995 - cost
  partnership=c()
  if(revenue<3500){partnership=3500}
  else if(revenue>=3500 & revenue<=9000){partnership=revenue}
  else{partnership=revenue*0.1}
  return(partnership)
}

S = 10000
partnership_r= replicate(S,partnership_r())

mean(partnership_r)



#3-3

sum(income>80000/12)/S
sum(partnership_r>80000/12)/S






