#相關變數名稱請見書面檔案
A=690000
B=5680000
C=0.005
D=0.2
E=0.1
F_loss=0.001
H=0.02
J=0.02
d_again=0
d_1=0.01
d_7=0.07
d_9=0.09
L_again=0.5
L_1=0.35
L_7=0.1
L_9=0.5
M=0.1
N=0.6
O=0.025
R=0.09
P=200
Q=325
a=150
b=50000000#將app開發成本以及市場推廣成本合在一起計算
c=100000
e=0.0179
f_first=0.01
f_together=0.0001
g=600
h=4.72
i=0.0075
j=seq(0.02,0.045,by=0.001)
G=function(j){#回饋對應成長率方程式
  g1=0.7*j+0.0034
  return(g1/12)
}
J=function(j){#回饋對應優惠方程式
  g1=-0.05*j+0.051
  return(g1)
}
k=0.0179/30
l=0.0179/30*8
m=1200
n=2566000
o=43333
p=16.35
q=0.02
r=200
t=0.08
u1=0.25
v1=0.5
x=200000
marketing=100000#每月行銷成本支出
library(EnvStats)


z=function(k){
  return (1+e)^k
}#超過十萬元的回饋上限成長


num=24



profit=function(num){
profit_table=c() 

  v=rnorm(num,6500,3000)
  v[which(v<0)]=500#因為常態分配可能取道負的，所以設下限為500
  u=rnorm(num,4215,2000)
  u[which(u<0)]=500
  K=rtri(num,0,90,60)
  s=rtri(num,3000/12,40000/12,10000/12)
  L=sample(c(d_1,d_7,d_9,d_again),num,prob=c(L_1,L_7,L_9,L_again),replace=TRUE)
  I=runif(num,0.03,0.07)

  
  max_customer1=A*D#主要客群上限
  max_customer2=B*C#非主要客群上限

for (rate in j){ #跑過每一個rate
  profit=0
  customer1=0
  customer2=0
  revenue=0
  add_revenue=0#當月增加的收益
  expo1=sort(rexp(num,1/((A*D)/24)),decreasing = TRUE)#來客數呈現指數分配遞減，因此先模擬後進行排序
  expo2=sort(rexp(num,1/((B*C)/24)),decreasing = TRUE)
  exp_app=sort(rexp(num,1/(G(rate)/12)),decreasing = TRUE)
  #模擬num個月的收益
  for(month in c(1:num)){ 
    customer1=min(max_customer1,round(customer1+expo1[month]))
    customer2=min(max_customer2,round(customer2+expo2[month]))
    customer=min(max_customer1+max_customer2,customer1+customer2)
    circular_revenue=customer*p
    loss_revenue=rbinom(1,customer,F_loss)*r
    transaction=customer2*q*v[month]+customer1*q*u[month]
    period=rbinom(1,customer,H)*s[month]*t
    add_revenue=add_revenue+circular_revenue+loss_revenue+transaction+period
    q1=(1+exp_app[month]/12+I[month]/12)^month
    
    revenue=revenue+(add_revenue)*q1+x
    
  }

  
  
  customer1=0
  customer2=0
  cost=0
  #模擬num個月的支出
  for(month in c(1:num)){
    customer1=min(max_customer1,round(customer1+expo1[month]))
    customer2=min(max_customer2,round(customer2+expo2[month]))
    customer=min((max_customer1+max_customer2),customer1+customer2)
    card=expo1[month]*a
    bad=customer*h
    rollgame_person=round(rbinom(1,customer1,N))*P*L[month]+round(rbinom(1,customer2,N))*Q*L[month]
    rollgame=rbinom(1,round(rollgame_person),M)
    cus100000=customer*O*min(1200,600*z(month))
    general=customer1*u[month]*u1*i+customer2*v[month]*v1*i
    special=customer1*min(u[month]*(1-u1)*j,m)+customer2*min(v[month]*(1-v1)*j,m)
    holiday=(u[month]+v[month])*R*((l+i)+(k+i))*customer
    together=(K[month]*f_together)*(customer1*u[month]*(1-u1)+customer2*v[month]*(1-v1))
    cost=cost+card+bad+rollgame+cus100000+general+special+holiday+together+c
    cost=cost*(1+J(rate))+marketing
    
  }
  cost=cost+b

  
  profit=(revenue-cost)
  profit_table=c(profit_table,profit)
}
return(profit_table)
  
}

profit(1)





ans=profit(12)
sim_times=1000
final_month=1

#print出在不同回饋下的所有值
pri=function(mp,month){
  for(i in c(1:length(j))){
    cat("回饋",j[i],"時第",month,'個月時獲利',mp[i],"元\n")
  }
}

#把所有回饋存在table中
total_profit_table=data.frame(matrix(NA, ncol = length(j)))
colnames(total_profit_table) <- j
return_back=0
final_month=1

#在每個月分下都模擬1000次profit function
for(month in c(final_month:24)){
  tp=rep(0,length(j))
  for(times in c(1:sim_times)){
    tp=tp+profit(month)
  }
  mp=tp/sim_times
  total_profit_table=rbind(total_profit_table,mp)
  pri(mp,month)
  
  if(length(which(mp>0))>0 && return_back==0){
    cat("在回饋等於",j[which(mp>0)],"時在第",month,"個月可以最快回本\n")
    cat('在回本時賺最多者為',j[which.max(mp)],'賺',max(mp),'元')
    return_back=1
  }
}


#plot image
total_profit_table=total_profit_table[-1,]
month_times=seq(1,24,by=1)
plot(month_times,total_profit_table$"0.021" ,col='green')
plot(month_times,total_profit_table$"0.031",col='blue')
plot(month_times,total_profit_table$"0.044",col='red',ylim =c(-20000000,2000000000) )
plot(month_times,total_profit_table$"0.045",col='orange',ylim =c(-20000000,2000000000) )
lines(month_times,total_profit_table$"0.021" ,col='green')
lines(month_times,total_profit_table$"0.031",col='blue')
lines(month_times,total_profit_table$"0.044",col='red')
lines(month_times,total_profit_table$"0.045",col='orange')
abline(h = 0, col = "black")
legend("topleft", legend=c("0.021", "0.031",'0.044','0.045'),
       col=c("green", "blue",'red',"orange"), lty=1:2, cex=0.8)



  