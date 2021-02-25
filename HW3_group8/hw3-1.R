catch_fish=10000#模擬1000次出海捕魚
boat_capacity=3800#滿船容量
cost=7200#operation cost
corrgr=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)#correlation table

strategy_table<-data.frame(matrix(ncol = 4, nrow = 0))#設一個空的策略表，將來會存放s1~s4
x <- c("s1", "s2", "s3","s4")#命名
colnames(strategy_table) <- x


#模擬Rick和morty去抓魚
R_catch=runif(catch_fish, min = 0.6*boat_capacity, max = 0.9*boat_capacity)
M_catch=rtri(catch_fish,0.5*boat_capacity,1*boat_capacity,0.75*boat_capacity)

#g和r市場的需求
dg=rtri(catch_fish,4000,8000,7000) 
dr=rtri(catch_fish,4800,7200,6300)

library(MASS)


for (cor in corrgr){
covxy=cor*0.35*0.25 #算出g市場和r市場價格的共變異數
sigma <- matrix(c(0.35**2,covxy,covxy,0.25**2), 2, 2)#製作matrix
mean <- c(3.5,3.65)
p <- mvrnorm(catch_fish, mu = mean, Sigma = sigma)#模擬兩市場的價格
price_g=p[,1]#擷取g市場的價格
price_r=p[,2]#擷取R市場的價格
for (i in c(1:10000)){
  #模擬strategy的獲利
  strategy1=min(R_catch[i],dg[i])*price_g[i]-7200+min(M_catch[i],dr[i])*price_r[i]-7200
  #R船員進入G市場,M船員進入R市場
  strategy2=min(R_catch[i],dr[i])*price_r[i]-7200+min(M_catch[i],dg[i])*price_g[i]-7200
  #R船員進入R市場,M船員進入G市場
  strategy3=min(R_catch[i]+M_catch[i],dg[i])*price_g[i]-7200-7200
  #R船員進入G市場,M船員進入G市場
  strategy4=min(R_catch[i]+M_catch[i],dr[i])*price_r[i]-7200-7200
  #R船員進入R市場,M船員進入R市場
  strategy=c(strategy1,strategy2,strategy3,strategy4)#加入空的table
  strategy_table=rbind(strategy_table, strategy)
  
  
}



}
print("finish")



draw_table<-data.frame()#儲存要畫圖的資料
cor_index=c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000)#因為1-10000是corr=-0.8的answer，10001~20000是
                                                                    #corr=-0.6的answer，所以紀錄要取出的index範圍

#(1)
for (i in 1:4){#巡過s1~s4
  print(i) 
  s=strategy_table[i]
  for(j in 1:(length(cor_index)-1)){
    #取出各corr代表的資料再取平均
    m=sum(s[(cor_index[j]+1):cor_index[j+1],])/10000
    draw_table=rbind(draw_table, m)
  }
  
}

s1=draw_table[1:9,]

s2=draw_table[10:18,]

s3=draw_table[19:27,]

s4=draw_table[28:36,]

#第一題畫圖程式
corrgr=c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
library(ggplot2)
ggplot()+geom_line(aes(corrgr,s1),colour="blue")+geom_line(aes(corrgr,s2),colour="green")+geom_line(aes(corrgr,s3),colour="black")+geom_line(aes(corrgr,s4),colour="red")


#(2)
draw_table<-data.frame()
for (i in 1:4){
  print(i) 
  s=strategy_table[i]
  for(j in 1:(length(cor_index)-1)){
    a=s[(cor_index[j]+1):cor_index[j+1],]
    q=quantile(a, probs =0.05)#取出q cvar5%
    a=a[a<=q]#取出小於q的所有值
    m=mean(a)#取平均
    draw_table=rbind(draw_table, m)
  }
  
}

s1=draw_table[1:9,]

s2=draw_table[10:18,]

s3=draw_table[19:27,]

s4=draw_table[28:36,]

ggplot()+geom_line(aes(corrgr,s1),colour="blue")+geom_line(aes(corrgr,s2),colour="green")+geom_line(aes(corrgr,s3),colour="black")+geom_line(aes(corrgr,s4),colour="red")





