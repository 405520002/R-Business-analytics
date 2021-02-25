cost=10#每件衣服的價錢
before_price=25#比賽開打前衣服的售價
win_price=before_price#雄鷹贏球衣服的售價
lose_price=12.5#雄鷹輸球衣服的售價
n=1000#模擬1000場比賽


win_prob=0.4#雄鷹贏球機率

# mean 2000 and standard deviation of 1000
#ex=shape*scale,varx=shape*scale^2
scale=(1000**2)/2000#gamma的scale參數計算(var/mean)
sh=2000/scale#gamma的shape參數計算(mean/scale)



supply=seq(from=1000,to=50000,by=200)#模擬supply(1000~100000，增量1000為一單位)
profit_one<-c()#存放一次模擬的不同supply情況下的所有profit
demand_one<-c()
profit_table<-data.frame(matrix(ncol = length(supply), nrow = 0))#建立一個空的dataframe，存放10000次模擬結果
game=sample(c(1,0),n,replace=TRUE,prob=c(win_prob,1-win_prob))#模擬10000次比賽
demand_table=data.frame(matrix(ncol = length(supply), nrow = 0))
for (i in c(1:1000)){
  before_demand=rnorm(1,9000,2000)#比賽前的demand
  if(game[i]==1){  #贏球
    demand=rnorm(1,6000,2000)#模擬贏球後球衣需求
    
  }
  else{ #輸球
    demand=rgamma(1,shape=sh,scale=scale)#模擬輸球後球衣需求
    
    
  }
  for(s in supply){#模擬不同supply下之所有獲利情況
    if(game[i]==1){
      profit=min(s,before_demand)*(before_price-cost)+max(0,min(s-before_demand,demand))*(win_price-cost)-(s-before_demand-max(0,min(s-before_demand,demand)))*cost
      #獲利=min(生產量,比賽前的需求)*比賽前單件球衣利潤+max(0,min(比賽後僅存庫存,比賽贏球後需求))-比賽結束後沒賣掉的衣服*成本
      
    }
    
    else{
      
      profit=min(s,before_demand)*(before_price-cost)+max(0,min(s-before_demand,demand))*(lose_price-cost)-(s-before_demand-max(0,min(s-before_demand,demand)))*cost
      #獲利=min(生產量,比賽前的需求)*比賽前單件球衣利潤+max(0,min(比賽後僅存庫存,比賽輸球後需求))-比賽結束後沒賣掉的衣服*成本
    }
    d=before_demand+demand
    profit_one=c(profit_one,profit)#加入該需求模擬一次比賽後的獲利
    
    
    
  }
total=demand+before_demand#比賽前+比賽後的需求
demand_one=c(before_demand,demand,total)#加入一次模擬的:比賽前、比賽後、和總需求
profit_table=rbind(profit_table, profit_one)#加入獲利表
demand_table<-rbind(demand_table,demand_one)#加入需求表
profit_one<-c()
demand_one<-c()
#清空暫存區
}



profitmean=colMeans(profit_table)#求出各供給下的平均收益

library(ggplot2)

#(1)畫圖
mean<-data.frame(x = supply, y = profitmean)

ggplot(data=mean,mapping = aes(x = supply, y = profitmean))+geom_line(colour="blue")+geom_point(x=supply[which.max(profitmean)],y=max(profitmean),colour="red")



#(2)
cvar10<-c()
#把每個supply下的所有資料取出來然後求cvar10
for(i in 1:length(supply)){
data=profit_table[,i]
q=quantile(data, probs =0.1)
m=mean(data[data<=q])
cvar10<-c(cvar10,m)

}

#(2)畫圖
cvar10_p<-data.frame(x = supply, y = cvar10)
ggplot(data=cvar10_p,aes(x = supply, y = cvar10))+geom_line(colour="red")+geom_point(x=supply[which.max(cvar10)],y=max(cvar10),colour="black")


#(3)不管雄鷹是否贏球都已滿足贏球或輸球的最大服務水準去算，最大服務水準:滿足贏球的市場需求
win_threshold=(win_price-cost)/((win_price-cost)+cost)#贏球的cu/(cu+co)=0.6
#把每個demand下的所有資料取出來然後求小於prob=0.6 quantile的所有值平均

q=quantile(demand_table[,3],prob=win_threshold)#求total demand<0.6 的quantile值
perfect_profit<-c()
perfect_profit_mean<-c()

#試算profit
  for(i in 1:n){
    before_demand=demand_table[i,1]
    after_demand=demand_table[i,2]
    total_demand=demand_table[i,3]
    if(total_demand<q){#如果那一次模擬出來的total_demand<q
      if(game[i]==0){
      profit=min(q,before_demand)*(before_price-cost)+max(0,min(s-before_demand,after_demand))*(lose_price-cost)-(q-before_demand-max(0,min(q-before_demand,after_demand)))*cost
      perfect_profit<-c(perfect_profit,profit)
      }
      else{
        profit=min(s,before_demand)*(before_price-cost)+max(0,min(q-before_demand,after_demand))*(win_price-cost)-(q-before_demand-max(0,min(q-before_demand,after_demand)))*cost
        perfect_profit<-c(perfect_profit,profit)
      }
    }
  }
mean(perfect_profit)


#1+2+3總圖
ggplot()+geom_line(aes(supply,profitmean),colour="blue")+geom_line(aes(supply,cvar10),colour="red")
#print(1,2,3題最佳生產量和profit)
cat("optimal_profit of (1):",round(max(profitmean)),"optimal_supply of (1): ",supply[which.max(profitmean)])
cat("optimal_profit of (2):",round(max(cvar10)),"optimal_supply of (2): ",supply[which.max(cvar10)])
cat("optimal_profit of (3):",round(mean(perfect_profit)),"optimal_supply of (3): ",round(q))


 





  











