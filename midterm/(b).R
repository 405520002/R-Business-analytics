h<-c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)#往年需求量
month<-c("no_4","no_5","no_6",7,8,9,10,11,12,1,2,3,4,5,6)#至明年的每月份
s=300
x_name <- "hi"
y_name <- "month"
historic<-data.frame(h,month)#存成dataframe
names(historic)<-c(x_name,y_name)
ec_growth=rnorm(s,0,0.05)#經濟成長率
#demandi=hi(1+eco)(1+noise)
q7=seq(11,110,by=1)#雇用人數區間

#r91=runif(1,0.8,1)一月九月均勻分配
#r5678=runif(1,0.8,1)五到八月均勻分配
#r234101112=runif(1,0.9,1)十月到四月(不含一月)均勻分配

p=63#四月時的員工人數
#pi+1=pi*ri+ai+1(supply)
norm_contri=4000
trans_contri=400
r_vector=c()#從今年4月到明年6月的留職機率ri，r隨著雇用人數不同會有不同的結果，模擬次數為: s x length(d) x length(month)
p_vector=c()#從今年4月到明年6月的最後當月員工人數
d_vector<-c()#從今年4月到明年6月的需求分析值
p_total<-c()
d_total<-c()
noise=rnorm(length(month)*s,0,0.1)#模擬1000次


#a
n=-length(month)+1
for(i in c(1:s)){
  n=n+length(month)#n從第一個月開始計算
  for(d in q7){
    d=round(d/2,0)#七月來上班的人
    half=d#九月來上班的人
    p=63
    a=0
    for(m in month){#計算歷史需求量
      hi=historic[which(historic['month']==m),1]
      demand=hi*(1+ec_growth[i])*(1+noise[n])
      n=n+1
      d_vector<-c(d_vector,demand)
      if(m=="no_4"){#今年4月後剩下多少人
        p_vector=c(p_vector,p)
        r=runif(1,0.9,1)
        r_vector=c(1)
        
      }
      else if(m=="no_5"){#今年5月後剩下多少人
        r_vector=c(r_vector,r)
        p=p*r
        r=runif(1,0.8,1)
        p_vector=c(p_vector,p)
        
      }
      else if(m=="no_6"){#今年6月後剩下多少人
        r_vector=c(r_vector,r)
        p=p*r
        r=runif(1,0.8,1)
        p_vector=c(p_vector,p)
      }
      
      else{#七月開始有人到職後每月的人數變化
        
        if(m==1){
          r=runif(1,0.8,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==5||m==6||m==8){
          r=runif(1,0.95,1)
          r_vector=c(r_vector,r)
        }
        else if(m==7){
          r=runif(1,0.95,1)
          r_vector=c(r_vector,r)
          a=rbinom(1,d,0.7) 
          
        }
        
        else if(m==2||m==3||m==4||m==10||m==11||m==12){
          r=runif(1,0.9,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==9){
          r=runif(1,0.8,1)
          r_vector=c(r_vector,r)
          frac=round(runif(1,0.7,1),0)#答應九月來上班的人，會有0.7~1的機率會不來
          a=rbinom(1,half*frac,0.7)
          
          
        }
        #每一個月的經濟成長率會受到前一個月影響   
        index=which(month %in% m)-1
        supply=a+p_vector[index]*r_vector[index]#計算人數
        a=0
        p_vector=c(p_vector,supply)#存到p_vector table
        
      }
      
    }
    d_total=c(d_total,d_vector)
    p_total=c(p_total,p_vector)
    p_vector=c()
    r_vector=c()
    d_vector=c()
    n=n-length(month)
    
    
    
  }
}
dim(p_total)=c(length(month),length(q7),s)
dim(d_total)=c(length(month),length(q7),s)



e_total=c()
for(i in c(1:length(month))){
  for(j in c(1:length(q7))){
    e_simulate=0
    for(k in c(1:s)){
      pi=p_total[i,j,k]
      di=d_total[i,j,k]
      if(pi>di){#供給人數大於需求人數
        e=10000*di-6000*pi
        
      }
      else{#供需平衡與供不應求
        e=(10000-6000)*pi+(10000-9600)*(di-pi)
      }
      e_simulate=e_simulate+e#收益最大化加總
      
    }
    e_mean=e_simulate/s#平均最大化收益
    e_total=c(e_total,e_mean)
    
  }
}

dim(e_total)=c(length(q7),length(month))
e_table=as.data.frame(e_total)#存進table中
sum_earnings=rowSums(e_table[4:15])
best_q=q7[which.max(sum_earnings)]#最佳雇用人數
best_e=max(sum_earnings)#最佳化收益
cat("the best q is: ",best_q)
cat("the best e is: ",best_e)



