h<-c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)#每個月對應的歷史damand
month<-c("no_4","no_5","no_6",7,8,9,10,11,12,1,2,3,4,5,6)#月份(因為p0是四月的，公司七月才會有人進來，所以今年4,5,6設成no_4...，範圍是到明年六月)
s=300#模擬次數
x_name <- "hi"
y_name <- "month"

historic<-data.frame(h,month)#存成dataframe
names(historic)<-c(x_name,y_name)
ec_growth=rnorm(s,0,0.05)

#demandi=hi(1+eco)(1+noise)
q7=seq(11,110,by=1)#模擬七月要顧多少人的範圍

#r91=runif(1,0.8,1)一月九月均勻分配
#r5678=runif(1,0.8,1)五到八月均勻分配
#r234101112=runif(1,0.9,1)十月到四月(不含一月)均勻分配

p=63#四月的人
#pi+1=pi*ri+ai+1(supply)
r_vector=c()#存放一整年(4~隔年6月)該q的ri，假設r會隨雇用人數不同而有不同結果，所以r之後會模擬s*length(d)*length(month)次
p_vector=c()#存放一整年(4~隔年6月)該q的pi
d_vector<-c()#存放一整年(4~隔年6月)該q的di
p_total<-c()#存放所有的pi
d_total<-c()#存放所有的di(在每個模擬的次數裡面不同q的每個月demand相同)
noise=rnorm(length(month)*s,0,0.1)  #15*1000: 模擬1000次


#a
n=-length(month)+1   # 初始n=-14，為了抓取noise值的index
for(i in c(1:s)){
  n=n+length(month)  # 初始n=1
  for(d in q7){
    p=63
    a=0
    for(m in month){ 
      hi=historic[which(historic['month']==m),1]  #該月的歷史需求
      demand=hi*(1+ec_growth[i])*(1+noise[n])
      n=n+1
      d_vector<-c(d_vector,demand)
      #如果月份是今年4,5,6月人數只會減少pi*r
      if(m=="no_4"){
        p_vector=c(p_vector,p)
        r=runif(1,0.9,1)
        r_vector=c(1)
        
      }
      else if(m=="no_5"){
        r_vector=c(r_vector,r)
        p=p*r
        r=runif(1,0.8,1)
        p_vector=c(p_vector,p)
        
      }
      else if(m=="no_6"){
        r_vector=c(r_vector,r)
        p=p*r
        r=runif(1,0.8,1)
        p_vector=c(p_vector,p)
      }
      #如果月份是七月人數會增加，其他月則不會
      #人數=pi*ri+ai+1(supply)
      
      else{
        
        if(m==9||m==1){
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
          a=rbinom(1,d,0.7)#模擬員工accept offer的人數
          
          
        }
        
        else if(m==2||m==3||m==4||m==10||m==11||m==12){
          r=runif(1,0.9,1)
          r_vector=c(r_vector,r)
        }
        
        index=which(month %in% m)-1#取上個月的index
        supply=a+p_vector[index]*r_vector[index]
        a=0
        p_vector=c(p_vector,supply)

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
      if(pi>di){
        e=10000*di-6000*pi
        
      }
      else{
        e=(10000-6000)*pi+(10000-9600)*(di-pi)
      }
      e_simulate=e_simulate+e
      
    }
    e_mean=e_simulate/s
    e_total=c(e_total,e_mean)
    
  }
}

dim(e_total)=c(length(q7),length(month))
e_table=as.data.frame(e_total)
sum_earnings=rowSums(e_table[4:15])
best_q=q7[which.max(sum_earnings)]
best_e=max(sum_earnings)
cat("the best q is: ",best_q)
cat("the best e is: ",best_e)



