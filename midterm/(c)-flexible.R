
#同第二題註解，第三題q7=flexible只對增加的部分作註解
h<-c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
month<-c("no_4","no_5","no_6",7,8,9,10,11,12,1,2,3,4,5,6)
s=15
x_name <- "hi"
y_name <- "month"

historic<-data.frame(h,month)
names(historic)<-c(x_name,y_name)
ec_growth=rnorm(s,0,0.05)

#demandi=hi(1+eco)(1+noise)
q7=seq(11,110,by=1)
q12=seq(11,110,by=1)
q.grid=expand.grid(q7,q12)#創一個q7,q12的排列組合ｔａｂｌｅ

p=63

r_vector=c()
p_vector=c()
d_vector<-c()
p_total<-c()
d_total<-c()
noise=rnorm(length(month)*s,0,0.1)


#a
n=-length(month)+1
for(i in c(1:s)){
  n=n+length(month)
  for(d in c(1:10000)){#跑過所有排列組合有length(q7)*length(q12)=10000筆
    q7m=q.grid[d,2]#抓q7的值
    q12m=q.grid[d,1]#抓q12的值
    q=round(q7m/2,0)#七月來的只有一半
    half=q
    p=63
    a=0
    for(m in month){
      hi=historic[which(historic['month']==m),1]
      demand=hi*(1+ec_growth[i])*(1+noise[n])
      n=n+1
      d_vector<-c(d_vector,demand)
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
      
      else{#開始計算有人來上班的部分
        
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
          a=rbinom(1,q,0.7) 
          
        }
        
        else if(m==2||m==3||m==4||m==10||m==11){
          r=runif(1,0.9,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==9){
          r=runif(1,0.8,1)
          r_vector=c(r_vector,r)
          a=rbinom(1,half*round(runif(1,0.7,1),0),0.7)#七月答應九月來上班的人只有70~100趴會來
        }
        
        else if(m==12){
          r=runif(1,0.9,1)
          r_vector=c(r_vector,r)
          a=rbinom(1,q12m,0.7)#如果是12月q加入q12 accept的人
        }
        
        
        
        index=which(month %in% m)-1
        ri=r_vector[index]
        supply=a+p_vector[index]*r_vector[index]#pi+1=pi*ri+ai+1(supply)
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
dim(p_total)=c(length(month),10000,s)#變三維
dim(d_total)=c(length(month),10000,s)



e_total=c()
for(i in c(1:length(month))){
  for(j in c(1:10000)){
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

dim(e_total)=c(10000,length(month))
e_table=as.data.frame(e_total)
sum_earnings=rowSums(e_table[4:15])
best_q=q.grid[which.max(sum_earnings),]
best_e=max(sum_earnings)
cat("the best q12 is: ",best_q[1,1])
cat("the best q7 is: ",best_q[1,2])
cat("the best e is: ",best_e)



