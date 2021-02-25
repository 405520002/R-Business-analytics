h<-c(105,95,75,70,70,110,105,90,65,80,90,120,105,95,75)
month<-c("no_4","no_5","no_6",7,8,9,10,11,12,1,2,3,4,5,6)
s=100
x_name <- "hi"
y_name <- "month"
historic<-data.frame(h,month)
names(historic)<-c(x_name,y_name)
ec_growth=rnorm(s,0,0.05)
#demandi=hi(1+eco)(1+noise)
q7=seq(11,110,by=1)
r91=runif(1,0.8,1)
r5678=runif(1,0.8,1)
r234101112=runif(1,0.9,1)

p=63
#pi+1=pi*ri+ai+1(supply)
norm_contri=4000
trans_contri=400
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
  for(d in q7){
    d=round(d/2,0)
    half=d
    p=63
    a=0
    for(m in month){
      print(d)
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
      
      else{
        
        if(m==1){
          r=runif(1,0.8,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==5||m==6||m==7||m==8){
          r=runif(1,0.95,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==2||m==3||m==4||m==10||m==11||m==12){
          r=runif(1,0.9,1)
          r_vector=c(r_vector,r)
        }
        
        else if(m==9){
          r=runif(1,0.8,1)
          r_vector=c(r_vector,r)
          frac=runif(1,0.7,1)
          d=d+half*round(frac,digits=0)
          
          
          
        }
        
        index=which(month %in% m)-1
        ri=r_vector[index]
        d=d-a
        a=rbinom(1,max(d,0),0.7)
        supply=a+p_vector[index]*r_vector[index]
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



