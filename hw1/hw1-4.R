high_skill_do_business=function(customers=20){
  sample(c(0,1),size=customers,replace=T,prob=c(1/3,2/3))
}

low_skill_do_business=function(customers=20){
  sample(c(0,1),size=customers,replace=T,prob=c(2/3,1/3))
}

people=1000
sales=c()
skills=sample(c(0,1),size=people,prob=c(0.5,0.5),replace=TRUE)#模擬這次是高能力者還是低能力者(模擬1000次)


for (i in skills){
if(i==0){ 
  s=high_skill_do_business()#模擬高技能者面對20位客戶
  sales=rbind(sales,s)
}
else{ 
  s=low_skill_do_business()#模擬低技能者面對20位客戶
  sales=rbind(sales,s)
}
}

sales <- data.frame(sales)#變為dataframe比較好處理 sales為1000人的銷售成果




#(a)
ans_a=sum(rowSums(sales)>=9)/people#20位客戶超過9位認同的人數/全體人數
#(b)
high_skill_index=which(skills %in% 0)#挑出高技能的index
high_skill_sales=sales[high_skill_index,]#挑出高技能者的銷售紀錄
ans_b=sum(rowSums(high_skill_sales)>=9)/length(high_skill_index)
#(c)
ans_c=sum(rowSums(high_skill_sales)>=9)/sum(rowSums(sales)>=9)
#(d)
d=function(n,sales){
  ans_a=sum(rowSums(sales)>=n)/people
  ans_b=sum(rowSums(high_skill_sales)>=n)/length(high_skill_index)
  ans_c=sum(rowSums(high_skill_sales)>=n)/sum(rowSums(sales)>=n)
  c(ans_a,ans_b,ans_c)
}

table=c()
for(i in c(1:20)){
 print(d(i,sales))
 table<-rbind(table,d(i,sales))
}



