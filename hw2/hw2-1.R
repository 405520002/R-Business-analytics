good_chip_prob=0.83 #生產線實際生產良率
bad_chip_prob=0.17#生產線實際生產瑕疵率
n=50*20*8#一天全體工人檢查的晶片
good_good_prob=0.95#當實際是好的的工人檢查出好的機率
bad_good_prob=0.1#當實際是壞的的工人檢查出好的機率

daily_chip=function(s=1){
  good=rbinom(s,n,good_chip_prob)#8000裡面好的個數
  bad=n-good#8000裡面壞的個數
  good_good=rbinom(s,good,good_good_prob)#好的裡面也檢查出好的個數 n代good
  bad_good=rbinom(s,bad,bad_good_prob)#壞的裡面檢查出壞的個數 n代bad
  good_bad=good-good_good
  bad_bad=bad-bad_good
  daily_number_of_deliver_chip=good_good+bad_good
  ans1=good_good/(bad_good+good_good)
  return(c(daily_number_of_deliver_chip,ans1))
}

a=c()
for (i in c(1:1000)){
  e=daily_chip()
  a=rbind(a,e)
}


a <- data.frame(a)

ans1_1=sum(a[2]>=0.98)/1000
ans1_2=sum(a[1]>6400)/1000



####第二題####

##monthly##
m=c()
month_day=30
for (i in c(1:1000)){
  e=daily_chip(month_day)
  s=sum(e[1:month_day])#因為daily chip 會回傳一個陣列 index 1:month_day=daily_number_of_deliver_chip
  m=rbind(m,s)                                      # index month_day+1:=ans1(所以擷取1:month_day)
}

##quaterly##

q=c()
q_day=90
for (i in c(1:1000)){
  e=daily_chip(q_day)
  s=sum(e[1:q_day])#同monthly
  q=rbind(q,s)
}

###histigram##
hist(m, breaks=20, main="monthly With breaks=20")
hist(q, breaks=20, main="quaterly With breaks=20")

##sharpio.test()##
shapiro.test(m)
shapiro.test(q)



