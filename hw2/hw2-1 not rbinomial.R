
'''
daily_chip=function(){
chip=sample(c(1,0),n,prob=c(good_chip_prob,bad_chip_prob),replace=TRUE)
employee_good_chip=c()
for (i in chip){
  #print(i)
  if (i==1){
    s=sample(c(0,1),1,prob=c(0.05,0.95))
    #print(s)
    employee_good_chip=c(employee_good_chip,s)

  }
  else if (i==0){
    s=sample(c(0,1),1,prob=c(0.9,0.1))
    employee_good_chip=c(employee_good_chip,s)
    
    }
}

employee_good_index=which(employee_good_chip %in% 1)
real_good=chip[employee_good_index]
ans1=sum(real_good==1)/length(real_good)
good_chip_prob_by_employee=sum(employee_good_chip==1)/length(employee_good_chip)
daily_number_of_deliver_chip=rbinom(1,n,good_chip_prob_by_employee)
print(1)
return(c(daily_number_of_deliver_chip,ans1))
}
'''

