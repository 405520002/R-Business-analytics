roll_dice=function(){
  dice1=sample(c(1,2,3,4,5,6),size=1,replace=T)
  dice2=sample(c(1,2,3,4,5,6),size=1,replace=T)
  dice3=sample(c(1,2,3,4,5,6),size=1,replace=T,prob=c(0.15,0.25,0.15,0.15,0.15,0.15))
  min(c(dice1,dice2,dice3))
}

F=replicate(10000,roll_dice())#°²³]§ë1000¦¸
#expectvalue=sum(F)/10000
plot(table(F))

