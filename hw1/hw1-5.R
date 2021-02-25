
return_book=function(){
  return_day=c()
  return_book=sample(c(1,2,3,4),size=1,replace=T,prob=c(0.1,0.2,0.3,0.4))
  return_lib=sample(c(0,1),size=1,replace=T,prob=c(0.5,0.5))
  if (return_lib==0){
    return_day<-rbind(return_day,c(return_book+2))
  }
    else{
      return_day<-rbind(return_day,c(return_book))
    }
}

t=replicate(10000,return_book())

ans5=length(which(t==3))/10000
  

