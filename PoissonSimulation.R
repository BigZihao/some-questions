
tA = -log(runif(1))/0.1
tS = runif(1,5,10)


simuPoission = function(){
t=0
tA=NULL
while(t<T){
t = t + -log(runif(1))/0.1
tA = c(tA,t)
}
tS = runif(length(tA),5,10)
tW=rep(0,length(tA))
tW[1]=0
for(i in 2:length(tA))
{
  tW[i] = max(tA[i-1]+tS[i-1]+tW[i-1] - tA[i], 0)
}
return( c(length(tA),sum(tW>0),mean(tW),(tA+tS+tW)[length(tA)] - T))
}

res=NULL
for(i in 1:100)
{
a = simuPoission()
res=rbind(res, a)
}
res = data.frame(res)
names(res) = c("#patiens", "#waitingpatients","waitingtime","closetime")
apply(res, 2, mean)
