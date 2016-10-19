#install.packages("adagio")

setwd("C:/Users/user/Desktop/Python")

histo = as.numeric(readLines("knapsack.txt"))

hist(histo,breaks=100)

table(histo)

plot(sort(unique(histo)))

rep13 = sapply(2:52,function(x) 13*x)

rep2_13 = sapply(1:45,function(x) 165+(13*x))

plot(sort(unique(histo[histo %in% rep13])))

plot(sort(unique(histo[!histo %in% rep13])))

###################################

library(adagio)

cupon_we_have = histo

hist(cupon_we_have)

cupon_we_want=1000

a=1

prof=NULL

solution <- try(knapsack(p=cupon_we_have, w=cupon_we_have, cap=cupon_we_want))

###################################

library(adagio)

cupon_we_have = round(c(runif(1000,26,750)))

hist(cupon_we_have)

cupon_we_want=c(1000)

a=1

repeat{

solution <- try(mknapsack(p=cupon_we_have, w=cupon_we_have, k=cupon_we_want,bck=0))

print(solution)

if(class(solution)=="try-error") break

diff = NULL

for(i in 1:length(cupon_we_want)) diff[i] = cupon_we_want[i] - sum(cupon_we_have[solution$ksack==i])

if(all(diff==0)) cupon_we_want = c(cupon_we_want,1000) else break

a=a+1

print(a)

}

################################

library(adagio)

lot = as.numeric(names(table(histo)[table(histo)>30]))

lot_m = round(mean(table(histo)[table(histo)>30]))

hist(cupon_we_have)

cupon_we_want=c(1000)

a=1

prof = NULL

repeat{
  
  cupon_we_have = c(sample(c(rep13[!rep13 %in% lot],rep2_13[!rep2_13 %in% lot]),(1000-(length(lot)*lot_m)),replace = TRUE),
                    rep(lot,lot_m))
  
  solution <- try(knapsack(p=cupon_we_have, w=cupon_we_have, cap=cupon_we_want))
  
  if(class(solution)=="try-error") break
  
  diff = NULL
  
  for(i in 1:length(cupon_we_want)) diff[i] = cupon_we_want - sum(cupon_we_have[solution$indices])
  
  if(diff==0) break
  
  prof = c(prof,solution$profit)
  
  a=a+1
  
  print(a)
  
}