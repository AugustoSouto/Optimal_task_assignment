rm(list=ls())

library(GA)

desarrolladores=matrix(c(
  0.05, 120, 4,
  0.20, 200, 5,
  0.30, 210, 5,
   0.95, 230, 3,
  0.50, 180, 3), 
  nrow = 5, ncol = 3, byrow = TRUE
)

l=c("Juan", "Pablo", "Ana", "Laura", "Rodrigo")
rownames(desarrolladores)=l

tareas=matrix( c(
  1, 16,
  2, 28,
  3, 11,
  4, 51,
  5, 2,
  6, 23,
  7, 43,
  8, 15
), ncol=2, byrow = TRUE
)

tiempo_tareas=as.data.frame(tareas[,2])
rownames(tiempo_tareas)=tareas[,1]

hab=desarrolladores[,1]
sal=desarrolladores[,2]
hs=desarrolladores[,3]



habi.hs=matrix(
  c(((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]), 
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5]),
    ((0.5+hab[1])*hs[1]), ((0.5+hab[2])*hs[2]), ((0.5+hab[3])*hs[3]), ((0.5+hab[4])*hs[4]), ((0.5+hab[5])*hs[5])
  ), ncol=5, byrow = TRUE)



###solucion de forma binaria####
###como no la agarra bien ninguna libreria, bo la use, pero esta por las dudas###

##recordar que el deadline es de 4 semanas-28 dias##
deadline=28
fitness=function(sol){
  sol=matrix(sol,8,5, byrow = TRUE)
  dias=t((sol/habi.hs))%*%tiempo_tareas
  dias_pagar=ceiling(dias)
  if(all(sum(sol)==1)){
  if(all(dias_pagar<deadline+1)){
    -t(dias_pagar)%*%sal
  }else{
      -999999999
    }
  }else{
    -999999999
}
}

library(parallel)
library(doParallel)
er=ga(type = "binary", popSize=100, maxiter=100000  ,
      parallel = TRUE, fitness = fitness, nBits = 40)
summary(er)

er=ga(type = "binary", popSize=100, maxiter=100000  ,
      population = gaperm_Population(1,5),
      parallel = TRUE, fitness = fitness, nBits = 40)



###solucion con enteros de 1 a 5####
sol_1=round(sol_1)
deadline=28


fit2=function(sol_1){

a=matrix((hab[sol_1]+0.5))
rownames(a)=names(hab[sol_1])
b=matrix(hs[sol_1])
ab=a*b
tiempo_tareas=as.matrix(tiempo_tareas)

#tiempo total de las tareas#
diasn=t(1/ab)%*%tiempo_tareas
#tiempo por tarea#
tt=(1/ab)*tiempo_tareas

dias_pagarn=ceiling(diasn)
if(all(dias_pagarn<(deadline+1))){
  sal_p=sal[names(dias_pagarn)]
  c=matrix(dias_pagarn)
  d=matrix(sal_p)
  gasto=t(c)%*%d
  gasto
}else{999999999
}
}

matrix=matrix(c(4,3,4,4,3,3,3,4))

re=GeneticAlg.int(genomeLen=8, codonMin=1, codonMax=5,  
                  popSize =100, iterations = 1000, 
                  mutationChance = 0.01, elitism = 2, suggestions = matrix ,
                  monitorFunc = NULL, evalFunc=fit2, allowrepeat = TRUE)



##########de aca para abajo es todo borrador#####





library(genag)
rbga(stringMin=c(1,1,1,1,1,1,1,1), stringMax=c(5,5,5,5,5,5,5,5),
     suggestions=NULL,
     popSize=100, iters=100,
     mutationChance=NA,
     elitism=NA,
     monitorFunc=NULL, evalFunc=NULL,
     showSettings=FALSE, verbose=FALSE)


library(parallel)
library(doParallel)

er=ga(type = "real-valued", lower = c(1,1,1,1,1,1,1,1), upper = c(5,5,5,5,5,5,5,5), 
      popSize=100, maxiter=100  , elitism = 2,
      parallel = TRUE, fitness = fit2)

ers=summary(er)
sol.er=(round(ers$solution))


library(genalg)


library(gramEvol)
re=GeneticAlg.int(genomeLen=8, codonMin=1, codonMax=5, 
                popSize =50, 
               iterations = 100, 
               mutationChance = 1/(genomeLen+1), elitism = floor(popSize/10), 
               monitorFunc = NULL, evalFunc=fit2, allowrepeat = TRUE)
matrix=matrix(c(4,3,4,4,3,3,3,4))

re=GeneticAlg.int(genomeLen=8, codonMin=1, codonMax=5,  
                  popSize =100, iterations = 1000, 
                  mutationChance = 0.01, elitism = 2, suggestions = matrix ,
                  monitorFunc = NULL, evalFunc=fit2, allowrepeat = TRUE)








