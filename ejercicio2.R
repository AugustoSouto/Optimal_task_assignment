##CORRER EN LA TERMINAL CON LOS COMANDOS DE LINEA###
# Rscript ejercicio2.R salida.py_tareas salida.py_empleados solucionformato1.txt solucionformato2.txt
###########################################

args = commandArgs(trailingOnly=TRUE)


library(utils)
library(magrittr)
library(GA)

###PROGRAMACION GENERICA DEL PROBLEMA####

ruta="C:/Users/Usuario/Proyecto/"
###EMPLEADOS###
datos_empleados=readLines(args[2])

empleados=datos_empleados[1]
emp=unlist(strsplit(empleados," "))

horas_empleados=datos_empleados[2]
horas_empleados=as.numeric(unlist(strsplit(horas_empleados, " ")))
names(horas_empleados)=emp

hab_empleados=datos_empleados[3]
hab_empleados=as.numeric(unlist(strsplit(hab_empleados, " ")))
names(hab_empleados)=emp

jornal_empleados=datos_empleados[4]
jornal_empleados=as.numeric(unlist(strsplit(jornal_empleados, " ")))
names(jornal_empleados)=emp

cantidad_empleados=length(emp)

###TAREAS######
datos_tareaS=readLines(args[1])

deadline=datos_tareaS[1]
deadline=as.numeric(unlist(deadline))

tareas=datos_tareaS[2]
tareas=unlist(strsplit(tareas, " "))

tiempo_tareas=datos_tareaS[3]
tiempo_tareas=as.numeric(unlist(strsplit(tiempo_tareas, " ")))
names(tiempo_tareas)=tareas

cantidad_tareas=length(tareas)

#####FUNCION DE FITNESS######

fitgen=function(sol_1){
  sol_1=round(sol_1)
  
  a=matrix((hab_empleados[sol_1]+0.5))
  rownames(a)=names(hab_empleados[sol_1])
  b=matrix(horas_empleados[sol_1])
  ab=a*b
  
  tiempo_tareas=as.matrix(tiempo_tareas)
  #tiempo total de las tareas#
  diasn=t(1/ab)%*%tiempo_tareas
  #tiempo por tarea#
  tt=(1/ab)*tiempo_tareas
  
  diasn=tapply(tt,rownames(tt), sum)
  dias_pagarn=ceiling(diasn)
  if(all(dias_pagarn<(deadline+1))){
    jor_p=jornal_empleados[names(dias_pagarn)]
    c=matrix(dias_pagarn)
    d=matrix(jor_p)
    gasto=t(c)%*%d
    -gasto
  }else{
    jor_p=jornal_empleados[names(dias_pagarn)]
    c=matrix(dias_pagarn)
    d=matrix(jor_p)
    gasto=t(c)%*%d
    -(gasto+gasto*(max(dias_pagarn)-deadline))
  }
}

###FUNCION PARA EVALUAR EL TIEMPO DE EJECUCION DE LAS TAREAS PARA CADA EMPLEADO###

# time=function(sol_1){
 # sol_1=round(sol_1)
  
  #a=matrix((hab_empleados[sol_1]+0.5))
  #rownames(a)=names(hab_empleados[sol_1])
  #b=matrix(horas_empleados[sol_1])
  #ab=a*b
  
  #tiempo_tareas=as.matrix(tiempo_tareas)
  #tiempo total de las tareas#
  #diasn=t(1/ab)%*%tiempo_tareas
  #tiempo por tarea#
  #tt=(1/ab)*tiempo_tareas
  
  #diasn=tapply(tt,rownames(tt), sum)
  #dias_pagarn=ceiling(diasn)
  #dias_pagarn
  
#}

###CORRRO LA SOLUCION GENERICA CON LA LIBRERIA GA DE LUCA SCRUCCA###
poblacion=100

gensol=ga(type = "real-valued", lower = as.numeric(matrix(1,cantidad_tareas,1)),
          upper = as.numeric(matrix(cantidad_empleados, cantidad_tareas, 1)), 
          popSize=poblacion, maxiter=10000  , elitism =  max(1, round(poblacion*0.05)),
          parallel = TRUE , fitness = fitgen,
          population = gareal_Population, selection = gareal_lsSelection, 
          crossover = gareal_laCrossover, mutation=gareal_raMutation,  seed = 1,
          names=
            as.character(matrix(
              paste("t", seq(1:cantidad_tareas), sep=""), cantidad_tareas, 1)), run=1000)


resumen=summary(gensol)
solucion=round(resumen$solution[1,])
#time(solucion)
#all(time(solucion)<deadline+1)

#solucion

###GENERACION DEL ARCHIVO DE SALIDA DE LA SOLUCION###
library(dplyr)
library(tidyr)
library(reshape2)

dfsol=as.data.frame(solucion)
dfts=cbind(dfsol, tareas)
dfts$solucion=paste("e", dfts$solucion, sep = "")

tabla2=reshape(transform(dfts, time=ave(solucion,solucion,FUN=seq_along)),
               idvar="solucion", direction="wide", sep="")

tab= dfts %>%
  mutate(id = 1:n()) %>% 
  spread(solucion, tareas) %>% 
  select(-id)

tab1=lapply(tab, function(x) sort(x, na.last = TRUE)) %>% 
  as_data_frame()

tab1=data.frame(tab1)

###EXPORTAR LOS RESULTADOS A TABLA EN TXT###
##PARA EL VALIDADOR DE SOLUCIONES##


write.table(tabla2, file = args[3],  na="",
            sep=" ", quote = FALSE,  row.names = F, col.names = F)

write.table(tab1, file = args[4],  na="",
            sep=" ", quote = FALSE,  row.names = F, col.names = TRUE)
