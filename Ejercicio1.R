## 1.1 ------------------------------------------------------------------------

#Función del GD para funciones de dos variables
GD <- function(f,dfu,dfv,valor_inicial=c(0,0),num_iteraciones=1000000,nu=0.01,umbral_parada=NaN,umbral_dif=NaN){
  i = 0
  valor_anterior = valor_inicial   # Para calcular la diferencia de valores (si es muy pequeña el GD para)
  
  while( i < num_iteraciones & (f(valor_inicial[1],valor_inicial[2]) > umbral_parada | is.nan(umbral_parada)) & abs(f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) > umbral_dif | i == 0 | is.nan(umbral_dif)) ){
 
    valor_anterior[1] = valor_inicial[1]
    valor_anterior[2] = valor_inicial[2]
    
    valor_inicial[1] = valor_inicial[1] - nu*dfu(valor_inicial[1],valor_inicial[2])
    valor_inicial[2] = valor_inicial[2] - nu*dfv(valor_inicial[1],valor_inicial[2])  # Usa el valor u recién actualizado
    
    i = i+1
    
    
    if( !is.nan(umbral_parada) & f(valor_inicial[1],valor_inicial[2]) < umbral_parada) print("Alcanzado umbral de parada")
    if( !is.nan(umbral_dif) & (f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) < umbral_dif)) print("Alcanzado umbral de diferencia")
  
  }
  
  cat("\nGD ha acabado. Iteraciones completadas", i)
  cat("\nCoordenadas del minimo: ", valor_inicial[1], valor_inicial[2] )
  cat("\nValor minimo de la funcion: " , f(valor_inicial[1],valor_inicial[2]))
  cat("\n")
  
  f(valor_inicial[1],valor_inicial[2])
}




## ------------------------------------------------------------------------

## 1.2 ------------------------------------------------------------------------

funcion1 <- function(u,v){
  (u^3*exp(v-2) -4*v^3*(exp(-u)) )^2
}


funcion1du <- function(u,v){
  2*(exp(v-2)*u^3-4*v^3*exp(-u))*(4*v^3*exp(-u)+3*exp(v-2)*u^2) #Derivada de funcion1 respecro a u
}
funcion1dv <- function(u,v){
  2*(u^3*exp(v-2)-12*exp(-u)*v^2)*(u^3*exp(v-2)-4*exp(-u)*v^3) #Derivada de funcion1 respecro a v
}


valor_inicial_funcion1 = c(1,1)
umbral_parada_funcion1 = 10^-14
umbral_dif_funcion1 = 10^-14
num_iteraciones_funcion1 = 30000000
nu_funcion1 = 0.1

minimo_funcion1 = GD(funcion1,funcion1du,funcion1dv,valor_inicial_funcion1,num_iteraciones_funcion1,nu_funcion1,umbral_parada_funcion1,umbral_dif_funcion1)


## ------------------------------------------------------------------------

## 1.3 ------------------------------------------------------------------------




funcion2 <- function(x,y){
  2*sin(2*pi*y)*sin(2*pi*x)+(x-2)^2+2*(y+2)^2
}


funcion2dx <- function(x,y){
  4*pi*sin(2*pi*y)*cos(2*pi*x)+2*(x-2) #Derivada de funcion1 respecro a x
}
funcion2dy <- function(x,y){
  2*(2*pi*sin(2*pi*x)*cos(2*pi*y)+2*(y+2)) #Derivada de funcion1 respecro a y
}


punto_inicial_funcion2 = c(1,1)
nu_funcion2 = 0.01
nu_funcion2_0.1 = 0.1
num_iteraciones_funcion2 = 50

minimo_funcion2 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,num_iteraciones_funcion2,nu_funcion2)

minimos_nu0.01 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,1,nu_funcion2)
for(i in 1:50 ){
  minimos_nu0.01 = c(minimos_nu0.01,GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,i,nu_funcion2))
}

minimos_nu0.1 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,1,nu_funcion2_0.1)
for(i in 1:50 ){
  minimos_nu0.1 = c(minimos_nu0.1,GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,i,nu_funcion2_0.1))
}



minimo_punto2 = (GD(funcion2,funcion2dx,funcion2dy,c(2.1,-2.1),num_iteraciones_funcion2,nu_funcion2))
minimo_punto3 = (GD(funcion2,funcion2dx,funcion2dy,c(3,-3),num_iteraciones_funcion2,nu_funcion2))
minimo_punto4 = (GD(funcion2,funcion2dx,funcion2dy,c(1.5,1.5),num_iteraciones_funcion2,nu_funcion2))
minimo_punto5 = (GD(funcion2,funcion2dx,funcion2dy,c(1,-1),num_iteraciones_funcion2,nu_funcion2))
minimos_puntos = c(minimo_punto2,minimo_punto3,minimo_punto4,minimo_punto5)
## Ejercicios

# 1 - hecho

# 2.a) 
#   7.962252e-15
# 2.b) 
#   45
# 2.c)
#   E(0.9719347, 0.5162463)

# 3.a)
    # minomos_nu0.01 y minimos_nu0.1
# 3.b)
    # minimos_puntos
# 4





    
