## 1.1 ------------------------------------------------------------------------

#Función del GD para funciones de dos variables
GD <- function(f,dfu,dfv,valor_inicial=c(0,0),num_iteraciones=1000000,nu=0.01,umbral_parada=-10^100,umbral_dif=0){
  i = 0
  
  valor_anterior = valor_inicial
  while( i < num_iteraciones & f(valor_inicial[1],valor_inicial[2]) > umbral_parada & (f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) > umbral_dif | i == 0 ) ){

    valor_anterior[1] = valor_inicial[1]
    valor_inicial[1] = valor_inicial[1] - nu*dfu(valor_inicial[1],valor_inicial[2])
    
    valor_anterior[2] = valor_inicial[2]
    valor_inicial[2] = valor_inicial[2] - nu*dfv(valor_inicial[1],valor_inicial[2])
    i = i+1
    
    
    print(f(valor_inicial[1],valor_inicial[2]) > umbral_parada)
    print((f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) > umbral_dif))
    
  }
  print(i)
  print(valor_inicial[1])
  print(valor_inicial[2])
  print(f(valor_inicial[1],valor_inicial[2]))
  f(valor_inicial[1],valor_inicial[2])
}




## ------------------------------------------------------------------------

## 1.2 ------------------------------------------------------------------------

funcion1 <- function(u,v){
  e = 2.71828
  (u^3*e^(v-2) -4*v^3*e^-u )^2
  
  
}


funcion1du <- function(u,v){
  e = 2.71828
  2*(e^(v-2)*u^3-4*v^3*e^(-u))*(4*v^3*e^(-u)+3*e^(v-2)*u^2)
  
  
}

funcion1dv <- function(u,v){
  e = 2.71828
  2*(u^3*e^(v-2)-12*e^(-u)*v^2)*(u^3*e^(v-2)-4*e^(-u)*v^3)
  
  
}



valor_inicial = c(1,1)
umbral_parada = 10^-14
umbral_dif = 10^-14

minimo = GD(f,du,dv,valor_inicio,300,0.1,umbral__parada,umbral__dif)


## ------------------------------------------------------------------------

## 1.3 ------------------------------------------------------------------------




funcion2 <- function(x,y){
  2*sin(2*pi*y)*sin(2*pi*x)+(x-2)^2+2*(y+2)^2
}


funcion2dx <- function(x,y){
  4*pi*sin(2*pi*y)*cos(2*pi*x)+2*(x-2)
}


funcion2dy <- function(x,y){
  2*(2*pi*sin(2*pi*x)*cos(2*pi*y)+2*(y+2))
}


punto_inicial = c(3,-3)
nu = 0.01
num_iteraciones = 50


minimo = GD(funcion2,funcion2dx,funcion2dy,punto_inicial,num_iteraciones,nu)




