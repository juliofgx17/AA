# Julio A. Fresneda - 49215154F - juliofresnedag@correo.ugr.es


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# PARTE 1 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


## 1.1 ------------------------------------------------------------------------

#Función del GD para funciones de dos variables
GD <- function(f,dfu,dfv,valor_inicial=c(0,0),num_iteraciones=1000000,nu=0.01,umbral_parada=NaN,umbral_dif=NaN){
  i = 0
  valor_anterior = valor_inicial   # Para calcular la diferencia de valores (si es muy pequeña el GD para)
  
  while( i < num_iteraciones & (f(valor_inicial[1],valor_inicial[2]) > umbral_parada | is.nan(umbral_parada)) & abs(f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) > umbral_dif | i == 0 | is.nan(umbral_dif)) ){
    
    valor_anterior[1] = valor_inicial[1]
    valor_anterior[2] = valor_inicial[2]
    
    valor_inicial[1] = valor_inicial[1] - nu*dfu(valor_inicial[1],valor_inicial[2])
    valor_inicial[2] = valor_inicial[2] - nu*dfv(valor_anterior[1],valor_inicial[2])  # Usa el valor u recién actualizado
    
    i = i+1
    
    if( !is.nan(umbral_parada) & f(valor_inicial[1],valor_inicial[2]) < umbral_parada) print("Alcanzado umbral de parada")
    if( !is.nan(umbral_dif) & (f(valor_anterior[1],valor_anterior[2]) - f(valor_inicial[1],valor_inicial[2]) < umbral_dif)) print("Alcanzado umbral de diferencia")
    
  }
  
  cat("\nGD ha acabado. Iteraciones completadas:", i)
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
num_iteraciones_funcion1 = 30000
nu_funcion1 = 0.05

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
  4*pi*sin(2*pi*x)*cos(2*pi*y)+4*(y+2) #Derivada de funcion1 respecro a y
}


punto_inicial_funcion2 = c(1,1)
nu_funcion2 = 0.01
nu_funcion2_0.1 = 0.1
num_iteraciones_funcion2 = 50


minimo_funcion2 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,num_iteraciones_funcion2,nu_funcion2)

minimos_nu0.01 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,1,nu_funcion2)
for(i in 2:50 ){
  minimos_nu0.01 = c(minimos_nu0.01,GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,i,nu_funcion2))
}

minimos_nu0.1 = GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,1,nu_funcion2_0.1)
for(i in 2:50 ){
  minimos_nu0.1 = c(minimos_nu0.1,GD(funcion2,funcion2dx,funcion2dy,punto_inicial_funcion2,i,nu_funcion2_0.1))
}



minimo_punto2 = (GD(funcion2,funcion2dx,funcion2dy,c(2.1,-2.1),num_iteraciones_funcion2,nu_funcion2))
minimo_punto3 = (GD(funcion2,funcion2dx,funcion2dy,c(3,-3),num_iteraciones_funcion2,nu_funcion2))
minimo_punto4 = (GD(funcion2,funcion2dx,funcion2dy,c(1.5,1.5),num_iteraciones_funcion2,nu_funcion2))
minimo_punto5 = (GD(funcion2,funcion2dx,funcion2dy,c(1,-1),num_iteraciones_funcion2,nu_funcion2))

#Puntos mínimos desde distintos inicios
minimos_puntos = c(minimo_punto2,minimo_punto3,minimo_punto4,minimo_punto5)















#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

                                                  # PARTE 2 

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



set.seed(3)

#############################################################################################################

# FUNCIONES

#############################################################################################################

## ------------------------------------------------------------------------
## - Función derivada de Ein para Logistic Regression
dEinLR <- function(w,x,y,porcentaje){
  suma = c(1,1,1)
  N = length(y)*porcentaje
  for( i in 1:N ){
    irand = sample(length(y),1)
    
    a = y[irand]*x[irand,]
    b = exp(-y[irand]*t(w)%*%x[irand,])
    c = 1+exp(-y[irand]*t(w)%*%x[irand,])
    
    suma = suma - as.vector(a)*(as.vector(b)/as.vector(c))
    
    
  }
  
  suma = suma/N
  
  suma
}




## - Función del algoritmo SGD
SGD <- function(f,df,x,y,w_inicial=c(1,1,1),num_iteraciones=1000,porcentaje=0.2,nu=0.01){
  
  for( i in 1:num_iteraciones ){
    w_inicial = w_inicial - nu*dEinLR(w_inicial,x,y,porcentaje)
    
    
  }
  
  cat("\nSGD ha acabado. Pesos: ", w_inicial)
  cat("\n")
  
  w_inicial
}



#Función para añadir ruido
noise <- function(label, p){
  result <- label * sample(c(1, -1), size=length(label), replace=TRUE, prob=c(1 - p, p))
  result
}

#Función para simular puntos en un plano
simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
  m
}


#Función de simetría. Devuelve un número que nos dirá la simetría
fsimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

#Función para pasar los pesos a recta.
pasoARecta= function(w){
  if(length(w)!= 3)
    stop("Solo tiene sentido con 3 pesos")
  a = -w[1]/w[2]
  b = -w[3]/w[2]
  c(a,b)
}



#Función de pseudoinversa
pseudoinversa = function(fdatos){
  solve(t(fdatos) %*% fdatos ) %*% t(fdatos)  # (Xt*X)^-1 * Xt
}

#Función de regresión lineal usando la pseudoinversa
RegresionLinealPseudoinversa <- function(fdatos, fetiquetas){
  pesos <- pseudoinversa(fdatos) %*% fetiquetas
}


#Función que devuelve el error
error <- function(etiquetas,predicciones){
  length(etiquetas[etiquetas != as.vector(predicciones)])/length(etiquetas)
}

#######################################################################################################

# EJERCICIOS

#######################################################################################################


## 2.1) ------------------------------------------------------------------------


# Leer datos y etiquetas
digit.train <- read.table("zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE) # 1. lectura zip del train
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,] # Se obtienen los números 1 y 5
etiquetasTrain = digitos15.train[,1]    # vector de etiquetas del train
netiquetasTrain = nrow(digitos15.train)  # numero de muestras del train

digit.test <- read.table("zip.test", quote="\"", comment.char="", stringsAsFactors=TRUE) # 1. lectura zip del test
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,] # Se obtienen los números 1 y 5
etiquetasTest = digitos15.test[,1]    # vector de etiquetas del test
netiquetasTest = nrow(digitos15.test)  # numero de muestras del test


# se retira la clase y se monta una matriz 3D: 599*16*16

# Obtener grises
grisesTrain = array(unlist(subset(digitos15.train,select=-V1)),c(netiquetasTrain,16,16))
grisesTest = array(unlist(subset(digitos15.test,select=-V1)),c(netiquetasTest,16,16))

# Obtener intensidad
intensidadTrain = apply( grisesTrain, MARGIN = 1, FUN = mean )
intensidadTest = apply( grisesTest, MARGIN = 1, FUN = mean )

# Obtener simetria
simetriaTrain = apply( grisesTrain, MARGIN = 1, FUN = fsimetria )
simetriaTest = apply( grisesTest, MARGIN = 1, FUN = fsimetria )

# Liberar espacio
rm(digit.train) # Se borran cosas para liberar espacio
rm(digitos15.train)

rm(digit.test) # Se borran cosas para liberar espacio
rm(digitos15.test)

# Reetiquetar etiquetas
etiquetasTrain[etiquetasTrain==5]=-1
etiquetasTest[etiquetasTest==5]=-1

# Crear matriz de datos
datosTr = as.matrix(cbind(intensidadTrain,simetriaTrain,1))
datosTest = as.matrix(cbind(intensidadTest,simetriaTest,1))









# Obtener pesos (Pseudoinversa y SGD)
w = c(1,1,1)



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# El método de obtener los pesos mediante SGD está comentado. Para probarlo, descomentarlo y comentar el método
# de la pseudoinversa. 


w = RegresionLinealPseudoinversa(datosTr, etiquetasTrain)       # MÉTODO PSEUDO INVERSA
#w = SGD(EinLR,dEinLR,datosTr,etiquetasTrain,w,1000,0.2,0.1)     # MÉTODO SGD (COMENTADO)




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




# Obtener resultados predichos por h(x)
hx = sign(datosTr %*% w)      # TRAIN
hxout = sign(datosTest %*% w) # TEST


# Dibujar función
ayb = pasoARecta(w)

x = c(-1,0.5)
y = c(-1*ayb[1] + ayb[2], 0.5*ayb[1] + ayb[2] )


plot(x=intensidadTrain,y=simetriaTrain,col=etiquetasTrain+3,xlim=c(-1,0.5),ylim=c(-600,0)) # TRAIN
par(new=TRUE)
plot(x=intensidadTest,y=simetriaTest,col=etiquetasTest+7,xlim=c(-1,0.5),ylim=c(-600,0)) # TEST
lines(x,y,col='green')


# Obtener Ein y Eout
Ein = error(etiquetasTrain,hx)
Eout = error(etiquetasTest,hxout)




# 2.2) ------------------------------------------------------------------------------



# 2.2a) ------


# Creamos 1000 puntos aleatorios
puntos_aleatorios = simula_unif(1000,2,c(-1,1))

# Dibujamos los puntos
plot(puntos_aleatorios,col="blue")



# 2.2b) ------


# Declaramos la función
funcion3 = function(x1,x2){
  sign((x1 - 0.2)^2 + x2^2 - 0.6)
}

# Etiquetamos
etiquetas_puntos = funcion3(puntos_aleatorios[,1],puntos_aleatorios[,2])
etiquetas_puntos = noise(etiquetas_puntos,0.1)

puntos_etiquetados = c(puntos_aleatorios[,1],puntos_aleatorios[,2],etiquetas_puntos)
dim(puntos_etiquetados)=c(1000,3)

# Dibujamos los puntos, separados por colores según la etiqueta. Azul = +1, Rojo = -1
plot(puntos_etiquetados,col=etiquetas_puntos+3)



# 2.2c) ------


# Creamos la matriz que usaremos para obtener los pesos
unos = 1
for( i in 2:1000 ) unos = c(1,unos)
puntos_aleatorios = c(unos,puntos_aleatorios[,1],puntos_aleatorios[,2])
dim(puntos_aleatorios)=c(1000,3)


# Predecimos los pesos con el algoritmo SGD
w_f3 = SGD(EinLR,dEinLR,puntos_aleatorios,etiquetas_puntos,c(1,1,1),1000,0.3,0.1)


# Dibujamos la recta
ayb = pasoARecta(w_f3)
x = c(-1,1)
y = c(-1*ayb[1] + ayb[2], 1*ayb[1] + ayb[2] )
plot(puntos_etiquetados,col=etiquetas_puntos+3)
lines(x,y,col='black',size=3)


# Obtenemos las predicciones y su error
hxf3 = sign(puntos_aleatorios %*% w_f3)
Einf3 = error(etiquetas_puntos,hxf3)









# 2.2d) ------

media_Ein = 0
media_Eout = 0

# Media de 1000 veces
for( i in 1:1000 ){
  
  puntos_aleatorios = simula_unif(1000,2,c(-1,1))
  puntos_aleatorios_test = simula_unif(1000,2,c(-1,1))
  
  etiquetas_puntos = funcion3(puntos_aleatorios[,1],puntos_aleatorios[,2])
  etiquetas_puntos = noise(etiquetas_puntos,0.1)
  
  etiquetas_puntos_test = funcion3(puntos_aleatorios_test[,1],puntos_aleatorios_test[,2])
  
  puntos_etiquetados = c(puntos_aleatorios[,1],puntos_aleatorios[,2],etiquetas_puntos)
  dim(puntos_etiquetados)=c(1000,3)
  
  
  
  unos = 1
  for( i in 2:1000 ) unos = c(1,unos)
  puntos_aleatorios = c(unos,puntos_aleatorios[,1],puntos_aleatorios[,2])
  puntos_aleatorios_test = c(unos,puntos_aleatorios_test[,1],puntos_aleatorios_test[,2])
  
  dim(puntos_aleatorios)=c(1000,3)
  dim(puntos_aleatorios_test)=c(1000,3)
  
  w_f3 = SGD(EinLR,dEinLR,puntos_aleatorios,etiquetas_puntos,c(1,1,1),1000,0.05,0.1)
  
  
  hxf3 = sign(puntos_aleatorios %*% w_f3)
  Einf3 = error(etiquetas_puntos,hxf3)
  
  hxoutf3 = sign(puntos_aleatorios_test %*% w_f3)
  Eoutf3 = error(etiquetas_puntos_test,hxoutf3)
  
  media_Ein = media_Ein + Einf3
  media_Eout = media_Eout + Eoutf3
  
  
  
}

# Resultado
media_Eout = media_Eout/1000
media_Ein = media_Ein/1000






