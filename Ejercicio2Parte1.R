setwd("~/Desktop/Uni/AA/Practicas/P1")
set.seed(3)

## ------------------------------------------------------------------------



Ein_dwj = function(w,x,y,num_iteraciones){
  
  suma = 0

  for( i in 1:num_iteraciones ){
    xrand = sample(length(x)/3,1)


    if( sign(sum(t(w)%*%(x[xrand,]))) < y[xrand] ) suma = suma + x[xrand]*(-1)
    if( sign(sum(t(w)%*%(x[xrand,]))) > y[xrand] ) suma = suma + x[xrand]
  }
  value = 2/num_iteraciones * suma
  
  value
}

#Función del SGD para clasificador
SGD <- function(w,x,y,porcentaje_aleatorios=0.1,nu=0.01){
  
  i = 0
  w_inicial = w
  w_anterior = w_inicial   # Para calcular la diferencia de valores (si es muy pequeña el SGD para)
  
  num_iteraciones = length(y)*porcentaje_aleatorios
  w
  x[3]
  while( i < num_iteraciones ){
    w
    w_anterior[1] = w_inicial[1]
    w_anterior[2] = w_inicial[2]
    w_anterior[3] = w_inicial[3]
    
    w_inicial[1] = w_inicial[1] - nu*Ein_dwj(w,x,y,num_iteraciones)
    w_inicial[2] = w_inicial[2] - nu*Ein_dwj(w,x,y,num_iteraciones)
    w_inicial[2] = w_inicial[2] - nu*Ein_dwj(w,x,y,num_iteraciones)
    
    i = i+1
  }

  w_inicial


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






## ------------------------------------------------------------------------

# 1------------------- LEER ETIQUETAS

digit.train <- read.table("zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE) # 1. lectura zip del train
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,] # Se obtienen los números 1 y 5
etiquetasTrain = digitos15.train[,1]    # vector de etiquetas del train
netiquetasTrain = nrow(digitos15.train)  # numero de muestras del train

digit.test <- read.table("zip.test", quote="\"", comment.char="", stringsAsFactors=TRUE) # 1. lectura zip del test
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,] # Se obtienen los números 1 y 5
etiquetasTest = digitos15.test[,1]    # vector de etiquetas del test
netiquetasTest = nrow(digitos15.test)  # numero de muestras del test





# se retira la clase y se monta una matriz 3D: 599*16*16

# 2------------------- OBTENER GRISES
grisesTrain = array(unlist(subset(digitos15.train,select=-V1)),c(netiquetasTrain,16,16))
grisesTest = array(unlist(subset(digitos15.test,select=-V1)),c(netiquetasTest,16,16))

# -------------------- OBTENER INTENSIDAD
intensidadTrain = apply( grisesTrain, MARGIN = 1, FUN = mean )
intensidadTest = apply( grisesTest, MARGIN = 1, FUN = mean )



# 3------------------- OBTENER SIMETRIA
simetriaTrain = apply( grisesTrain, MARGIN = 1, FUN = fsimetria )
simetriaTest = apply( grisesTest, MARGIN = 1, FUN = fsimetria )


# -------------------- LIBERAR ESPACIO
rm(digit.train) # Se borran cosas para liberar espacio
rm(digitos15.train)

rm(digit.test) # Se borran cosas para liberar espacio
rm(digitos15.test)


# 4------------------- REETIQUETAR ETIQUETAS
etiquetasTrain[etiquetasTrain==5]=-1
etiquetasTest[etiquetasTest==5]=-1
  
# 5------------------- CREAR DATOSTR
datosTr = as.matrix(cbind(intensidadTrain,simetriaTrain,1))
datosTest = as.matrix(cbind(intensidadTest,simetriaTest,1))


# -------------------- SACAR PESOS
#w = RegresionLinealPseudoinversa(datosTr, etiquetasTrain)
w = c(0,0,0)

w = SGD(w,datosTr,etiquetasTrain,1)

# -------------------- OBTENER RESULTADOS DE h(x) CON TRAIN
hx = sign(datosTr %*% w)
hxout = sign(datosTest %*% w)


# -------------------- DIBUJAR FUNCION
ayb = pasoARecta(w)

x = c(-1,0.5)
y = c(-1*ayb[1] + ayb[2], 0.5*ayb[1] + ayb[2] )


plot(x=intensidadTrain,y=simetriaTrain,col=etiquetasTrain+3,xlim=c(-1,0.5),ylim=c(-600,0))
par(new=TRUE)
plot(x=intensidadTest,y=simetriaTest,col=etiquetasTest+7,xlim=c(-1,0.5),ylim=c(-600,0))
lines(x,y,col='green')


# -------------------- OBTENEMOS Ein y Eout
Ein = error(etiquetasTrain,hx)
Eout = error(etiquetasTest,hxout)














# 1. lectura zip del train
#       Muestras de 1: 442 Muestras de 5: 157
# 2. obtencion de la intensidad del train
# 3. obtencion de la simetria del train. --- liberar espacio rm(grises)
# 4. recodificar etiquetas del train (las etiquetas), las que figuran como 5 son -1
# 5. componer datosTr = as.matrix(cbind(intensidad,simetria)) del train
# repetir pasos 1..5 con el test
# ...
# 10. componer datosTst = as.matrix(cbind(intensidad,simetria)) del train

# tareas a realizar para llevar a cabo la regresion:

# 11. IMPLEMENTAR Regression_Lin para la obtencion de los pesos del modelo lineal
# w = Regression_Lin(datosTr,etiquetas)  # obtener w de pesos. Cuidado!!!  w3 es el termino independiente
# repetir 12 y 13 para training y para test, Ein y Eout
# 12. clasificar los datos con los pesos, esto es, obtención del vector de etiquetas predichas
# 13. calcular los errores
# 









#El gradiente desdendente es una funcion donde se le pasa la f y la f' (derivada) para que sea reutilizable
#Sirve para actualizar el peso 
#GD(f,f',valor_inicial,num_iteraciones,mu (cte, n esa rara), umbral_de_parada(10^-14 por ejemplo),umbral_diferencia_entre_dos_puntos(si la diferencia es muy pequeña es que la funcion apenas baja y se puede tirar años))


#SGD es el GD pero usando puntos aleatorios