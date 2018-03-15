
## ------------------------------------------------------------------------

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
RegresionLinealPseudoinversa <- function(fdatos, fdigitos){
  pesos <- pseudoinversa(fdatos) %*% fdigitos
}

#Función de regresión lineal usando SGD
RegresionLinealSGD <- function(fdatos, fdigitos){
 w0
}






## ------------------------------------------------------------------------

# 1------------------- LEER DIGITOS

digit.train <- read.table("zip.train", quote="\"", comment.char="", stringsAsFactors=FALSE) # 1. lectura zip del train
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,] # Se obtienen los números 1 y 5
digitos = digitos15.train[,1]    # vector de etiquetas del train
ndigitos = nrow(digitos15.train)  # numero de muestras del train

digit.test <- read.table("zip.test", quote="\"", comment.char="", stringsAsFactors=TRUE) # 1. lectura zip del test
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,] # Se obtienen los números 1 y 5
digitosTest = digitos15.test[,1]    # vector de etiquetas del test
ndigitosTest = nrow(digitos15.test)  # numero de muestras del test





# se retira la clase y se monta una matriz 3D: 599*16*16

# 2------------------- OBTENER GRISES
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))

grisesTest = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitosTest,16,16))

# -------------------- OBTENER INTENSIDAD
intensidad = apply( grises, MARGIN = 1, FUN = mean )

intensidadTest = apply( grisesTest, MARGIN = 1, FUN = mean )



# 3------------------- OBTENER SIMETRIA
simetria = apply( grises, MARGIN = 1, FUN = fsimetria )

simetriaTest = apply( grisesTest, MARGIN = 1, FUN = fsimetria )


# -------------------- LIBERAR ESPACIO
rm(digit.train) # Se borran cosas para liberar espacio
rm(digitos15.train)

rm(digit.test) # Se borran cosas para liberar espacio
rm(digitos15.test)


# 4------------------- REETIQUETAR DIGITOS
digitos[digitos==5]=-1

digitosTest[digitosTest==5]=-1
  
# 5------------------- CREAR DATOSTR
datosTr = as.matrix(cbind(intensidad,simetria,1))
datosTest = as.matrix(cbind(intensidadTest,simetriaTest,1))


# -------------------- SACAR PESOS
w = RegresionLinealPseudoinversa(datosTr, digitos)

#w = RegresionLinealSGD(datosTr,digitos)

# -------------------- OBTENER RESULTADOS DE h(x) CON TRAIN
hx = datosTr %*% w
hxout = datosTest %*% w


# -------------------- DIBUJAR FUNCION
ayb = pasoARecta(w)

x = c(-0.9,0.1)
y = c(-0.9*ayb[1] + ayb[2], 0.1*ayb[1] + ayb[2] )

plot(x=intensidad,y=simetria,col=digitos+2)
lines(x,y,col='red')


# -------------------- OBTENEMOS Ein y Eout

Ein = ( t(w) %*% t(datosTr) %*% datosTr %*% w - 2*t(digitos)%*%datosTr%*%w + t(digitos)%*%digitos )/599

Eout = ( t(w) %*% t(datosTest) %*% datosTest %*% w - 2*t(digitosTest)%*%datosTest%*%w + t(digitosTest)%*%digitosTest )/49



# 1. lectura zip del train
#       Muestras de 1: 442 Muestras de 5: 157
# 2. obtencion de la intensidad del train
# 3. obtencion de la simetria del train. --- liberar espacio rm(grises)
# 4. recodificar digitos del train (las etiquetas), las que figuran como 5 son -1
# 5. componer datosTr = as.matrix(cbind(intensidad,simetria)) del train
# repetir pasos 1..5 con el test
# ...
# 10. componer datosTst = as.matrix(cbind(intensidad,simetria)) del train

# tareas a realizar para llevar a cabo la regresion:

# 11. IMPLEMENTAR Regression_Lin para la obtencion de los pesos del modelo lineal
# w = Regression_Lin(datosTr,digitos)  # obtener w de pesos. Cuidado!!!  w3 es el termino independiente
# repetir 12 y 13 para training y para test, Ein y Eout
# 12. clasificar los datos con los pesos, esto es, obtención del vector de etiquetas predichas
# 13. calcular los errores
# 