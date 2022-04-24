# install.packages("neuralnet")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("ggplot2")

library(MASS) 
library(neuralnet) 
library(ggplot2)
library(readxl)

# LECTURA DATASET

futbol2 <- read_excel("C:/Users/mcond/OneDrive/Escritorio/RealMadrid.xlsx")

dataset <- futbol2

dataset <- data.frame(dataset)

dataset2= dataset[ ,c(8,9,37,38)]

names(dataset)

# CREACIÓN DE DATASETS TRAIN Y TEST

datos    <- dataset2
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]

# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]


# FORMULA
# -----------------------------------------------------
nms  <- names(train)
frml <- as.formula(paste("goles_local~", paste(nms[!nms %in% "goles_local"], collapse = " + ")))

nms

frml

# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(frml,
                       data          = train_nrm,
                       hidden        = c(5,3), # ver Notas para detalle 
                       threshold     = 0.05,   # ver Notas para detalle
                       algorithm     = "rprop+" 
)

modelo.nn

# PREDICCION
# -----------------------------------------------------

pr.nn   <- compute(modelo.nn,within(test_nrm,rm(goles_local)))


pr.nn


# se transforma el valor escalar al valor nominal original

goles_local.predict <- pr.nn$net.result*(max(datos$goles_local)-min(datos$goles_local))+min(datos$goles_local)

goles.real  <- (test_nrm$goles_local)*(max(datos$goles_local)-min(datos$goles_local))+min(datos$goles_local)

# Elección de mejor modelo y predicciones finales


plot(modelo.nn, rep="best")


data.frame(Real = test$goles_local, Predicted = goles_local.predict,
           Error = abs(test$goles_local - goles_local.predict) / test$goles_local)


ggplot(test, aes(x=goles_local, y=goles_local.predict)) + geom_point() +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dashed", size=1.5)
