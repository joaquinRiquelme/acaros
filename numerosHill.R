# Codigo para analisis de diversidad de comunidades de acaros en parques nacionales
# Metropolitano y Soberania  (Panama), estimacion de numeros Hill
# 20-ene-2026

# preambulo
library(readxl)
library(tidyverse)
library(vegan)

# cargar base de datos
acaros <- read_excel("Tabla Datos de ácaros del suelo PNM & PNM 6-07-22.xlsx", 
                                                              range = "A1:j294")
View(acaros)
head(acaros)

# creacion de columna id que contiene sitio y mes de muestreo
acaros$id <- paste(acaros$Lugar, acaros$Mes, sep="-")
acaros$Lugar[acaros$Lugar=="pnm"] <- "PNM"
acaros$Lugar[acaros$Lugar=="pns"] <- "PNS"

# columna morfoespecie a partir de la columna genero y cambiando los espacios por puntos 
acaros$morfoespecie <-acaros$Genero
acaros$morfoespecie <- gsub(pattern = " ", replacement = ".", x = acaros$morfoespecie)
sort(unique(acaros$id))
head(acaros)

# periodo
acaros$periodo <- "1" # Agregar columna de grupo
acaros$periodo[acaros$Mes==3 | acaros$Mes==4] <- "2" # Agregar columna de grupo
acaros$periodo[acaros$Mes==5 | acaros$Mes==6] <- "3" # Agregar columna de grupo

# abundancia por sitio, se suman las colectas de los dos tipos de muestreo
n.id <- acaros |> group_by(id, morfoespecie) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.id)

n.lugar <- acaros |> group_by(Lugar, morfoespecie) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.lugar)

n.periodo <- acaros |> group_by(periodo, morfoespecie) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.periodo)

# matriz de abundancia por cumnidades
matriz.abundancia <- pivot_wider(data = n.id, id_cols = c("id"), names_from = "morfoespecie", values_from = "abundancia")
head(matriz.abundancia)
matriz.abundancia[is.na(matriz.abundancia)] <-  0
matriz.num <- matriz.abundancia[,3:ncol(matriz.abundancia)]
matriz.sitio <- unique(acaros[,c("id","Lugar","Mes")])
matriz.sitio <- matriz.sitio[order(matriz.sitio$id),]
head(matriz.sitio)


# Numeros Hill

plot(renyi(matriz.num, scales = c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, Inf),
      hill = TRUE))


plot(renyiaccum(matriz.num, scales = c(0, 0.5, 1, 2, 4, Inf), permutations = 100,
           raw = FALSE, collector = FALSE), what = c("Collector", "mean", "Qnt 0.025", "Qnt 0.975"),
     type = "smooth")

# con inext

library(iNEXT)
library(ggplot2)
library(gridExtra)
library(grid)

head(n.id)

x.id <- as.data.frame(pivot_wider(n.id, id_cols = "morfoespecie", names_from = "id", values_from = "abundancia"))
x.id[is.na(x.id)] <-  0
rownames(x.id) <- x.id$morfoespecie
x.id$morfoespecie <- NULL

x.lugar <- as.data.frame(pivot_wider(n.lugar, id_cols = "morfoespecie", names_from = "Lugar", values_from = "abundancia"))
x.lugar[is.na(x.lugar)] <-  0
rownames(x.lugar) <- x.lugar$morfoespecie
x.lugar$morfoespecie <- NULL

x.periodo <- as.data.frame(pivot_wider(n.periodo, id_cols = "morfoespecie", names_from = "periodo", values_from = "abundancia"))
x.periodo[is.na(x.periodo)] <-  0
rownames(x.periodo) <- x.periodo$morfoespecie
x.periodo$morfoespecie <- NULL

id.hill <- iNEXT(x.id, q=c(0, 1, 2), datatype="abundance", endpoint = 300)
id.ggp1 <- ggiNEXT(id.hill, type=1, facet.var="Assemblage", color.var = "Assemblage")
id.ggp1 + theme_classic() + theme(legend.position = "bottom", legend.box = "horizontal") 

lugar.hill <- iNEXT(x.lugar, q=c(0, 1, 2), datatype="abundance", endpoint = 400)
lugar.ggp1 <- ggiNEXT(lugar.hill, type=1, facet.var="Assemblage", color.var = "Assemblage")

png("lugarhill.png", width = 360, height = 480)
lugar.ggp1 + theme_classic() + theme(legend.position = "bottom", legend.box = "horizontal") 
dev.off()

periodo.hill <- iNEXT(x.periodo, q=c(0, 1, 2), datatype="abundance", endpoint = 300)
periodo.ggp1 <- ggiNEXT(periodo.hill, type=1, facet.var="Assemblage", color.var = "Assemblage")

png("periodohill.png", width = 360, height = 480)
periodo.ggp1 + theme_classic() + theme(legend.position = "bottom", legend.box = "horizontal") 
dev.off()

iNEXT(x = datos$Abundancia, q=0, datatype="abundance") 

iNEXT(x = joaquin0$Abundancia, q=0, datatype="abundance")
iNEXT(x = joaquin$Abundancia, q=0, datatype="abundance") 

# numeros hill
q0.lugar <- iNEXT(x = x.lugar, q=0, datatype="abundance")
png("q0lugar.png", width = 300, height = 400)
plot(q0.lugar, ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()

q0.periodo <- iNEXT(x = x.periodo, q=0, datatype="abundance")
png("q0periodo.png", width = 480, height = 640)
plot(q0.periodo, ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()

# plot(iNEXT(x = x.id, q=1, datatype="abundance"), ylim=c(0,90))+ 
#   theme_classic() + theme(legend.position = "bottom", legend.box = "horizontal")
q1.lugar <- iNEXT(x = x.lugar, q=1, datatype="abundance")
png("q1lugar.png", width = 480, height = 640)
plot(q1.lugar, ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()

q1.periodo <- iNEXT(x = x.periodo, q=1, datatype="abundance")
png("q1periodo.png", width = 480, height = 640)
plot(q1.periodo, ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()

# plot(iNEXT(x = x.id, q=2, datatype="abundance"), ylim=c(0,90))+ 
#   theme_classic() + theme(legend.position = "bottom", legend.box = "horizontal")
q2.lugar <- iNEXT(x = x.lugar, q=2, datatype="abundance")
png("q2lugar.png", width = 480, height = 640)
plot(q2.lugar, ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()

q2.periodo <- iNEXT(x = x.periodo, q=2, datatype="abundance")
png("q2periodo.png", width = 480, height = 640)
plot(iNEXT(x = x.periodo, q=2, datatype="abundance"), ylim=c(0,90))+ 
  theme_minimal() + theme(legend.position = "bottom", legend.box = "horizontal")
dev.off()
