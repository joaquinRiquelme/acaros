# Codigo para analisis de recambio de especies de acaros en parques nacionales por genero
# Metropolitano y Soberania, Panama
# 20-ene-2026


# preambulo
library(readxl)
library(tidyverse)
library(vegan)

# cargar base de datos
acaros <- read_excel("Tabla Datos de ácaros del suelo PNM & PNM 6-07-22.xlsx", 
                                                              range = "A1:j294")
# View(acaros)
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

acaros <- subset(acaros, 
                 !is.element(Genero, c("Morfoespecie 1","Morfoespecie 10","Morfoespecie 11",
                                       "Morfoespecie 12","Morfoespecie 13 C,A (Pro)" ,"Morfoespecie 14",
                                       "Morfoespecie 15","Morfoespecie 16","Morfoespecie 17","Morfoespecie 18",
                                       "Morfoespecie 19","Morfoespecie 2","Morfoespecie 20","Morfoespecie 21",
                                       "Morfoespecie 22","Morfoespecie 23","Morfoespecie 24","Morfoespecie 25",
                                       "Morfoespecie 26","Morfoespecie 27","Morfoespecie 3","Morfoespecie 4",
                                       "Morfoespecie 5","Morfoespecie 6","Morfoespecie 7","Morfoespecie 8","Morfoespecie 9")))


# abundancia por sitio, se suman las colectas de los dos tipos de muestreo
n.genero <- acaros |> group_by(Lugar, Mes, Genero) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.genero)
sort(unique(n.genero$Genero))

# Datos para Sankey: flujo de generos entre sitios
total.abundancia <- n.genero |> group_by(Genero) |>
  summarise(abun.total=sum(abundancia, na.rm = TRUE))
total.abundancia <- total.abundancia[order(total.abundancia$abun.total, decreasing = TRUE), ]

total.sitio <- n.genero |> group_by(Lugar, Genero) |>
  summarise(abun.total=sum(abundancia, na.rm = TRUE))
total.sitio.pnm <- subset(total.sitio, Lugar=="PNM")
total.sitio.pnm <- total.sitio.pnm[order(total.sitio.pnm$abun.total, decreasing = TRUE), ]

total.sitio.pns <- subset(total.sitio, Lugar=="PNS")
total.sitio.pns <- total.sitio.pns[order(total.sitio.pns$abun.total, decreasing = TRUE), ]







f.total <- sort(unique(n.genero$Genero))
length(f.total)

pnm <- subset(n.genero, Lugar =="PNM")
f.pnm <- sort(unique(pnm$Genero))
length(f.pnm)

pns <- subset(n.genero, Lugar =="PNS")
f.pns <- sort(unique(pns$Genero))
length(f.pns)

f.inter <- sort(intersect(f.pnm, f.pns))
length(f.inter)

f.diff <- sort(union(setdiff(f.pnm, f.pns), setdiff(f.pns, f.pnm)))
length(f.diff)

solo.pnm <- intersect(f.diff, f.pnm)
length(solo.pnm)

solo.pns <- intersect(f.diff, f.pns)
length(solo.pns)


orden <- data.frame(Genero=c(solo.pnm, f.inter, solo.pns), n.f=1:53)

n.familia.orden <- merge(n.genero, orden)

n.familia <- n.familia.orden[order(n.familia.orden$n.f),]

# nodes: primero los lugares, luego las familias (o todo único)
nodes <- data.frame(name = unique(c(n.genero$Lugar, n.genero$Genero)))

# links con índices 0-based (IMPORTANTE para networkD3)
links <- data.frame(
  source = match(n.genero$Lugar,   nodes$name) - 1,
  target = match(n.genero$Genero, nodes$name) - 1,
  value  = n.familia$abundancia
)

png("Sankey.png")
sankeyNetwork(
  Links = links, Nodes = nodes,nodePadding = 5,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  fontSize = 10, nodeWidth = 80, sinksRight =FALSE
)
dev.off()

library(plotly)

plot_ly(
  type = "sankey",
  orientation = "h", # "v" para vertical, "h" para horizontal
  node = list(label = nodes$name),
  link = list(source = links$source, target = links$target, value = links$value)
)

# diagrama de venn
comunidades <- list(
  Sitio_pnm = f.pnm,
  Sitio_pns = f.pns
)

install.packages("VennDiagram")
library(VennDiagram)

# 2. Crear el gráfico
ggvenn(
  comunidades, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF"),
  stroke_size = 0.5, set_name_size = 4
)



