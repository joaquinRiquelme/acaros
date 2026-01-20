# Codigo para analisis de recambio de especies de acaros en parques nacionales
# Metropolitano y Soberania, Panama
# 15-ene-2026


# Instalar si es necesario
# install.packages("ggvenn")
library(ggvenn)

# 1. Preparar los datos como una lista de vectores (nombres de especies)
comunidades <- list(
  Sitio_A = c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5", "Sp10"),
  Sitio_B = c("Sp3", "Sp4", "Sp5", "Sp6", "Sp7", "Sp11"),
  Sitio_C = c("Sp5", "Sp6", "Sp7", "Sp8", "Sp9", "Sp12")
)

install.packages("VennDiagram")
library(VennDiagram)

# 2. Crear el gráfico
ggvenn(
  comunidades, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF"),
  stroke_size = 0.5, set_name_size = 4
)



# Listas de familias por sitio
PNM <- c("Nehypochthoniidae", "Galumnidae", "Scheloribatidae", "Macrochelidae", "Lohmanniidae", "Haplozetidae", "Oppiidae")
PNS <- c("Macrochelidae", "Parasitidae", "Nehypochthoniidae")

# Crear diagrama de Venn
venn.plot <- draw.pairwise.venn(
  area1 = length(PNM),
  area2 = length(PNS),
  cross.area = length(intersect(PNM, PNS)),
  category = c("PNM", "PNS"),
  fill = c("forestgreen", "darkorange"),
  lty = "blank",
  cex = 2,
  cat.cex = 2
)
grid.draw(venn.plot)



install.packages("networkD3")
library(networkD3)

# Datos para Sankey: flujo de familias entre sitios
nodes <- data.frame(name = c("PNM", "PNS", "Nehypochthoniidae", "Galumnidae", "Scheloribatidae", "Macrochelidae", "Parasitidae"))
links <- data.frame(
  source = c(0,0,0,0,1,1),  # PNM=0, PNS=1
  target = c(2,3,4,5,5,6),  # Familias
  value = c(10,8,6,12,15,7) # Abundancia aproximada
)

aaa <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
              Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30)
png("Figura2.png", width = 800,height = 600)
aaa
dev.off()

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

# columna morfoespecie a partir de la columna genero y cambiando los espacios por puntos 
acaros$morfoespecie <-acaros$Genero
acaros$morfoespecie <- gsub(pattern = " ", replacement = ".", x = acaros$morfoespecie)
sort(unique(acaros$id))
head(acaros)
acaros$Familia[acaros$Superfamilia=="Bdellidae"] <- "Bdellidae"

# abundancia por sitio, se suman las colectas de los dos tipos de muestreo
n.familia <- acaros |> group_by(Lugar, Mes, Familia) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.familia)

# Datos para Sankey: flujo de familias entre sitios

n.familia$Familia[n.familia$Familia=="Laelapidae (L3)"] <- "Laelapidae"


f.total <- sort(unique(n.familia$Familia))
length(f.total)

pnm <- subset(n.familia, Lugar =="pnm")
f.pnm <- sort(unique(pnm$Familia))
length(f.pnm)

pns <- subset(n.familia, Lugar =="pns")
f.pns <- sort(unique(pns$Familia))
length(f.pns)

f.inter <- sort(intersect(f.pnm, f.pns))
length(f.inter)

f.diff <- sort(union(setdiff(f.pnm, f.pns), setdiff(f.pns, f.pnm)))
length(f.diff)

solo.pnm <- intersect(f.diff, f.pnm)
length(solo.pnm)

solo.pns <- intersect(f.diff, f.pns)
length(solo.pns)


orden <- data.frame(Familia=c(solo.pnm, f.inter, solo.pns), n.f=1:33)

n.familia.orden <- merge(n.familia, orden)

n.familia <- n.familia.orden[order(n.familia.orden$n.f),]

# nodes: primero los lugares, luego las familias (o todo único)
nodes <- data.frame(name = unique(c(n.familia$Lugar, n.familia$Familia)))

# links con índices 0-based (IMPORTANTE para networkD3)
links <- data.frame(
  source = match(n.familia$Lugar,   nodes$name) - 1,
  target = match(n.familia$Familia, nodes$name) - 1,
  value  = n.familia$abundancia
)

sankeyNetwork(
  Links = links, Nodes = nodes,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name",
  fontSize = 18, nodeWidth = 30, 
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



