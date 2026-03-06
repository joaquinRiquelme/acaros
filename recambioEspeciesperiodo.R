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
# View(acaros)
head(acaros)

# creacion de columna id que contiene sitio y mes de muestreo
acaros$id <- paste(acaros$Lugar, acaros$Mes, sep="-")
acaros$Lugar[acaros$Lugar=="pnm"] <- "PNM"
acaros$Lugar[acaros$Lugar=="pns"] <- "PNS"
acaros$periodo <- 1
acaros$periodo[acaros$Mes ==3 | acaros$Mes ==4] <- 2
acaros$periodo[acaros$Mes ==5 | acaros$Mes ==6] <- 3


# columna morfoespecie a partir de la columna genero y cambiando los espacios por puntos 
acaros$morfoespecie <-acaros$Genero
acaros$morfoespecie <- gsub(pattern = " ", replacement = ".", x = acaros$morfoespecie)
sort(unique(acaros$id))
head(acaros)
acaros$Familia[acaros$Superfamilia=="Bdellidae"] <- "Bdellidae"

# abundancia por sitio, se suman las colectas de los dos tipos de muestreo
n.familia <- acaros |> group_by(Lugar, periodo, Familia) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.familia)

# Datos para Sankey: flujo de familias entre sitios

n.familia$Familia[n.familia$Familia=="Laelapidae (L3)"] <- "Laelapidae"


f.total <- sort(unique(n.familia$Familia))
length(f.total)

p1 <- subset(n.familia, periodo ==1)
f.p1 <- sort(unique(p1$Familia))
length(f.p1)

p2 <- subset(n.familia, periodo ==2)
f.p2 <- sort(unique(p2$Familia))
length(f.p2)

p3 <- subset(n.familia, periodo ==3)
f.p3 <- sort(unique(p3$Familia))
length(f.p3)

f.inter <- sort(intersect(intersect(f.p1, f.p2), f.p3))
length(f.inter)

f.diff <- sort(
  setdiff(f.total, union(union(intersect(f.p1,f.p2), intersect(f.p1, f.p3)),intersect(f.p2, f.p3))))

length(f.diff)

solo.p1 <- intersect(f.diff, f.p1)
length(solo.p1)

solo.p2 <- intersect(f.diff, f.p2)
length(solo.p2)

solo.p3 <- intersect(f.diff, f.p3)
length(solo.p3)

p1p2 <- setdiff(intersect(f.p1,f.p2), f.inter)
p1p3 <- setdiff(intersect(f.p1,f.p3), f.inter)
p2p3 <- setdiff(intersect(f.p2,f.p3), f.inter)

orden <- data.frame(Familia=c(solo.p1, p1p2,  f.inter, solo.p2, p2p3, p1p3, solo.p3), n.f=1:33)

n.familia.orden <- merge(n.familia, orden)

n.familia <- n.familia.orden[order(n.familia.orden$n.f),]

# nodes: primero los lugares, luego las familias (o todo único)
nodes <- data.frame(name = unique(c(n.familia$Familia, n.familia$periodo)))

# links con índices 0-based (IMPORTANTE para networkD3)
# n.familia12 <- subset(n.familia, periodo==1|periodo==2)
links <- data.frame(
  source = match(n.familia$periodo,   nodes$name) - 1,
  target = match(n.familia$Familia, nodes$name) - 1,
  value  = n.familia$abundancia
)

png("Sankeyperiodo.png")
sankeyNetwork(
  Links = links, Nodes = nodes,nodePadding = 15,
  Source = "source", Target = "target",
  Value = "value", NodeID = "name", iteration =0,
  fontSize = 18, nodeWidth = 80, sinksRight =FALSE
)
dev.off()


# diagrama de venn
comunidades <- list(
  Periodo_1 = f.p1,
  Periodo_2 = f.p2,
  Periodo_3 = f.p3
)

install.packages("VennDiagram")
library(VennDiagram)

# 2. Crear el gráfico
ggvenn(
  comunidades, 
  fill_color = c("#0073C2FF", "#EFC000FF", "#868686FF"),
  stroke_size = 0.5, set_name_size = 4
)



