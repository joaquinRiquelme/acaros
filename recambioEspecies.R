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

sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
              Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 30)



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




# matriz de abundancia por cumnidades
matriz.abundancia <- pivot_wider(data = n.id, id_cols = "id", names_from = "morfoespecie", values_from = "abundancia")
head(matriz.abundancia)
matriz.abundancia[is.na(matriz.abundancia)] <-  0
matriz.num <- matriz.abundancia[,2:ncol(matriz.abundancia)]
matriz.sitio <- unique(acaros[,c("id","Lugar","Mes")])
matriz.sitio <- matriz.sitio[order(matriz.sitio$id),]
head(matriz.sitio)

# matriz de divergencia
dist_matrix <- vegdist(matriz.num, method = "bray")
dist_matrix

# PCoA
pcoa_acaros <- wcmdscale(dist_matrix, eig = TRUE)

# variabilidad explicada por cada eje
eigenvalues <- pcoa_acaros$eig
pc1_exp <- round(eigenvalues[1] / sum(eigenvalues) * 100, 1)
pc2_exp <- round(eigenvalues[2] / sum(eigenvalues) * 100, 1)

# Extraer coordenadas de los sitios
pcoa_coords <- as.data.frame(pcoa_acaros$points)
colnames(pcoa_coords) <- c("PCoA1", "PCoA2")

# Graficar
ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(label = rownames(pcoa_coords)), vjust = -1, size = 3) +
  labs(title = "Ordenación PCoA (Bray-Curtis)",
       x = paste0("PCoA 1 (", pc1_exp, "%)"),
       y = paste0("PCoA 2 (", pc2_exp, "%)")) +
  theme_minimal()


# Permanova
set.seed(123) # Para que los resultados sean reproducibles
resultado_permanova_sitio <- adonis2(matriz.num ~ Lugar, 
                               data = matriz.sitio, 
                               method = "bray", 
                               permutations = 999)

print(resultado_permanova_sitio)

resultado_permanova_mes <- adonis2(matriz.num ~ Mes, 
                               data = matriz.sitio, 
                               method = "bray", 
                               permutations = 999)

print(resultado_permanova_mes)

resultado_permanova_lugar_mes <- adonis2(matriz.num ~ Lugar+Mes, 
                                   data = matriz.sitio, 
                                   method = "bray", 
                                   permutations = 999)

print(resultado_permanova_lugar_mes)

resultado_permanova_mes_lugar <- adonis2(matriz.num ~ Mes+Lugar, 
                                         data = matriz.sitio, 
                                         method = "bray", 
                                         permutations = 999)

print(resultado_permanova_mes_lugar)

# nmds
nmds <- metaMDS(matriz.num, distance = "bray", k = 2, trymax = 200, autotransform = FALSE)
nmds
nmds$stress
scores_nmds <- as.data.frame(scores(nmds, display = "sites"))
scores_nmds$SampleID <- rownames(scores_nmds)

# une metadata (ajusta nombres a tu objeto real)
scores_nmds <- scores_nmds %>%
  left_join(matriz.sitio %>% mutate(SampleID = rownames(matriz.sitio)),
            by = "SampleID")

# plot básico
ggplot(scores_nmds, aes(NMDS1, NMDS2, color = as.factor(Mes), shape = Lugar)) +
  geom_point(size = 3, alpha = 0.9) +
  theme_classic() +
  labs(color = "Mes")


# plot mejorado

# Extraer coordenadas de los sitios
nmds_coords <- as.data.frame(scores(nmds, display = "sites"))
nmds_coords$Mes <- as.factor(matriz.sitio$Mes) # Agregar columna de grupo
nmds_coords$periodo <- "1" # Agregar columna de grupo
nmds_coords$periodo[nmds_coords$Mes==3 | nmds_coords$Mes==4] <- "2" # Agregar columna de grupo
nmds_coords$periodo[nmds_coords$Mes==5 | nmds_coords$Mes==6] <- "3" # Agregar columna de grupo

nmds_coords$Lugar <- matriz.sitio$Lugar
# grafico
ggplot(nmds_coords, aes(x = NMDS1, y = NMDS2, color = Lugar, pch=periodo)) +
  # Ejes en el origen
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  
  # Elipses de confianza (95%)
  stat_ellipse(geom = "polygon", alpha = 0.15, color = NA) +
  
  # Puntos
  geom_point(size = 3) +
  
  # Mostrar el valor de Stress en el gráfico
  annotate("text", x = max(nmds_coords$NMDS1), y = max(nmds_coords$NMDS2), 
           label = paste("Stress =", round(nmds$stress, 3)), 
           hjust = 1, vjust = 1, fontface = "italic") +
  
  labs(title = "NMDS de Comunidades (Bray-Curtis)",
       x = "NMDS Eje 1",
       y = "NMDS Eje 2") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black"))
