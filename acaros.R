# Codigo para analisis de diversidad de comunidades de acaros en parques nacionales
# Metropolitano y Soberania, Panama
# 13-ene-2026

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

# abundancia por sitio, se suman las colectas de los dos tipos de muestreo
n.id <- acaros |> group_by(id, morfoespecie) |> 
  summarise(abundancia = sum(cantidad, na.rm=TRUE))
head(n.id)

# total por sitio
total.abundancia <- n.id |> group_by(morfoespecie) |> 
  summarise(abundancia.total = sum(abundancia, na.rm=TRUE))
head(total.abundancia)



# matriz de abundancia por cumnidades
matriz.abundancia <- pivot_wider(data = n.id, id_cols = "id", names_from = "morfoespecie", values_from = "abundancia")
head(matriz.abundancia)
matriz.abundancia[is.na(matriz.abundancia)] <-  0
matriz.num <- matriz.abundancia[,2:ncol(matriz.abundancia)]
matriz.sitio <- unique(acaros[,c("id","Lugar","Mes")])
matriz.sitio <- matriz.sitio[order(matriz.sitio$id),]
head(matriz.sitio)


# matriz.num <- decostand(matriz.num, method = "hellinger")

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


data_hel <- decostand(matriz.num, method = "hellinger")

set.seed(123) # Para que los resultados sean reproducibles
resultado_permanova_sitio <- adonis2(matriz.num ~ Lugar, 
                               data = matriz.sitio, 
                               method = "bray", 
                               permutations = 999)

print(resultado_permanova_sitio)
resultado_permanova_sitio <- adonis2(data_hel ~ Lugar, 
                                     data = matriz.sitio, 
                                     method = "bray", 
                                     permutations = 999)

print(resultado_permanova_sitio)



resultado_permanova_mes <- adonis2(matriz.num ~ as.factor(Mes), 
                               data = matriz.sitio, 
                               method = "bray", 
                               permutations = 999)
print(resultado_permanova_mes)

resultado_permanova_mes <- adonis2(matriz.num ~ Mes, 
                                   data = matriz.sitio, 
                                   method = "bray", 
                                   permutations = 999)
print(resultado_permanova_mes)


matriz.sitio$periodo <- 1
matriz.sitio$periodo[matriz.sitio$Mes ==3 | matriz.sitio$Mes ==4] <- 2
matriz.sitio$periodo[matriz.sitio$Mes ==5 | matriz.sitio$Mes ==6] <- 3

resultado_permanova_periodo <- adonis2(matriz.num ~ periodo, 
                                       data = matriz.sitio, 
                                       method = "bray", 
                                       permutations = 999)
print(resultado_permanova_periodo)

resultado_permanova_periodo <- adonis2(data_hel ~ periodo, 
                                       data = matriz.sitio, 
                                       method = "bray", 
                                       permutations = 999)
print(resultado_permanova_periodo)


resultado_permanova_lugar_mes <- adonis2(data_hel ~ Lugar+Mes, 
                                         data = matriz.sitio, 
                                         method = "bray", 
                                         permutations = 999)

print(resultado_permanova_lugar_mes)

resultado_permanova_mes_lugar <- adonis2(data_hel ~ Mes+Lugar, 
                                         data = matriz.sitio, 
                                         method = "bray", 
                                         permutations = 999)

print(resultado_permanova_mes_lugar)

# nmds
nmds <- metaMDS(data_hel, distance = "bray", k = 2, trymax = 200, autotransform = FALSE)
nmds
nmds$stress
scores_nmds <- as.data.frame(scores(nmds, display = "sites"))
scores_nmds$SampleID <- rownames(scores_nmds)
stressplot(nmds)

gof <- goodness(nmds)

# Graficar: burbujas más grandes indican sitios PEOR representados
plot(nmds, display = "sites", type = "t", main = "Bondad de ajuste por sitio")
points(nmds, display = "sites", cex = gof * 100) # El

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

library(vegan)

# matriz de comunidad (muestras x taxa)
comm <- data_hel

# distancia (ej. Bray)
d <- vegdist(comm, method = "bray")

# NMDS
set.seed(1)
nmds <- metaMDS(comm, distance = "bray", k = 2, trymax = 200)

# PERMANOVA (por Lugar)
adonis2(d ~ Lugar, data = matriz.sitio, permutations = 999)

# Chequeo de dispersión (muy recomendado)
bd <- betadisper(d, matriz.sitio$periodo)
boxplot(bd, permutations = 999)

bd.mes <- betadisper(d, matriz.sitio$Mes)
boxplot(bd.mes, permutations = 999)

bd.lugar <- betadisper(d, matriz.sitio$Lugar)
boxplot(bd.lugar, permutations = 999)

bd.id <- betadisper(d, matriz.sitio$id)


figura4 <- ggplot(nmds_coords, aes(x = NMDS1, y = NMDS2, color = Lugar , pch=periodo)) +
  # Ejes en el origen
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  
  # Elipses de confianza (95%)
  stat_ellipse(geom = "polygon", alpha = 0.15, color = NA,   level = 0.67,) +
  
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


png("Figura4.png", width = 640, height = 480)
figura4
dev.off()


# beta-diversidad en Bray y Jaccard, y particiona Jaccard en turnover vs nestedness;