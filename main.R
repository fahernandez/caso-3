# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
library(extrafont) 
library(RColorBrewer)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(kableExtra)

# Read data
#setwd("C:/Users/abrenes/Documents/Github/caso-3")
#setwd("C:/Users/Anibal/Documents/Github/caso-3")
data<- read.spss("./data/caso3.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")[53:57]
summary(data)
names(data)
view(data)

palett<-"Dark2"
fuente<-"Fuente: Encuesta de opinión en asociados de una cooperativa."

################# Limpieza y transformación de data
## NA values van a ser convertidos a zero al ser pocos valores (< 3%)


## Acceso a servicios de salud
levels(data$ACC2)
data$indexAccess = rep(0, nrow(data))
head(data[,2:9])
for (i in 2:9){
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexAccess[j] = data$indexAccess[j] + 0
      
    }
    else if (data[j,i] == "Muy dificil") {
       data$indexAccess[j] = data$indexAccess[j] + 1
    }  
    else if (data[j,i] == "Díficil") {
      data$indexAccess[j] = data$indexAccess[j] + 2
    }  
    else if (data[j,i] == "Regular") {
      data$indexAccess[j] = data$indexAccess[j] + 3
    }  
    else if (data[j,i] == "Fácil") {
      data$indexAccess[j] = data$indexAccess[j] + 4
    } 
    else if (data[j,i] == "Muy fácil") {
      data$indexAccess[j] = data$indexAccess[j] + 5
    } else {
      print(data[j,i])
    }
  }
}

minV<-min(data$indexAccess)
maxV<-max(data$indexAccess)
data$indexAccess<-sapply(data$indexAccess, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})


## Confianza en los servicios de salud
levels(data$CON8)
data$indexConfidence = rep(0, nrow(data))
head(data[,10:17])
for (i in 10:17){
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexConfidence[j] = data$indexConfidence[j] + 0
      
    }
    else if (data[j,i] == "Ninguna") {
      data$indexConfidence[j] = data$indexConfidence[j] + 1
    }  
    else if (data[j,i] == "Poca") {
      data$indexConfidence[j] = data$indexConfidence[j] + 2
    }  
    else if (data[j,i] == "Regular") {
      data$indexConfidence[j] = data$indexConfidence[j] + 3
    }  
    else if (data[j,i] == "Alguna") {
      data$indexConfidence[j] = data$indexConfidence[j] + 4
    } 
    else if (data[j,i] == "Mucha") {
      data$indexConfidence[j] = data$indexConfidence[j] + 5
    } else {
      print(data[j,i])
    }
  }
}

minV<-min(data$indexConfidence)
maxV<-max(data$indexConfidence)
data$indexConfidence<-sapply(data$indexConfidence, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})


# Calificación de la  CCSS en servicios de salud
data$indexScoreCCSS = rep(0, nrow(data))
head(data[,26:37])
for (i in 26:36){
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexScoreCCSS[j] = data$indexScoreCCSS[j] + 0
    } else {
      data$indexScoreCCSS[j] = data$indexScoreCCSS[j] + as.numeric(data[j,i])
    }
  }
}

minV<-min(data$indexScoreCCSS)
maxV<-max(data$indexScoreCCSS)
data$indexScoreCCSS<-sapply(data$indexScoreCCSS, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})


# Uso de servicios de salud privados
data$indexPrivateUse = rep(0, nrow(data))
head(data[,38:43])
for (i in 38:43) {
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 0
    }
    else if (data[j,i] == "NS/NR") {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 1
    }
    else if (data[j,i] == "NO") {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 1
    }  
    else if (data[j,i] == "No") {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 1
    } 
    else if (data[j,i] == "Sí") {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 2
    } 
    else if (data[j,i] == "Si") {
      data$indexPrivateUse[j] = data$indexPrivateUse[j] + 2
    } else {
      print(data[j,i])
    }
  }
}

minV<-min(data$indexPrivateUse)
maxV<-max(data$indexPrivateUse)
data$indexPrivateUse<-sapply(data$indexPrivateUse, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})

# Disposición en usar la clínica
data$indexDispositionClinic = rep(0, nrow(data))
head(data[,44:51])
for (i in 44:51) {
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 0
    }
    else if (data[j,i] == "NS/NR") {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 1
    }
    else if (data[j,i] == "NO") {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 1
    }  
    else if (data[j,i] == "No") {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 1
    } 
    else if (data[j,i] == "Sí") {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 2
    } 
    else if (data[j,i] == "Si") {
      data$indexDispositionClinic[j] = data$indexDispositionClinic[j] + 2
    } else {
      print(data[j,i])
    }
  }
}

minV<-min(data$indexDispositionClinic)
maxV<-max(data$indexDispositionClinic)
data$indexDispositionClinic<-sapply(data$indexDispositionClinic, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})

# Disposición en utilizar la tarjeta
data$indexDispositionCard = rep(0, nrow(data))
head(data[,53:57])
for (i in 53:57) {
  for(j in 1:nrow(data)) {
    if (is.na(data[j,i])) {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 0
    }
    else if (data[j,i] == "NS/NR") {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 1
    }
    else if (data[j,i] == "NO") {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 1
    }  
    else if (data[j,i] == "No") {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 1
    } 
    else if (data[j,i] == "Sí") {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 2
    } 
    else if (data[j,i] == "Si") {
      data$indexDispositionCard[j] = data$indexDispositionCard[j] + 2
    } else {
      print(data[j,i])
    }
  }
}

minV<-min(data$indexDispositionCard)
maxV<-max(data$indexDispositionCard)
data$indexDispositionCard<-sapply(data$indexDispositionCard, function(x) {
  return (((x - minV)/(maxV-minV))*100)
})

data<-as_tibble(data)
data<-data %>%
  mutate(Preferencia=
           case_when(
             Preferencia == "La clínica propia de la cooperativa" ~ "Clínica Propia",
             Preferencia == "La tarjeta exclusiva de la cooperativa con descuentos especiales en los hospitales y clínicas privadas" ~ "Tarjeta Exclusiva",
             Preferencia == "Ninguno de los dos" ~ "Ninguna",
             TRUE ~ as.character(Preferencia)
           ))

#### Opinión de que la coperativa tenga una clínica privada
data %>% 
  count(NCL1)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

#### Opinión del lanzamiento de una tarjeta para seguros de salud
data %>% 
  count(Tarj1)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

################ Preferencia a servicios médicos
data %>% 
  count(Preferencia)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

################# Preferencia según los índices analizados
data %>% 
  summarise(mean(indexAccess),mean(indexConfidence),mean(indexScoreCCSS),mean(indexPrivateUse),mean(indexDispositionClinic),mean(indexDispositionCard))

data %>% 
  group_by(Preferencia) %>% 
  summarise(mean(indexAccess),mean(indexConfidence),mean(indexScoreCCSS),mean(indexPrivateUse),mean(indexDispositionClinic),mean(indexDispositionCard))

################# Preferencia en cuanto a tipo de servicio por zona de residencia
data %>% 
  count(residencia)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

pref<-data %>%
  count(Preferencia, residencia) %>% 
  mutate(per=n/nrow(data)*100)

ggplot(pref, aes(x = reorder(Preferencia,-n), weight = n, fill = residencia)) + 
  geom_bar() +
  labs(x = "Preferencia de servicio médico", y = "Cantidad de agremiados") +
  scale_fill_manual(values = list(color = brewer.pal(3, palett))$color[1:2], name = "Residencia") +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  theme(plot.caption = element_text(vjust = 2)) +labs(caption = fuente)

################# Preferencia en cuanto a tipo de servicio por sexo
data %>% 
  count(Sexo)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

sex<-data %>%
  count(Preferencia, Sexo) %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

ggplot(sex, aes(x = reorder(Preferencia,-n), weight = n, fill = Sexo)) + 
  geom_bar() +
  labs(x = "Preferencia de servicio médico", y = "Cantidad de agremiados") +
  scale_fill_manual(values = list(color = brewer.pal(3, palett))$color[1:2]) +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  theme(plot.caption = element_text(vjust = 2)) +labs(caption = fuente)


################# Preferencia en cuanto a ingreso familiar
data %>% 
  count(Ingfam)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

ing<-data %>%
  count(Preferencia, Ingfam)

################# Preferencia en frecuencia de uso
data %>% 
  count(Frecuso)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

frequso<-data %>%
  count(Preferencia, Frecuso)

ggplot(frequso, aes(x = reorder(Preferencia,-n), weight = n, fill = Frecuso)) + 
  geom_bar() +
  labs(x = "Preferencia de servicio médico", y = "Cantidad de agremiados") +
  scale_fill_manual(values = list(color = brewer.pal(4, palett))$color[1:4]) +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  theme(plot.caption = element_text(vjust = 2)) +labs(caption = fuente)

###### K Means ######

tarjeta = data[data$Preferencia == "Tarjeta Exclusiva", ]
clinica = data[data$Preferencia == "Clínica Propia", ]
ninguno = data[data$Preferencia == "Ninguna",]



#### PCA ####
### TARJETA ###
# disposition to card index was not included because they already prefer the card project 
res.pca <- PCA(scale(tarjeta[,67:71]),  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
####     ####

#best number of k clusters
res.nbclust <- NbClust(scale(tarjeta[,67:72]), distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + labs(x = "Número de clústeres", y = "Cantidad de índices") +theme_bw() + ggtitle("")
ggsave("./indices_cluster.png", units = "cm", height = 8, width = 15.5)

res.nbclust$All.index

# disposition to card index was not included because they already prefer the card project 
km3 = kmeans(scale(tarjeta[,67:71]), centers = 3, nstart = 100)
km3$centers
km3$size
tarjeta$cluster = km3$cluster
colnames(tarjeta[,67:71])
centros = tarjeta %>% group_by(cluster) %>% summarize(Access = mean(indexAccess),
                                                          Confidence = mean(indexConfidence),
                                                          ScoreCCSS = mean(indexScoreCCSS),
                                                          PrivateUse = mean(indexPrivateUse),
                                                          DispositionClinic = mean(indexDispositionClinic))
centros
means = colMeans(tarjeta[,67:71])

write.table(centros, "clipboard", sep = ",")
write.table(means, "clipboard", sep = ",")

p3 <- fviz_cluster(km3, data = scale(tarjeta[,67:71]), frame.type = "convex") +
  theme_bw() + ggtitle("") 
p3

write.table(table(tarjeta$Ingfam, tarjeta$cluster), "clipboard", sep = ",")
table(clinica$Ingfam)
table(ninguno$Ingfam)

write.table(table(tarjeta$residencia, tarjeta$cluster), "clipboard", sep = ",")
table(clinica$residencia)
table(ninguno$residencia)

write.table(table(tarjeta$Frecuso, tarjeta$cluster), "clipboard", sep = ",")
table(clinica$Frecuso)
table(ninguno$Frecuso)


### Clínica ###
# disposition to card index was not included because they already prefer the card project 
res.pca <- PCA(scale(clinica[,c(67:70,72)]),  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
####     ####

#best number of k clusters
res.nbclust <- NbClust(scale(clinica[,c(67:70,72)]), distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")
factoextra::fviz_nbclust(res.nbclust) + labs(x = "Número de clústeres", y = "Cantidad de índices") +theme_bw() + ggtitle("")
ggsave("./indices_cluster.png", units = "cm", height = 8, width = 15.5)

res.nbclust$All.index

# disposition to card index was not included because they already prefer the card project 
km3 = kmeans(scale(clinica[,c(67:70,72)]), centers = 3, nstart = 100)
km3$centers
km3$size
clinica$cluster = km3$cluster
colnames(clinica[,c(67:70,72)])
centros = clinica %>% group_by(cluster) %>% summarize(Access = mean(indexAccess),
                                                      Confidence = mean(indexConfidence),
                                                      ScoreCCSS = mean(indexScoreCCSS),
                                                      PrivateUse = mean(indexPrivateUse),
                                                      DispositionCard = mean(indexDispositionCard))
centros
means = colMeans(clinica[,c(67:70,72)])

write.table(centros, "clipboard", sep = ",")
write.table(means, "clipboard", sep = ",")

p3 <- fviz_cluster(km3, data = scale(clinica[,c(67:70,72)]), frame.type = "convex") +
  theme_bw() + ggtitle("") 
p3

write.table(table(clinica$Ingfam, clinica$cluster), "clipboard", sep = ",")
table(tarjeta$Ingfam)
table(ninguno$Ingfam)

write.table(table(clinica$residencia, clinica$cluster), "clipboard", sep = ",")
table(tarjeta$residencia)
table(ninguno$residencia)

write.table(table(clinica$Frecuso, clinica$cluster), "clipboard", sep = ",")
table(tarjeta$Frecuso)
table(ninguno$Frecuso)