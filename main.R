# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
library(extrafont) 
library(RColorBrewer)


# Read data
data<- read.spss("./data/caso3.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")
summary(data)
names(data)
view(data)

data<-as_tibble(data)
palett<-"Dark2"
fuente<-"Fuente: Encuesta sobre preferencia de servicios médicos, 2012"

################# Limpieza y transformación de data
data<-data %>%
  mutate(Preferencia=
           case_when(
             Preferencia == "La clínica propia de la cooperativa" ~ "Clínica Propia",
             Preferencia == "La tarjeta exclusiva de la cooperativa con descuentos especiales en los hospitales y clínicas privadas" ~ "Tarjeta Exclusiva",
             Preferencia == "Ninguno de los dos" ~ "Ninguna",
             TRUE ~ as.character(Preferencia)
           ))

## Access data
levels_ACC = levels(data$ACC1)
for (i in 2:9){
  data[,i] = as.numeric(data[,i])
}
head(data[,2:9])


## Confidence data
levels_CON = levels(data$CON8)
for (i in 10:17){
  data[,i] = as.numeric(data[,i])
}
head(data[,10:17])


################ Preferencia a servicios médicos
data %>% 
  count(Preferencia)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

################# Preferencia en cuanto a tipo de servicio por zona de residencia
data %>% 
  count(residencia)  %>% 
  mutate(per=n/nrow(data)*100) %>% 
  arrange(per)

pref<-data %>%
  count(Preferencia, residencia)

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
  count(Preferencia, Sexo)

ggplot(sex, aes(x = reorder(Preferencia,-n), weight = n, fill = Sexo)) + 
  geom_bar() +
  labs(x = "Preferencia de servicio médico", y = "Cantidad de agremiados") +
  scale_fill_manual(values = list(color = brewer.pal(3, palett))$color[1:2]) +
  theme(text = element_text(size=10, family="LM Roman 10")) +
  theme(plot.caption = element_text(vjust = 2)) +labs(caption = fuente)

