# Read data
library(foreign)
library(tibble)
library(tidyverse)
library(ggplot2)
library(extrafont) 
library(RColorBrewer)


# Read data
#setwd("C:/Users/abrenes/Documents/GitHub/caso-3")
data<- read.spss("./data/caso3.sav", to.data.frame=TRUE, use.value.labels = TRUE)
attr(data, "variable.labels")[44:51]
summary(data)
names(data)
#view(data)
table(data$Preferencia)
table(data$ACC1)
head(as.numeric(data$ACC1),17)



#data[,2:9] = apply(data[,2:9], MARGIN = 2, FUN = as.numeric)
## Access data
levels_ACC = levels(data$ACC1)
for (i in 2:9){
  data[,i] = as.numeric(data[,i])
}


## COnfidence data
levels_CON = levels(data$CON8)
for (i in 10:17){
  data[,i] = as.numeric(data[,i])
}
head(data[,10:17])

