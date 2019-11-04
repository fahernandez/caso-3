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
