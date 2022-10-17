library(readr)
library(tidyverse)
library(highcharter)
library(splitstackshape)
library(dplyr)
library(ggplot2)
library(lubridate)

#### carga de datos ####

data <- read_csv('c1.csv')

#### limpieza de datos modo tidy ####

data <- data %>% 
  pivot_longer(c("Camion_5","Pickup","Moto"),
               names_to = "Vehiculo", values_to = "Costo")

data <- data %>% 
  pivot_longer(c("fijoCamion_5","fijoPickup","fijoMoto"),
               names_to = "Fijo", values_to = "CostoFijo")

data <- data %>% 
  pivot_longer(c("directoCamion_5","directoPickup","directoMoto"),
               names_to = "Directo", values_to = "CostoDirecto")

data<- data %>% 
  pivot_longer(c("5-30","30-45","45-75","75-120","120+"),
               names_to = "DistanciaRecorrida", values_to = "X" )

#### eliminar datos NA que no le agregan valor a los datos ####

data[data== "Q-"]<-NA

data <- data[!is.na(data$Costo),]
data <- data[!is.na(data$CostoFijo),]
data <- data[!is.na(data$CostoDirecto),]
data <- data[!is.na(data$X),]

####  vuelvo solo numericos los costos y factura para mejor manejo de info ####

data$factura <- str_remove(data$factura, "Q")
data$factura <- as.numeric(data$factura)
data$CostoDirecto<-str_remove(data$CostoDirecto, "Q")
data$CostoDirecto<-as.numeric(data$CostoDirecto)
data$CostoFijo<-str_remove(data$CostoFijo, "Q")
data$CostoFijo<-as.numeric(data$CostoFijo)
data$Costo<-str_remove(data$Costo, "Q")
data$Costo<-as.numeric(data$Costo)

#### elimino columnas que no me aportan valor  ####

data<-data %>% 
  select(-Fijo,-Directo,-...23,-...24,-...25,-...26,-...27,-...28,-X)

#### estado actual de la empresa ####

# cantidad de veces que se ha realizado alguna operacion a cada poste
noPostes <- data %>%
  count(ID)
noPostes

# cantidad de veces que se ha realizado cada operacion
tipoOperacion <- data %>%
  count(Cod)
tipoOperacion

# cantidad de veces que se ha originado una operacion por cada centro de distribucion
centrosDistribucion <- data %>%
  count(origen)
centrosDistribucion

# ganancia segun cada operacion realizada
data$Ganancias <- data$factura - data$Costo

# ganancia total 2017
gananciaTotal <- sum(data$Ganancias)

# operaciones por mes
data$Mes <- month(data$Fecha)
operacionesMes <- data %>%
  select(Mes, ID) %>%
  group_by(Mes)%>%
  summarise(operaciones = n()) %>%
  hchart("column", hcaes(x =  Mes, y = operaciones)) %>%
  hc_title(text = "<b>Cantidad de Operaciones por Mes en el 2017</b>") %>%
  hc_subtitle(text = "<i>El mes mas recurrido de operaciones fue Octubre. El mes menos recurrido fue Febrero.</i>")
operacionesMes

# tipo de operaciones
gtipoOperacion <- tipoOperacion %>%
  select(Cod, n)%>%
  hchart("column", hcaes(x =  Cod, y = n)) %>%
  hc_title(text = "<b>Cantidad de Operaciones por tipo de servicio.</b>") %>%
  hc_subtitle(text = "<i>El servicio que mas se realiza es Revision. El servicio que menos se realiza es Cambio de Puentes.</i>")
gtipoOperacion

# tipo de operaciones por mes
tipoOperacionMes <- data %>%
  select(Mes, Cod) %>%
  group_by(Mes,Cod)%>%
  summarise(operaciones = n())

ggplot(tipoOperacionMes, aes(fill=Cod, y=tipoOperacionMes$operaciones, x=tipoOperacionMes$Mes)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  xlab("Mes")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Tipo de servicio realizado por mes.")

# postes mas problematicos
noPostes$ID <- as.character(noPostes$ID)
problematicos <- noPostes %>%
  select(ID, n) %>%
  arrange(-n) %>%
  top_n(5)%>%
  hchart("column", hcaes(x =  ID, y = n)) %>%
  hc_title(text = "<b>Top 5 postes mas problematicos.</b>") %>%
  hc_subtitle(text = "<i>El poste con mas servicios realizados durante el año 2017 fue 477971.</i>")
problematicos

aveProblemas <- mean(noPostes$n)

# postes problematicos segun 
completo <- data %>%
  select(ID,Cod)%>%
  group_by(ID,Cod)%>%
  summarise(operaciones = n())%>%
  filter(ID %in% c(337161,477971,773607,863979,969156))

completo$ID <- as.character(completo$ID)
ggplot(completo, aes(fill=Cod, y=completo$operaciones, x=completo$ID)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Poste")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Tipo de servicio realizado por poste problematico.")
  

# Recurrencia centros de distribucion
recurrencia <- centrosDistribucion %>%
  select(origen, n)
recurrencia$origen <- as.character(recurrencia$origen)
ggplot(recurrencia, aes(y=n, x=origen)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Centro de Distribucion")+
  ylab("Origen de Operaciones")+
  ggtitle("Recurrencia de Operaciones por Centro de Distribucion.")

# cod por centro de distribucion
data$origen <- as.character(data$origen)
codCentro <- data %>%
  select(origen, Cod) %>%
  group_by(origen,Cod) %>%
  summarise(operaciones = n())

ggplot(codCentro, aes(fill=Cod, y=operaciones, x=origen)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Centro de Origen")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Tipo de Servicio realizado por Centro de Distribucion.")
  
# vehiculo mas recurrido
vehiculos <- data %>%
  select(Vehiculo, ID) %>%
  group_by(Vehiculo)%>%
  summarise(operaciones = n())

ggplot(vehiculos, aes(y=operaciones, x=Vehiculo)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Vehiculo")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Recurrencia de Opereaciones por tipo de Vehiculo.")

# vehiculo segun distancias
distanciaVehiculo <-  data %>%
  select(Vehiculo,DistanciaRecorrida, ID)%>%
  group_by(Vehiculo,DistanciaRecorrida)%>%
  summarise(operaciones = n())

ggplot(distanciaVehiculo, aes(fill=DistanciaRecorrida, y=operaciones, x=Vehiculo)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Vehiculo")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Distancias recurridas segun cada tipo de vehiculo.")

# vehiculo por cod
codVehiculo <- data %>%
  select(Vehiculo, Cod) %>%
  group_by(Vehiculo, Cod) %>%
  summarise(operaciones = n())

ggplot(codVehiculo, aes(fill=Cod, y=operaciones, x=Vehiculo)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Vehiculo")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Tipo de operaciones realizadas segun cada tipo de vehiculo.")


# Contabilidad total anual
Contabilidad <- data %>%
  summarise(totalFacturacion = sum(factura),
            totalCosto = sum(Costo),
            totalCostoD = sum(CostoDirecto),
            totalCostoF = sum(CostoFijo),
            totalGanancia = sum(Ganancias))

Contabilidad$MargenAnual <- Contabilidad$totalGanancia/Contabilidad$totalFacturacion
Contabilidad$MargenAnual <- formattable::percent(Contabilidad$MargenAnual)

# facturacion mensual
mensual <- data %>%
  select(Mes, factura) %>%
  group_by(Mes) %>%
  summarise(facturacion = sum(factura))

ggplot(mensual, aes(y=facturacion, x=Mes)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  scale_y_continuous(breaks = seq(0, 3200000, by = 500000))+
  xlab("Mes")+
  ylab("Cantidad Facturada")+
  ggtitle("Cantidad facturada por mes operativo.")

# ganancias mensuales
gananciasmensuales <- data %>%
  select(Mes, Ganancias) %>%
  group_by(Mes) %>%
  summarise(gananciasM = sum(Ganancias))

ggplot(gananciasmensuales, aes(y=gananciasM, x=Mes)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000))+
  xlab("Mes")+
  ylab("Ganancias")+
  ggtitle("Ganancias por mes operativo.")


# ganancias por Cod
gananciasCod <- data %>%
  select(Ganancias, Cod) %>%
  group_by(Cod)%>%
  summarise(gananciascod = sum(Ganancias)) %>%
  hchart("column", hcaes(x =  Cod, y = gananciascod)) %>%
  hc_title(text = "<b>Ganancias Segun Tipo de Servicio Realizado.</b>") %>%
  hc_subtitle(text = "<i>El tipo de servicio que genera mas ganancias es la Revision.</i>")
gananciasCod

#ganancias por vehiculo
gananciaVehiculo <- data %>%
  select(Vehiculo, Ganancias) %>%
  group_by(Vehiculo) %>%
  summarise(gananciasv = sum(Ganancias)) %>%
  hchart("column", hcaes(x =  Vehiculo, y = gananciasv)) %>%
  hc_title(text = "<b>Ganancias Segun medio de Transporte.</b>") %>%
  hc_subtitle(text = "<i>El tipo de vehiculo que mas ganancias aporta es el Pickup.</i>")
gananciaVehiculo

# vehiculo por centro
vehiculocentro <- data %>%
  select(Vehiculo, origen, Cod)%>%
  group_by(Vehiculo, origen)%>%
  summarise(codcentro = n())%>%
  filter(origen %in% c('150224','150277','150278','150841'))

ggplot(vehiculocentro, aes(fill=Vehiculo, y=codcentro, x=origen)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Centro de Origen")+
  ylab("Cantidad de Operaciones")+
  ggtitle("Vehiculos utilizados en operaciones por Centro de Distribucion")

  