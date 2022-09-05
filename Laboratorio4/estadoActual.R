library(readr)
library(tidyverse)
library(highcharter)
library(splitstackshape)
library(dplyr)
library(ggplot2)

df <- read_delim("tabla_completa.csv",
                 ",", escape_double = FALSE, trim_ws = TRUE)
df

df$CLIENTE <- str_replace(df$CLIENTE, '/', '|||')

sdf <- cSplit(df, 'CLIENTE', '|')
sdf

Newdf<- (splitdf %>% 
           select(-12,-13,-15,-16,))
Newdf

# rename de columnas
names(Newdf)[11]<- "CLIENTE"
names(Newdf)[12]<- "MOTIVO"
names(Newdf)[13]<- "OTRO"
Newdf

# quitar ñ y letras no ingles
df <-mutate_if(Newdf, is.character, as.factor)
df$CLIENTE <- iconv(Newdf$CLIENTE, to = "UTF-8")
df$PILOTO <- iconv(Newdf$PILOTO, to = "UTF-8")
df$UNIDAD <- iconv(Newdf$UNIDAD, to = "UTF-8")
Newdf <-mutate_if(Newdf, is.character, as.factor)
Newdf$CLIENTE <- iconv(Newdf$CLIENTE, to = "UTF-8")
Newdf$PILOTO <- iconv(Newdf$PILOTO, to = "UTF-8")
Newdf$UNIDAD <- iconv(Newdf$UNIDAD, to = "UTF-8")

Newdf$MOTIVO<-Newdf$MOTIVO %>%
  replace_na('Despacho a cliente')
Newdf$MOTIVO <- str_replace(Newdf$MOTIVO, 'FALTANTE', 'Faltante')

EnviosporMes <- df %>%
    select(COD_VIAJE, MES) %>%
    group_by(MES) %>%
    summarise(viajes = n_distinct(COD_VIAJE)) %>%
    hchart("column", hcaes(x =  MES, y = viajes)) %>%
    hc_title(text = "<b>Cantidad de Viajes por Mes en el 2017</b>") %>%
    hc_subtitle(text = "<i>Mayo fue el mes con mayor cantidad de viajes, mientras que marzo fue el mes con menor cantidad de viajes.</i>")
#EnviosporMes

Ubicaciones <- df %>%
  select(COD_VIAJE, UBICACION) %>%
  group_by(UBICACION) %>%
  summarise(viajes = n_distinct(COD_VIAJE)) %>%
  hchart("bar", hcaes(x =  UBICACION, y = viajes), name = "cantidad de envios") %>%
  hc_title(text = "<b>Cantidad de Viajes por ubicacion</b>") %>%
  hc_subtitle(text = "<i>La Ubicacion de 76002 realizo una mayor cantidad de envios.</i>")
#Ubicaciones

TransporteRecurrido <- df %>%
  select(COD_VIAJE, UNIDAD) %>%
  group_by(UNIDAD) %>%
  summarise(viajes = n_distinct(COD_VIAJE)) %>%
  hchart("bar", hcaes(x =  UNIDAD, y = viajes), name = "cantidad de envios") %>%
  hc_title(text = "<b>Cantidad de Viajes por Unidad de Transporte</b>") %>%
  hc_subtitle(text = "<i>La Ubicacion de 76002 realizo una mayor cantidad de envios.</i>")
#TransporteRecurrido

IngresoAlMes <- (Newdf %>% 
                  group_by(MES) %>%
                  filter(MOTIVO != "DEVOLUCION") %>% 
                  summarise_at(vars(Q),
                               list(Q = sum)))

IngresoAlMes  %>%
  hchart("column", hcaes(x =  MES, y = Q)) %>%
  hc_title(text = "<b>Ingresos de Distribuidora al Mes</b>") %>%
  hc_subtitle(text = "<i>El mes con mayor cantidad de ingresos fue Mayo y el mes con menor ingreso fue marzo.</i>")

ViajeporPiloto <- Newdf %>%
  group_by(MES, PILOTO) %>% 
  summarise(viajes=n()) 
  ggplot(ViajeporPiloto, aes(fill=MES, y=ViajeporPiloto$PILOTO, x=ViajeporPiloto$viajes)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Cantidad de Viajes")+
  ylab("Piloto")+
  ggtitle("Cantidad de Viajes por Piloto")

TipoDeViaje <- Newdf %>%
  group_by(UNIDAD, MOTIVO) %>% 
  summarise(motivo=n())
ggplot(TipoDeViaje, aes(fill=MOTIVO, y=TipoDeViaje$motivo, x=TipoDeViaje$UNIDAD)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("Unidad")+
  ylab("Cantidad de viajes")+
  ggtitle("Tipo de viaje por Unidad de Transporte")

CreditoPorcliente <- Newdf %>%
                      group_by(CLIENTE)  %>% 
                      summarise(credi=sum(CREDITO))
CreditoPorcliente$CLIENTE <- str_sub(CreditoPorcliente$CLIENTE, 1, 9)
ggplot(Credito_cliente, aes(y=credi, x=CLIENTE)) + 
  geom_bar(position="stack", stat="identity", fill="#42A7E3")+
  xlab("Cliente")+
  ylab("Credito Acumulado")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Credito por cliente")

IngresoPorcliente<-(Newdf %>%
  group_by(CLIENTE) %>%
  filter(MOTIVO != "DEVOLUCION") %>% 
  summarise_at(vars(Q),
               list(Q = sum)) %>%
  arrange(-Q)) %>%
  hchart("column", hcaes(x =  CLIENTE, y = Q)) %>%
  hc_title(text = "<b>Ingresos por Cliente</b>") %>%
  hc_subtitle(text = "<i>El cliente que brinda los mayores ingresos es El Pinche Obelisco y cliente con menor aporte a ingresos es Hospital Las Americas.</i>")
IngresoPorcliente



