#Instalacion paquetes utilizados 
install.packages("tidyverse")
install.packages("DBI")
install.packages("RSQLite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stats")

#Llamas paquetes
library(tidyverse)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(stats)

#Coneccion SQLite
datos <- dbConnect(SQLite(), dbname = "C:/Users/gaby_/Documents/ADRES/ADRES.db")
dbListTables(datos)

#Llamas tablas 
clpr <- tbl(datos, "Clpr") %>% collect()
departamento <- tbl(datos, "Departamento") %>% collect()
entidad <- tbl(datos, "Entidad") %>% collect()
municipio <- tbl(datos, "Municipio") %>% collect()
personas <- tbl(datos, "Personas") %>% collect()
prestadores <- tbl(datos, "Prestador") %>% collect()
region <- tbl(datos, "Region") %>% collect()
habilitacion <- tbl(datos, "Habilitacion") %>% collect()

#Analisis Descriptivo
################################################################################
#CLPR

summary(clpr)
#Todas las variables de clpr son tipo caracter, ninguna tiene datos faltantes
#Se sugiera una transformaciona tipo numerico para un analisis

# Verificar los valores únicos de las variables antes de la conversión
unique(clpr$COD_HABILITACION)
unique(clpr$COD_CLPR)
unique(clpr$COD_TIPO_PERSONA)
unique(clpr$COD_TIPO_ENTIDAD)

# Convertir variables, si los valores son adecuados para ser tratados como números
clpr$COD_HABILITACION <- as.numeric(clpr$COD_HABILITACION)
clpr$COD_CLPR <- as.numeric(clpr$COD_CLPR)
clpr$COD_TIPO_PERSONA <- as.numeric(clpr$COD_TIPO_PERSONA)
clpr$COD_TIPO_ENTIDAD <- as.numeric(clpr$COD_TIPO_ENTIDAD)

#conteo por codigo clpr
# 1 -> Instituciones Prestadoras de Servicios de Salud - IPS
# 2 -> Profesional Independiente
# 3 -> Transporte Especial de Pacientes
# 4 -> Objeto Social Diferente a la Prestación de Servicios de Salud
conteo_cod_clpr <- table(clpr$COD_CLPR)

#histograma codigo clpr
tipo_clpr <- as.numeric(clpr$COD_CLPR)
histograma_clpr <- ggplot(clpr, aes(x = COD_CLPR)) +
  geom_bar(fill = "blue") +
  labs(x = "Codigo CLPR", y = "Frecuencia", title = "Histograma de CLPR")

#descargar histograma
ggsave("histograma_clpr.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

#conteo por tipo de persona
# 1 -> Natural
# 2 -> Juridica
conteo_tipo_persona <- table(clpr$COD_TIPO_PERSONA)
#histograma tipo de persona
tipo_persona <- as.numeric(clpr$COD_TIPO_PERSONA)
histograma_tipo_persona <- histograma_ggplot(clpr, aes(x = tipo_persona)) +
  geom_bar(fill = "blue") +
  labs(x = "Tipo de Persona", y = "Frecuencia", title = "Histograma de Tipo de Persona")
#descargar histograma
ggsave("histograma_tipo_persona.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

#conteo codigo entidad
# 1 -> Privada
# 2 -> Publica
# 3 -> Mixta
conteo_cod_entidad <- table(clpr$COD_TIPO_ENTIDAD)

#histograma codigo tipo entidad
hitograma_tipo_entidad <- ggplot(clpr, aes(x = COD_TIPO_ENTIDAD)) +
  geom_bar(fill = "blue") +
  labs(x = "Tipo Entidad", y = "Frecuencia", title = "Histograma de Tipo de Entidad")

#descargar histograma
ggsave("histograma_tipo_entidad.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

################################################################################
#HABILITACION 
summary(habilitacion)

#convertir las fechas al formato de fecha estándar de R usando as.POSIXct()
habilitacion$FECHA_RADICACION <- as.Date(as.character(habilitacion$FECHA_RADICACION), "%Y%m%d")
habilitacion$FECHA_VENCIMIENTO <- as.Date(as.character(habilitacion$FECHA_VENCIMIENTO), "%Y%m%d")

# Calcular los años entre la fecha de radicación y la fecha de vencimiento
habilitacion$AÑOS_HABILITADOS <- as.numeric(habilitacion$FECHA_VENCIMIENTO - habilitacion$FECHA_RADICACION) / 365

#histograma años habilitados
histograma_años_habilitados <- ggplot(habilitacion, aes(x = AÑOS_HABILITADOS)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Años Habilitados", y = "Frecuencia", title = "Histograma de Años Habilitados")

#descargar histograma
ggsave("histograma_años_habilitados.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

################################################################################
#PRESTADORES
#agrupar cuantos codigos habilitados hay por municipio
habilitados_municipio <- prestadores %>%
  group_by(ID_MUNICIPIO) %>%
  summarize(total_cod_habilitacion = n_distinct(COD_HABILITACION))

#histograma habilitados por municipio(10primeros)

#ordenar los municipios por la cantidad total de códigos de habilitación
habilitados_municipio_ordenados <- habilitados_municipio %>% 
  arrange(desc(total_cod_habilitacion))

#seleccionar los primeros 10 municipios
top_10_municipios <- head(habilitados_municipio_ordenados, 10)

#crear el gráfico de barras con los 10 primeros municipios
histograma_habilitados_municipios <- ggplot(top_10_municipios, aes(x = reorder(ID_MUNICIPIO, -total_cod_habilitacion), y = total_cod_habilitacion)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  labs(x = "ID de Municipio", y = "Total de códigos de habilitación", title = "Top 10 municipios con mayor cantidad de códigos de habilitación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#descargar histograma
ggsave("histograma_habilitados_municipios.png", plot = last_plot(), width = 6, height = 4, dpi = 300)
################################################################################
#HABILITADOS POR MUNICIPIO
#unir tablas prestadroes y habilitacion 
union_1 <- merge(prestadores, habilitacion,by = "COD_HABILITACION")
union_1 <- subset(union_1, select = -c(5, 6))

summary(union_1)

#boxplot años habilitados 
años_habilitados <- ggplot(union_1, aes(y = AÑOS_HABILITADOS)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(y = "Años Habilitados", title = "Boxplot de Años Habilitados")

#descargar boxplot
años_habilitados <- ggsave("años_habilitados.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

################################################################################

dbDisconnect(datos)