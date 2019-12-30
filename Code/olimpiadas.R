
## ----------------------------------------------------------------------------------
## SCRIPT: olimpiadas.R
## ASIGNATURA: Tipología y ciclo de vida de los datos
## AUTORES: Manuel Betancor y María Navalón
## PAQUETES NECESARIOS: dplyr, modeest
## ----------------------------------------------------------------------------------

# Inicialización de librerías

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

if(!require("modeest")){
  install.packages("modeest")
  library("modeest")
}


## ----------------------------------------------------------------------------------
##                               CARGA DE LOS DATOS
## ----------------------------------------------------------------------------------

# Los ficheros csv a importar se han adquirido a través de los siguientes enlaces:
#  https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
#  https://www.kaggle.com/statchaitya/country-to-continent
#  https://www.downloadexcelfiles.com/wo_en/download-excel-file-list-olympic-host-cities
# Para la carga se va a utilizar enlaces suministrados por dropbox de los archivos

atletas <- read.csv("https://www.dropbox.com/s/8ogvaf0ipu4raby/athlete_events.csv?dl=1", encoding="utf-8")
paises <- read.csv("https://www.dropbox.com/s/qol9t3g4z359img/countryContinent.csv?dl=1", encoding="utf-8")
sedes <- read.csv("https://www.dropbox.com/s/6fhwd3ydoeop30j/list-host-cities-olympic-943j.csv?dl=1", encoding="utf-8")


## ----------------------------------------------------------------------------------
##                         ANÁLISIS PRELIMINAR DE LOS DATOS
## ----------------------------------------------------------------------------------

# Revisamos la estructura de los dataframes para comprobar el tipo de datos de cada atributo
str(atletas)
str(paises)
str(sedes)

# Exploramos los dataframes creados visualizando las primeras filas
head(atletas)
head(paises)
head(sedes)

# Verificamos si hay registros duplicados
anyDuplicated(sedes)
anyDuplicated(paises)
anyDuplicated(atletas)

# Comprobamos que los códigos de país son todos distintos en el dataframe paises
# para garantizar la correcta integración posterior de los datos ya que ese campo
# se utilizará como identificador único del conjunto
paises %>% distinct(NOC)


## ----------------------------------------------------------------------------------
##                           SELECCIÓN DE LOS DATOS
## ----------------------------------------------------------------------------------

#Eliminamos las columnas no utilizadas en los análisis para simplificar los datasets
atletas <- atletas[,c("NOC","Year","Season","City","Sport","Event","Medal")]
paises <- paises[,c("country","code_3","continent","sub_region")]
sedes <- sedes[,c("City","Country","Continent","Year")]

# En el dataframe sedes hay que eliminar los 3 últimos registros ya que son basura
sedes <- sedes[1:(nrow(sedes)-3),]


## ----------------------------------------------------------------------------------
##                         TRATAMIENTO DE DATOS PERDIDOS
## ----------------------------------------------------------------------------------

# En el dataframe de sedes encontramos valores no definidos NA en el atributo Year.
# Esto ocurre cuando en un mismo año coindicen las ediciones de invierno y verano, por
# lo que imputamos el año del registro inmediamente anterior que contiene el dato
# exacto que corresponde a esa edición.

for(i in 3:nrow(sedes)){
  if (is.na(sedes[i,"Year"])) {
    sedes[i,"Year"] <- sedes[i-1,"Year"]
  }
}

## ----------------------------------------------------------------------------------
##                              DETECCIÓN DE OUTLIERS
## ----------------------------------------------------------------------------------

# Lo que nos interesa es el número de medallas por país por lo que agrupamos
# por ese campo y representamos los datos mediante gráficos de cajas (boxplots)
# con el objetivo de detectar outliers en este nuevo campo calculado

by_NOC <- atletas %>% group_by(NOC) %>% summarise(count=n())
boxplot(by_NOC$count)

# Realizamos la misma representación para el total de medallas de oro, plata y bronce

by_NOC_Medal <- atletas %>% group_by(NOC, Medal) %>% summarise(count=n())
by_NOC_Medal_Gold <- by_NOC_Medal %>% filter(Medal == "Gold")
by_NOC_Medal_Silver <- by_NOC_Medal %>% filter(Medal == "Silver")
by_NOC_Medal_Bronze <- by_NOC_Medal %>% filter(Medal == "Bronze")

boxplot(by_NOC_Medal_Gold$count)
boxplot(by_NOC_Medal_Silver$count)
boxplot(by_NOC_Medal_Bronze$count)


## ----------------------------------------------------------------------------------
##                              INTEGRACIÓN DE LOS DATOS
## ----------------------------------------------------------------------------------

# Unificamos el nombre de los atributos para simplificar la unión de los dataframes 
# con la función merge. Mantenemos todos los registros del primer dataframe.
# Hemos generado un diccionario que relaciona los códigos NOC con los códigos code_3

diccionario <- read.csv("https://www.dropbox.com/s/gibjji4okfhl0ru/diccionario.csv?dl=1", encoding="utf-8")

names(paises) <- c("country","NOC","continent","sub_region")
names(diccionario) <- c("NOC","country","code_3")
names(sedes) <- c("City","Country_host","Continent_host","Year")

df <- merge(atletas, diccionario, by = "NOC", all.x=TRUE) %>% 
merge(paises, by = "country", all.x=TRUE) %>% 
merge(sedes, by = c("City","Year"), all.x=TRUE)


## ----------------------------------------------------------------------------------
##                         ANÁLISIS ESTADÍSTICO DESCRIPTIVO
## ----------------------------------------------------------------------------------


# Ranking de países por número de medallas
summary(df$country)

# Número de medallas de cada tipo por país
tapply(df$country, df$Medal, summary)

# Listado de paises con sus medallas por tipo
tapply(df$Medal, df$country, summary)

# Calculamos la moda del atributo "Medal" (tipo de medalla) para todo el conjunto
mlv(df$Medal, na.rm=TRUE)

# Calculamos la moda del atributo "Medal" (tipo de medalla) para cada país
tapply(df$Medal, df$country, mlv, na.rm=TRUE)

# Calculamos la moda del par país-medalla
apply(df[,c("country", "Medal")], 2, mlv,  method = "mfv", na.rm=TRUE)



