
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

if(!require("MASS")){
  install.packages("MASS")
  library("MASS")
}

if(!require("modeest")){
  install.packages("modeest")
  library("modeest")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
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
sapply(atletas, function(x) class(x))
str(paises)
sapply(paises, function(x) class(x))
str(sedes)
sapply(sedes, function(x) class(x))

# Exploramos los dataframes creados visualizando las primeras filas
head(atletas)
head(paises)
head(sedes)

# Verificamos si hay registros duplicados
anyDuplicated(sedes)
anyDuplicated(paises)
anyDuplicated(atletas)

# Comprobamos que los paises son todos distintos en el dataframe paises para garantizar
# la correcta integración posterior de los datos ya que ese campo se utilizará como 
# identificador único del conjunto
paises %>% distinct(country)


## ----------------------------------------------------------------------------------
##                      DETECCIÓN Y TRATAMIENTO DE DATOS PERDIDOS
## ----------------------------------------------------------------------------------

## Dataframe atletas
summary(atletas)
summary(as.factor(atletas$Medal)) # detalle del campo con NA's

# En el dataframe atletas encontramos 231.333 valores no definidos (NA's) en el atributo Medal
# esto es debido a que en el dataframe se recogen todos los participantes, hayan o no obtenido medallas
# por lo que hay que eliminar los registros, al no considerarse datos perdidos, sino registros nulos
# para el cálculo estadístico que persigue esta práctica

atletas <- atletas[!is.na(atletas$Medal),]

## Dataframe paises
summary(paises)

## Dataframe sedes
summary(sedes)
summary(as.factor(sedes$Year)) # detalle del campo con NA's

# En el dataframe de sedes encontramos 18 valores no definidos (NA's) en el atributo Year.
# Esto ocurre cuando en un mismo año coindicen las ediciones de invierno y verano, por
# lo que imputamos el año del registro inmediamente anterior que contiene el dato
# exacto que corresponde a esa edición.

for(i in 3:nrow(sedes)){
  if (is.na(sedes[i,"Year"])) {
    sedes[i,"Year"] <- sedes[i-1,"Year"]
  }
}

any(is.na(sedes$Year))
summary(as.factor(sedes$Year)) 


## ----------------------------------------------------------------------------------
##                              DETECCIÓN DE OUTLIERS
## ----------------------------------------------------------------------------------

# Lo que nos interesa es el número de medallas por país por lo que agrupamos
# por ese campo y representamos los datos mediante gráficos de cajas (boxplots)
# con el objetivo de detectar outliers en este nuevo campo calculado

by_NOC <- atletas %>% group_by(NOC) %>% summarise(count=n())
boxplot(by_NOC$count)
boxplot.stats(by_NOC$count)$out

# Realizamos la misma representación diferenciando por tipo de medalla
# de oro, plata y bronce

by_NOC_Medal <- atletas %>% group_by(NOC, Medal) %>% summarise(count=n())
by_NOC_Medal_Gold <- by_NOC_Medal %>% filter(Medal == "Gold")
by_NOC_Medal_Silver <- by_NOC_Medal %>% filter(Medal == "Silver")
by_NOC_Medal_Bronze <- by_NOC_Medal %>% filter(Medal == "Bronze")

boxplot(by_NOC_Medal_Gold$count)
boxplot.stats(by_NOC_Medal_Gold$count)$out

boxplot(by_NOC_Medal_Silver$count)
boxplot.stats(by_NOC_Medal_Silver$count)$out

boxplot(by_NOC_Medal_Bronze$count)
boxplot.stats(by_NOC_Medal_Bronze$count)$out


## ----------------------------------------------------------------------------------
##                           SELECCIÓN DE LOS DATOS
## ----------------------------------------------------------------------------------

## SELECCIÓN DE COLUMNAS O ATRIBUTOS

#Eliminamos las columnas no utilizadas en los análisis para simplificar los datasets
atletas <- atletas[,c("NOC","Year","Season","City","Sport","Event","Medal")]
paises <- paises[,c("country","code_3","continent","sub_region")]
sedes <- sedes[,c("City","Country","Continent","Year")]

## SELECCIÓN DE REGISTROS U OBSERVACIONES

# En el dataframe sedes hay que eliminar los 3 últimos registros ya que son basura
sedes <- sedes[1:(nrow(sedes)-3),]

# Además hemos descubierto que en el año 1956, por una cuarentena en el país, las pruebas 
# de equitación de Melbourne se realizaron en Estocolmo, por lo que hay que añadir dos 
# líneas, ya que en este dataframe no viene desagregadas estas dos ciudades.

sedes <- sedes[sedes$City!="Melbourne\n Stockholm",]
sedes <- add_row(sedes, City="Melbourne",Country="Australia",Continent="Oceania",Year=1956) %>%
  add_row(City="Stockholm",Country="Sweden",Continent="Europe",Year=1956)

# Las dos ediciones de 1940 que aparecen con doble sede se cancelaron por la guerra
# sino-japonesa y la segunda guerra mundial por lo que eliminamos esos registros
sedes <- sedes[sedes$Year != 1940,]

# Para dejar el dataframe de sedes limpio y evitar la generación de datos nulos, eliminamos
# los valores TBD que no contienen sedes reales

sedes <- sedes[sedes$City != "TBD",]

# Los Juegos Olímpicos de 1906 que se celebraron en Atenas no son reconocidos por el 
# Comité Olímpico Internacional (COI) en la actualidad por lo que no aparecen en el
# dataset sedes y los tenemos que eliminar del dataset atletas

atletas <- atletas[atletas$Year != 1906,]


## ----------------------------------------------------------------------------------
##                              INTEGRACIÓN DE LOS DATOS
## ----------------------------------------------------------------------------------

# Preparamos los datos para la unión homogeneizando los niveles de las variables
# categóricas que se van a utilizar posteriormente

# Pasamos el atributo a caracteres para poder hacer la imputación
sedes$Country <- as.character(sedes$Country)

sedes[sedes$Country == "United States", "Country"] = "USA"
sedes[sedes$Country == "United Kingdom", "Country"] = "UK"
sedes[sedes$Country == "Nazi Germany", "Country"] = "Germany"
sedes[sedes$Country == "West Germany", "Country"] = "Germany"
sedes[sedes$Country == "Soviet Union", "Country"] = "Russia"

# Volvemos a cambiar a factor
sedes$Country <- as.factor(sedes$Country)

# Pasamos el atributo a caracteres para poder hacer la imputación
sedes$Continent <- as.character(sedes$Continent)

sedes[sedes$Continent == "North America", "Continent"] = "Americas"
sedes[sedes$Continent == "South America", "Continent"] = "Americas"

# Volvemos a cambiar a factor
sedes$Continent <- as.factor(sedes$Continent)

# Hay ciertos nombres de ciudades que no coinciden con los de atletas, esto es debido 
# al idioma utilizado, ya que en sedes siempre están en inglés y en atletas utilizan
# para poner el nombre el propio del país.

# Pasamos el atributo a caracteres para poder hacer la imputación manual de las 11 ciudades
sedes$City <- as.character(sedes$City)

# Imputamos manualmente los nombres correctos de las ciudades
sedes[sedes$City == "Rome", "City"] = "Roma"
sedes[sedes$City == "Athens", "City"] = "Athina"
sedes[sedes$City == "Antwerp", "City"] = "Antwerpen"
sedes[sedes$City == "St. Moritz", "City"] = "Sankt Moritz"
sedes[sedes$City == "Moscow", "City"] = "Moskva"
sedes[sedes$City == "Turin", "City"] = "Torino"

# Volvemos a cambiar a factor
sedes$City <- as.factor(sedes$City)

# Hemos generado un diccionario que relaciona los códigos NOC con los códigos code_3
diccionario <- read.csv("https://www.dropbox.com/s/gibjji4okfhl0ru/diccionario.csv?dl=1", encoding="utf-8")

# Unificamos el nombre de los atributos para simplificar la unión de los dataframes 
names(paises) <- c("country","NOC","continent","sub_region")
diccionario <- diccionario[,c("NOC","code_3")]
names(diccionario) <- c("NOC","code_3")
names(sedes) <- c("City","Country_host","Continent_host","Year")

# Mantenemos todos los registros del primer dataframe con all.x=TRUE
df <- merge(atletas, diccionario, by = "NOC", all.x=TRUE) %>% 
merge(paises, by = "code_3", by.y = "NOC", all.x=TRUE) %>% 
merge(sedes, by = c("City","Year"), all.x=TRUE)

# Comprobamos la no existencia de NA

summary(df)


# Es necesario "dummificar" la variable cualitativa Medal que toma los valores
# Gold, Silver, Bronze, así la convertimos en 3 variables dicotómicas (0, 1)
# para lo que añadimos tres columnas nuevas en el dataframe

df <- df %>%
  mutate(Gold = ifelse(Medal == 'Gold', 1, 0)) %>%
  mutate(Silver = ifelse(Medal == 'Silver', 1, 0)) %>%
  mutate(Bronze = ifelse(Medal == 'Bronze', 1, 0)) 

# Creamos dos nuevos atributo dicotómicos, "sedePais" y "sedeContinente" donde
# el 1 significa que en esa edición de los juegos el país o el continente del 
# equipo fue sede de dichos juegos

df$country <- as.character(df$country)
df$Country_host <- as.character(df$Country_host)
df$continent <- as.character(df$continent)
df$Continent_host <- as.character(df$Continent_host)
df <- df %>%
  mutate(sedePais = ifelse(country == Country_host, 1, 0)) %>%
  mutate(sedeContinente = ifelse(continent == Continent_host, 1, 0))

# Creamos un dataframe con los datos agregados por país para nuestro análisis
df_pais <- df %>% group_by(country, City, Year, sedePais, sedeContinente) %>%
  summarise(T_Gold=sum(Gold), T_Silver=sum(Silver), T_Bronze=sum(Bronze))
  
df_pruebas <- df %>%
  select(City,Year,Country_host,Continent_host) %>%
  filter (is.na(Country_host)) %>%
  count(City,Year)

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


## ----------------------------------------------------------------------------------
##                         ANÁLISIS ESTADÍSTICO INFERENCIAL
## ----------------------------------------------------------------------------------

## ANÁLISIS DE CORRELACIÓN

# Comprobamos como se distribuyen las variables para ver que prueba es la más adecuada
shapiro.test(df_pais$sedePais)
shapiro.test(df_pais$T_Gold)
shapiro.test(df_pais$T_Silver)
shapiro.test(df_pais$T_Bronze)

# Lo podemos ver también gráficamente comparándolo con una distribución normal
plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

plotn(df_pais$sedePais, main="Distribución normal")
plotn(df_pais$T_Gold, main="Distribución normal")
plotn(df_pais$T_Silver, main="Distribución normal")
plotn(df_pais$T_Bronze, main="Distribución normal")

# Usamos el coeficiente de correlación de Spearman 
cor(x=df_pais$sedePais, y=df_pais$T_Gold, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$T_Silver, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$T_Bronze, method = "spearman")
# Significancia de la correlación de Spearman 
cor.test(x=df_pais$sedePais, y=df_pais$T_Gold, conf.level  = 0.95, method = "spearman")
cor.test(x=df_pais$sedePais, y=df_pais$T_Silver, conf.level  = 0.95, method = "spearman")
cor.test(x=df_pais$sedePais, y=df_pais$T_Bronze, conf.level  = 0.95, method = "spearman")

# Usamos el coeficiente de correlación de Kendall
cor(x=df_pais$sedePais, y=df_pais$T_Gold, method = "kendall")
cor(x=df_pais$sedePais, y=df_pais$T_Silver, method = "kendall")
cor(x=df_pais$sedePais, y=df_pais$T_Bronze, method = "kendall")


