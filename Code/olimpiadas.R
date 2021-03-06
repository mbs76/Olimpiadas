
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

if(!require("gmodels")){
  install.packages("gmodels")
  library("gmodels")
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
# por lo que cremamos una nueva categoría para imputarla a todos los NA's.

atletas$Medal <- as.character(atletas$Medal)
atletas[is.na(atletas$Medal),"Medal"] = "Sin medalla"
atletas$Medal <- as.character(atletas$Medal)

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
boxplot(by_NOC$count, main="Outliers de medallas por países")
boxplot.stats(by_NOC$count)$out

# Realizamos la misma representación diferenciando por tipo de medalla
# de oro, plata y bronce

by_NOC_Medal <- atletas %>% group_by(NOC, Medal) %>% summarise(count=n())
by_NOC_Medal_Gold <- by_NOC_Medal %>% filter(Medal == "Gold")
by_NOC_Medal_Silver <- by_NOC_Medal %>% filter(Medal == "Silver")
by_NOC_Medal_Bronze <- by_NOC_Medal %>% filter(Medal == "Bronze")

boxplot(by_NOC_Medal_Gold$count, main="Outliers - oro por países")
boxplot.stats(by_NOC_Medal_Gold$count)$out

boxplot(by_NOC_Medal_Silver$count, main="Outliers - plata por países")
boxplot.stats(by_NOC_Medal_Silver$count)$out

boxplot(by_NOC_Medal_Bronze$count, main="Outliers - bronce por países")
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

# En la tabla paises hay que asignar de continente a la Antartida y la Isla Bouvet
paises$continent <- as.character(paises$continent)
paises$sub_region <- as.character(paises$sub_region)
paises[paises$NOC=="ATA","continent"] = "Antarctica"
paises[paises$NOC=="ATA","sub_region"] = "Antarctica"
paises[paises$NOC=="BVT","continent"] = "Antarctica"
paises[paises$NOC=="BVT","sub_region"] = "Antarctica"

# Añadimos contienente a islas oceánicas sin clasificar
paises[paises$continent=="","continent"] = "Oceania"
paises[paises$sub_region=="","sub_region"] = "Others"
paises$continent <- as.factor(paises$continent)
paises$sub_region <- as.factor(paises$sub_region)

# En el dataframe sedes hay que eliminar los 3 últimos registros ya que son basura
sedes <- sedes[1:(nrow(sedes)-3),]

# Además hemos descubierto que en el año 1956, por una cuarentena en el país, las pruebas 
# de equitación de Melbourne se realizaron en Estocolmo, por lo que hay que añadir dos 
# líneas, ya que en este dataframe no viene desagregadas estas dos ciudades.

sedes <- sedes[sedes$City!="Melbourne\n Stockholm",]
sedes <- add_row(sedes, City="Melbourne",Country="Australia",Continent="Oceania",Year=1956) %>%
  add_row(City="Stockholm",Country="Sweden",Continent="Europe",Year=1956)

# Los Juegos Olímpicos de 1906 que se celebraron en Atenas no son reconocidos por el 
# Comité Olímpico Internacional (COI) en la actualidad por lo que no aparecen en el
# dataset sedes y los tenemos que eliminar del dataset atletas
atletas <- atletas[atletas$Year != 1906,]

# Por motivo de la Primera Guerra Mundial, se anuló la celebración de los juegos en Berlin en 1916
sedes <- sedes[sedes$Year != 1916,]

# Las dos ediciones de 1940 que aparecen con doble sede se cancelaron por la guerra
# sino-japonesa y la segunda guerra mundial por lo que eliminamos esos registros
sedes <- sedes[sedes$Year != 1940,]

# Las ediciones de 1944 también se cancelaron por la segunda guerra mundial
sedes <- sedes[sedes$Year != 1944,]

# Para dejar el dataframe de sedes limpio y evitar la generación de datos nulos, eliminamos
# los valores TBD que no contienen sedes reales

sedes <- sedes[sedes$City != "TBD",]


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

# En la tabla países falta por añadir Kosovo para evitar datos nulos en la unión
paises <- paises %>%
  add_row(country="Kosovo",NOC="XKX",continent="Europe",sub_region="Southern Europe")
  
# Mantenemos todos los registros del primer dataframe con all.x=TRUE
df <- merge(atletas, diccionario, by = "NOC", all.x=TRUE) %>% 
merge(paises, by = "code_3", by.y = "NOC", all.x=TRUE) %>% 
merge(sedes, by = c("City","Year"), all.x=TRUE)

# En la unión vemos que hay registros NA generados, analízándolos vemos que coresponden a los siguientes códigos NOC/code_3
# EUN EUN Unified Team
# IOA IOA Individual Olympic Athletes
# Estos registros hay que eliminarlos, ya que no nos va a servir para el análisis por países o continentes

df <- df[df$NOC!="EUN",]
df <- df[df$NOC!="IOA",]

# Comprobamos la no existencia de NA tras la unión
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

df$country <- as.factor(df$country)
df$Country_host <- as.factor(df$Country_host)
df$continent <- as.factor(df$continent)
df$Continent_host <- as.factor(df$Continent_host)

# Creamos un dataframe con los datos agregados por país para nuestro análisis
df_pais <- df %>% group_by(country, City, Year, sedePais) %>%
  summarise(Participacion=n(), T_Gold=sum(Gold), T_Silver=sum(Silver), T_Bronze=sum(Bronze), T_Medal=(sum(Gold)+sum(Silver)+sum(Bronze)), P_Medal=(sum(Gold)+sum(Silver)+sum(Bronze))/n())

# Creamos un dataframe con los datos agregados por continente para nuestro análisis
df_continente <- df %>% group_by(continent, City, Year, sedeContinente) %>%
  summarise(Participacion=n(), T_Gold=sum(Gold), T_Silver=sum(Silver), T_Bronze=sum(Bronze), T_Medal=(sum(Gold)+sum(Silver)+sum(Bronze)), P_Medal=(sum(Gold)+sum(Silver)+sum(Bronze))/n())


## ----------------------------------------------------------------------------------
##                         ANÁLISIS ESTADÍSTICO DESCRIPTIVO
## ----------------------------------------------------------------------------------

# Ranking de países por participación de atletas
summary(df$country)

# Total de medallas de cada tipo y sus porcentajes
CrossTable(df$Medal)

# Resultados acumulados absolutos de todos los juegos por país
table(df$Medal, df$country)
# Resultados acumulados en porcentaje de todos los juegos por país
CrossTable(df$Medal, df$country)

# Medallas acumuladas de oro en todos los juegos por país
tapply(df$Gold, df$country, sum)
# Medallas acumuladas de plata en todos los juegos por país
tapply(df$Silver, df$country, sum)
# Medallas acumuladas de bronce en todos los juegos por país
tapply(df$Bronze, df$country, sum)

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
shapiro.test(df_pais$T_Medal)
shapiro.test(df_pais$P_Medal)
shapiro.test(df_continente$sedeContinente)
shapiro.test(df_continente$T_Gold)
shapiro.test(df_continente$T_Silver)
shapiro.test(df_continente$T_Bronze)
shapiro.test(df_continente$T_Medal)
shapiro.test(df_continente$P_Medal)

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

plotn(df_pais$sedePais, main="Distribución normal - Paises - Sedes")
plotn(df_pais$T_Gold, main="Distribución normal -Paises - Oro")
plotn(df_pais$T_Silver, main="Distribución normal - Paises - Plata")
plotn(df_pais$T_Bronze, main="Distribución normal - Paises - Bronce")
plotn(df_pais$T_Medal, main="Distribución normal - Paises - Medallas")
plotn(df_continente$sedeContinente, main="Distribución normal - Continentes - Sedes")
plotn(df_continente$T_Gold, main="Distribución normal - Continentes - Oro")
plotn(df_continente$T_Silver, main="Distribución normal - Continentes - Plata")
plotn(df_continente$T_Bronze, main="Distribución normal - Continentes - Bronce")
plotn(df_continente$T_Medal, main="Distribución normal - Continentes - Medallas")

# Usamos el coeficiente de correlación de Spearman 
cor(x=df_pais$sedePais, y=df_pais$T_Gold, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$T_Silver, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$T_Bronze, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$T_Medal, method = "spearman")
cor(x=df_pais$sedePais, y=df_pais$P_Medal, method = "spearman")
cor(x=df_continente$sedeContinente, y=df_continente$T_Gold, method = "spearman")
cor(x=df_continente$sedeContinente, y=df_continente$T_Silver, method = "spearman")
cor(x=df_continente$sedeContinente, y=df_continente$T_Bronze, method = "spearman")
cor(x=df_continente$sedeContinente, y=df_continente$T_Medal, method = "spearman")
cor(x=df_continente$sedeContinente, y=df_continente$P_Medal, method = "spearman")

# Significancia de la correlación de Spearman 
cor.test(x=df_pais$sedePais, y=df_pais$T_Gold, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_pais$sedePais, y=df_pais$T_Silver, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_pais$sedePais, y=df_pais$T_Bronze, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_pais$sedePais, y=df_pais$T_Medal, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Gold, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Silver, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Bronze, conf.level  = 0.95, method = "spearman", exact=FALSE)
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Medal, conf.level  = 0.95, method = "spearman", exact=FALSE)

# Usamos el coeficiente de correlación de Kendall
cor(x=df_pais$sedePais, y=df_pais$T_Gold, method = "kendall")
cor(x=df_pais$sedePais, y=df_pais$T_Silver, method = "kendall")
cor(x=df_pais$sedePais, y=df_pais$T_Bronze, method = "kendall")
cor(x=df_pais$sedePais, y=df_pais$T_Medal, method = "kendall")
cor(x=df_continente$sedeContinente, y=df_continente$T_Gold, method = "kendall")
cor(x=df_continente$sedeContinente, y=df_continente$T_Silver, method = "kendall")
cor(x=df_continente$sedeContinente, y=df_continente$T_Bronze, method = "kendall")
cor(x=df_continente$sedeContinente, y=df_continente$T_Medal, method = "kendall")

# Significancia de la correlación de Kendall
cor.test(x=df_pais$sedePais, y=df_pais$T_Gold, conf.level  = 0.95, method = "kendall")
cor.test(x=df_pais$sedePais, y=df_pais$T_Silver, conf.level  = 0.95, method = "kendall")
cor.test(x=df_pais$sedePais, y=df_pais$T_Bronze, conf.level  = 0.95, method = "kendall")
cor.test(x=df_pais$sedePais, y=df_pais$T_Medal, conf.level  = 0.95, method = "kendall")
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Gold, conf.level  = 0.95, method = "kendall")
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Silver, conf.level  = 0.95, method = "kendall")
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Bronze, conf.level  = 0.95, method = "kendall")
cor.test(x=df_continente$sedeContinente, y=df_continente$T_Medal, conf.level  = 0.95, method = "kendall")

## ----------------------------------------------------------------------------------
##                                VISUALIZACION
## ----------------------------------------------------------------------------------

# Comparamos gráficamente los resultados de EEUU y Rusia con otro país seleccionado
seleccion <- "Spain" # cambiar el país para comparar
df_seleccion <- df_pais[df_pais$country %in% list(seleccion, "United States of America", "Russian Federation"),] 
ggplot(df_seleccion, aes(Year, T_Medal, colour = country)) + 
geom_point() +
  xlab("Juegos Olimpicos") + 
  ylab("Medallas obtenidas") +
  labs(colour="Paises") +
  theme_light()

# Visualizamos los mismos resultados pero con ejes ortogonales para verlo de forma más intuitiva
# e incluyendo el estadístico densidad
seleccion <- list("Spain", "France", "Germany", "United States of America", "Russian Federation") 
df_seleccion <- df_pais[df_pais$country %in% seleccion,] 
ggplot(df_seleccion, aes(Year, T_Medal, colour = country)) + 
  geom_point()+
  geom_density2d() +
  facet_grid(df_seleccion$country~.) +
  xlab("Juegos Olimpicos") + 
  ylab("Medallas obtenidas") +
  labs(colour="Paises") +
  theme_light() + 
  theme(legend.position="bottom")

# Comparamos resultados individuales por paises es diferentes ediciones
seleccion <- list("Australia") 
df_seleccion <- df[df$country %in% seleccion & (df$Year==1952 | df$Year==1956) & df$Medal!="Sin medalla",] 
ggplot(df_seleccion, aes(Medal)) + 
  geom_bar() +
  facet_grid(.~df_seleccion$Year) +
  xlab("Tipo de medalla") + 
  ylab("Numero de medallas") +
  theme_light()

seleccion <- list("Spain") 
df_seleccion <- df[df$country %in% seleccion & (df$Year==1992 | df$Year==1996) & df$Medal!="Sin medalla",] 
ggplot(df_seleccion, aes(Medal)) + 
  geom_bar() +
  facet_grid(.~df_seleccion$Year) +
  xlab("Tipo de medalla") + 
  ylab("Numero de medallas") +
  theme_light()

## ----------------------------------------------------------------------------------
##                       EXPORTACIÓN DE LOS DATOS A CSV
## ----------------------------------------------------------------------------------

write.csv(df,file="df.csv", row.names = F)
write.csv(df_pais,file="df_pais.csv", row.names = F)
write.csv(df_continente,file="df_continente.csv", row.names = F)
write.csv(df_seleccion,file="df_seleccion.csv", row.names = F)

