install.packages("modeest") 
library(modeest)

# Importamos los diferentes dataset del proyecto
# Los ficheros csv a importar se han adquirido a través de los siguientes enlaces:
#  https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
#  https://www.kaggle.com/statchaitya/country-to-continent
#  https://www.downloadexcelfiles.com/wo_en/download-excel-file-list-olympic-host-cities
# Para la carga se va a utilizar enlaces suministrados por dropbox de los archivos

atletas <- read.csv("https://www.dropbox.com/s/8ogvaf0ipu4raby/athlete_events.csv?dl=1", encoding="utf-8")
paises <- read.csv("https://www.dropbox.com/s/qol9t3g4z359img/countryContinent.csv?dl=1", encoding="utf-8")
ciudades <- read.csv("https://www.dropbox.com/s/6fhwd3ydoeop30j/list-host-cities-olympic-943j.csv?dl=1", encoding="utf-8")

# Revisamos la estructura de los dataframes para comprobar el tipo de datos de cada atributo
str(atletas)
str(paises)
str(ciudades)

# Exploramos los dataframes creados visualizando las primeras filas
head(atletas)
head(paises)
head(ciudades)

#Eliminamos las columnas no utilizadas en los análisis para simplificar los datasets
atletas <- atletas[,c("NOC","Year","Season","City","Sport","Event","Medal")]
paises <- paises[,c("country","code_3","continent","sub_region")]
ciudades <- ciudades[,c("City","Country","Continent","Year")]

# En el dataframe ciudades hay que eliminar los 3 últimos registros ya que son basura
ciudades <- ciudades[1:(nrow(ciudades)-3),]


#####################################################################################
#                         TRATAMIENTO DE DATOS PERDIDOS
#####################################################################################

# En el dataframe de ciudades encontramos vlaores no definidos NA en el atributo Year.
# Esto ocurre cuando en un mismo año coindicen las ediciones de invierno y verano, por
# lo que imputamos el año del registro inmediamente anterior que contiene el dato
# exacto que corresponde.

for(i in 3:nrow(ciudades)){
  if (is.na(ciudades[i,"Year"])) {
    ciudades[i,"Year"] <- ciudades[i-1,"Year"]
  }
}


#####################################################################################
#                         INTEGRACIÓN DE DATOS
#####################################################################################

# Unificamos el nombre de los atributos para simplificar la unión de los dataframes 
# con la función merge. Mantenemos todos los registros del primer dataframe.

names(paises) <- c("country","NOC","continent","sub_region")
df <- merge(atletas, paises, by = "NOC", all.x=TRUE)
names(ciudades) <- c("City","Country_host","Continent_host","Year")
df <- merge(df, ciudades, by = c("City","Year"), all.x=TRUE)

# Calculamos la moda del atriburo tipo de medallas
mlv(df$Medal, method = "mfv", na.rm=TRUE)

