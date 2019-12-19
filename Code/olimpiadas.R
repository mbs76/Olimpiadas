
# Importamos los diferentes dataset del proyecto
# Los ficheros csv a importar se han adquirido a través de los siguientes enlaces:
#  https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
#  https://www.kaggle.com/statchaitya/country-to-continent
#  https://www.downloadexcelfiles.com/wo_en/download-excel-file-list-olympic-host-cities
# Para la carga se va a utilizar enlaces suministrados por dropbox de los archivos

atletas <- read.csv("https://www.dropbox.com/s/8ogvaf0ipu4raby/athlete_events.csv?dl=1", encoding="utf-8")
noc <- read.csv("https://www.dropbox.com/s/hjngggjiwmsdyw8/noc_regions.csv?dl=1", encoding="utf-8")
paises <- read.csv("https://www.dropbox.com/s/qol9t3g4z359img/countryContinent.csv?dl=1", encoding="utf-8")
ciudades <- read.csv("https://www.dropbox.com/s/6fhwd3ydoeop30j/list-host-cities-olympic-943j.csv?dl=1", encoding="utf-8")

# Revisamos la estructura de los dataframes para comprobar el tipo de datos de cada atributo
str(atletas)
str(noc)
str(paises)
str(ciudades)

# Exploramos los dataframes creados visualizando las primeras filas
head(atletas)
head(noc)
head(paises)
head(ciudades)

# Unificamos el nombre de los campos para simplificar la unión de los dataframes con la función merge
names(noc) <- c("NOC", "region", "notes")
df <- merge(atletas, noc, by = "NOC", all.x=TRUE)

# Calculamos la media de edad de los medallistas por país
tapply(df$Age, df$region, mean, na.rm=TRUE)
