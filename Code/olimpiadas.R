# 

#Importamos los diferentes dataset del proyecto
#Es importante modificar la ruta de ubicación de cada uno de los archivos
#Los ficheros csv a importar se han adquirido a través de los siguientes enlaces:
#  https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results
#  https://www.kaggle.com/statchaitya/country-to-continent
#  https://www.downloadexcelfiles.com/wo_en/download-excel-file-list-olympic-host-cities


atletas <- read.csv("athlete_events.csv")
noc <- read.csv("noc_regions.csv")
paises <- read.csv("countryContinent.csv")
ciudades <- read.csv("list-host-cities-olympic-943j.csv")
