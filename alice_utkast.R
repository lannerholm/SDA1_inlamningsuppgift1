
#Uppgift 1.1, 
#Vad kan man generellt säga om kriminaliteten i censusdistrikten? Använd lämpliga figurer samt fördelningsmått som underlag.

#Load Data
load(file = url("https://github.com/StatisticsSU/SDA1/blob/main/assignments/assignment1/Boston_census_data.RData?raw=true")) 
load(file = url("https://github.com/StatisticsSU/SDA1/blob/main/assignments/assignment1/Boston_districts_to_predict.RData?raw=true"))

#Overview
head(Boston_census_data)
str(Boston_census_data)


histogram(~ crime_rate, data = Boston_census_data, breaks = 40)
#Vi ser i histogrammet att det är unimodald, skevt åt höger och har ett flertal outliers/extremvärden


iqr(~ crime_rate, data = Boston_census_data)
sd(~ crime_rate, data = Boston_census_data)
#Vi ser att spridningsmåtten (IQR = 3,598 och sd = 8.74) är höga, vilket tyder på en stor spridning.
#Att standardavikelsen är mycket högre än IQR tyder på att de extremvärden som finns har mycket höga värden jämfört med övriga settet.
#Då iqr är mindre känsligt för outliers än sd.


median(~ crime_rate, data = Boston_census_data)
mean(~ crime_rate, data = Boston_census_data)
#Vi ser här att medianen är lägre än medelvärdet, vilket, igen, tyder på outliers.

#Vad vi sammanfattningsvis kan se är att kriminalitetet i en majoritet av distrikten är låg,
#men samtidigt finns det outliers med mycket högre kriminalitet. Medianen och medelvärdet här
#visar alltså två olika saker. Medianen och IQR är mer enlgit boken mer "korrekt" än de andra att redovisa då de inte blir lika påverkade av outliers, men det finns fortfarande ett
#sociologiskt värde i att visa medelvärdet imo. Det är trots allt mer intressant att veta varför just dessa distrikt har så mycket högre kriminalitet, och hur det påverkar distriktet i stort.

#*---------------------------------------------------------------------------------------------------------------------------------------#
#Uppgift 2.1
#Vad kan man generellt säga om fastighetsskatten i censusdistrikten? Använd lämpliga figurer samt fördelningsmått som underlag.
histogram(~ tax_rate, data = Boston_census_data, breaks = 40)
densityplot(~ tax_rate, data = Boston_census_data)
#täthetsdiagrammet visar på en bimodal assymetrisk updelning av datan, där det är ett mycket stort avstånd mellan de två typvärdena. 



iqr(~ tax_rate, data = Boston_census_data)
sd(~ tax_rate, data = Boston_census_data)
#Till skillnad från när vi analyserade brottsligheten ser vi här att IQR är högre än sd. 

median(~ tax_rate, data = Boston_census_data)
mean(~ tax_rate, data = Boston_census_data)
#Vi ser här att medianen är lägre än medelvärdet, vilket, igen, tyder på outlier.

#Det viktiga att notera här verkar vara att datasettet har så pass höga och många extremvärden. Det skulle kunna vara värt att splittra settet i två för att bättre analysera det?


#-------------------------------------------------------------------------------------------------------------------------------------------------

#Uppgift 3.1
#Gör ett histogram för variabeln dist_fenway_park. Vilket av censusdistrikten har längst respektive kortast avstånd till Fenway park? 
#Markera ut dessa distrikt i en interaktiv karta tillsammans med Fenway park.
histogram(~ dist_fenway_park, data = Boston_census_data, breaks = 50)

dist_fenway_park_min <- min(~ dist_fenway_park, data = Boston_census_data)
dist_fenway_park_max <- max(~ dist_fenway_park, data = Boston_census_data)
closest_district_to_fenway_park <- filter(Boston_census_data, dist_fenway_park == dist_fenway_park_min)
farthest_district_from_fenway_park <- filter(Boston_census_data, dist_fenway_park == dist_fenway_park_max)
#Distriktet med kortast avstånd = Wilmington med en distans på 887.
#Distriktet med längst avstånd = Marshfield med en distans på 33638.

#Draw interactive map
library(geosphere) 
lat_long <- cbind(Boston_census_data$latitude, Boston_census_data$longitude)
fenway_park_lat_long <- c(42.346462, -71.097250) # latitude and longitude for Fenway_park
Boston_census_data$dist_fenway_park <- distHaversine(lat_long, fenway_park_lat_long)

library(leaflet) 
Boston_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lat = fenway_park_lat_long[1], lng = fenway_park_lat_long[2], popup="Fenway park") %>%
  addMarkers(lat = closest_district_to_fenway_park$latitude[1], lng = closest_district_to_fenway_park$longitude[1], popup = closest_district_to_fenway_park$town) %>%
  addMarkers(lat = farthest_district_from_fenway_park$latitude[1], lng = farthest_district_from_fenway_park$longitude[1], popup = farthest_district_from_fenway_park$town)

Boston_map # Show interactive map


