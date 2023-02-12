#set working directory to path of this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#install and require packages
required_pkgs <- c("mosaic", "dplyr", "geosphere", "leaflet")
pkg_is_installed <- required_pkgs %in% rownames(installed.packages())
if(any(pkg_is_installed = FALSE)) {
  install.packages(required_pkgs[!pkg_is_installed])
}
invisible(lapply(required_pkgs, FUN = function(pkgs) {
  do.call("require", list(pkgs)) 
}))

#load boston census data
load(file = url("https://github.com/StatisticsSU/SDA1/blob/main/assignments/assignment1/Boston_census_data.RData?raw=true")) 
load(file = url("https://github.com/StatisticsSU/SDA1/blob/main/assignments/assignment1/Boston_districts_to_predict.RData?raw=true"))

head(Boston_census_data)
str(Boston_census_data)

1.1
# histogrammet visar att distributionen av crime_rate är unimodal, icke-symmetrisk och mycket skev åt höger.
# datan ser ut att ha flera outliers.
histogram(~ crime_rate, data = Boston_census_data, breaks = 50)
favstats(Boston_census_data$crime_rate)

#re-express med square root av crime_rate. Fördelningen är förtfarande skev.
histogram(~ sqrt(crime_rate), data = Boston_census_data, breaks = 50)

#re-expression with log visar en bimodal fördelning, något mer symmetrisk. denna är nog bäst.
#bimodaliteten är intressant. vilken tredje variabel kan förklara?
histogram(~ log(crime_rate), data = Boston_census_data, breaks = 50)
densityplot(~ log(crime_rate), data = Boston_census_data)
fav_stats(log(Boston_census_data$crime_rate))
boxplot(log(crime_rate), data = Boston_census_data)

1.2

selected_towns <- c("Boston East Boston", "Boston Downtown", "Cambridge", "Boston Roxbury", "Boston Savin Hill")
town_subset <- Boston_census_data %>%
  filter(town %in% selected_towns)

fav_stats(town_subset$crime_rate)
boxplot(crime_rate ~ town, data = town_subset)
boxplot(log(crime_rate) ~ town, data = town_subset)
# boxplot diagrammet visar att crime_rate . Cambridge har lägre brottsligehet.

### 1.3

strictly_numerical <- Boston_census_data %>%
  select_if(is.numeric) %>%
  select(-any_of(c("longitude", "latitude"))) #finns här kategoriska variabler kvar?
correlations <- cor(strictly_numerical)
library(corrplot)
corrplot(correlations, method = "number")
# vi kan avläsa att de tre varablerna med högst korrelation är radial_access, median_home_value och tax_rate.
# negativt samband
plot(Boston_census_data$crime_rate ~ Boston_census_data$median_home_value)
plot(log(Boston_census_data$crime_rate) ~ Boston_census_data$median_home_value)

plot(Boston_census_data$crime_rate ~ Boston_census_data$tax_rate)
plot(log(Boston_census_data$crime_rate) ~ Boston_census_data$tax_rate)

plot(Boston_census_data$crime_rate ~ Boston_census_data$radial_access) # not numerical? categorical?
plot(log(Boston_census_data$crime_rate) ~ Boston_census_data$radial_access)

### 2.1
histogram(Boston_census_data$tax_rate, breaks = 100)
favstats(Boston_census_data$tax_rate)
Boston_census_data$cat_tax <- cut(Boston_census_data$tax_rate, 
                                  breaks=c(0, 250, 400, 800),
                                  labels=c('Low', 'Medium', 'High'))
head(Boston_census_data)
histogram(~ tax_rate | cat_tax, data = Boston_census_data)
favstats(~ tax_rate | cat_tax, data = Boston_census_data)

histogram(~ tax_rate | cat_tax + borders_charles, data = Boston_census_data)
favstats(~ tax_rate | cat_tax + borders_charles, data = Boston_census_data)

boxplot(tax_rate ~ cat_tax + borders_charles, data = Boston_census_data)

### 2.3
#Hur många procent av alla censusdistrikt ligger i angränsning till Charles 
# River och tillhör en hög skattekategori?
tally(~ cat_tax & borders_charles, data = Boston_census_data, margins = TRUE, format = "percent")
# vi avläser 2.1%

# Hur stor andel av censusdistrikten med hög skatt ligger inte i angränsning till Charles River?
tally(~ borders_charles | cat_tax, data = Boston_census_data, margins = TRUE, format = "percent")
# vi avläser 94.7%

### 2.4
#se corrplot ovan
# vi avläser indust_p 0.72, NOx 0.67, radial_access 0.91, crime rate 0.58, median_home_value -0.58
plot(indust_p ~ tax_rate, data = Boston_census_data)
plot(NOx ~ tax_rate, data = Boston_census_data)
plot(crime_rate ~ tax_rate, data = Boston_census_data)
plot(median_home_value ~ tax_rate, data = Boston_census_data)

#radial access korrelationen påverkas av en extrem outlier. är denna kategorisk?
# kanske ska ta bort outliern och räkna igen.
plot(radial_access ~ tax_rate, data = Boston_census_data)

### 3

library(geosphere) # Install if not available
lat_long <- cbind(Boston_census_data$latitude, Boston_census_data$longitude)
fenway_park_lat_long <- c(42.346462, -71.097250) # latitude and longitude for Fenway_park
Boston_census_data$dist_fenway_park <- distHaversine(lat_long, fenway_park_lat_long)

library(leaflet) # Install if not available
Boston_map <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lat = fenway_park_lat_long[1], lng = fenway_park_lat_long[2], popup="Fenway park") %>%
  addMarkers(lat = Boston_census_data$latitude[30], lng = Boston_census_data$longitude[30], popup="Observation 30") %>%
  addMarkers(lat = Boston_census_data$latitude[45], lng = Boston_census_data$longitude[45], popup="Observation 45") 

Boston_map # Show interactive map
head(Boston_census_data)
histogram(~dist_fenway_park, data =Boston_census_data)

### 3.1
favstats(Boston_census_data$dist_fenway_park)
#avläser att längsta dist_fenway_park är 33638.4, och kortaste är 887.9
furthest<- Boston_census_data %>%
  filter(Boston_census_data$dist_fenway_park > 33638) # Marshfield

closest <- Boston_census_data %>%
  filter(Boston_census_data$dist_fenway_park < 888) # Wilmington

Boston_map %>%
  addMarkers(lat = furthest$latitude, lng = furthest$longitude, popup = "Marshfield") %>%
  addMarkers(lat = closest$latitude, lng = closest$longitude, popup = "Wilmington")
### 3.2
cor(Boston_census_data$median_home_value, Boston_census_data$dist_fenway_park)
#mycke låg korrelation. ej signifikan?
plot(Boston_census_data$dist_fenway_park, Boston_census_data$median_home_value)
#plotten visar inget samband
### 3.3

cor(Boston_census_data$crime_rate, Boston_census_data$dist_fenway_park)
#svagt negativt korrelation
plot(Boston_census_data$dist_fenway_park, Boston_census_data$crime_rate)
# men plot visar e spike i crime rate ungefärmellan 5000-10000(m?) avstånd.
# korrelation är förmodligen ej ett bra mått här

### 4.1

#fit the model
fit <- lm(NOx ~ employ_dist, data = Boston_census_data)

#print summary statistics
summary(fit)

#plot regression line over scatterplot
plot(NOx ~ employ_dist, data = Boston_census_data)
abline(fit)

# första intrycket är att modellen inte är bra, eftersom datan inte är linjärt.
#Dessutom kanske det finns några outliers där empl_dist är större än 10.
# Vi kontrollerar med residualanalys.

# show all diagnostic charts 
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

# Normal Q-Q plotten visar att de standardiserade residualern inte följer en 
# rak linje med 45 graders vinkel. Detta är ett tecken på att modellen inte är 
# väl anpassad, eftersom residualerna inte är normalfördelade.

# I residuals vs fitted syns ett (icke linjärt) samband mellan residualerna och fitted values. 
# Detta är ett tecken på att sambandet mellan NOx och employ_dist inte är linjärt.
# Dessutom uppvisar den heteroscedasticitet, dvs att residualerna blir större 
# längre åt höger.

# sist tittar vi på residuals vs. leverage. Vi letar efter outliers med hög
# residual och hög leverage, eftersom de har stor effekt på modellen. Särskilt
# observationerna 125 och 130 verkar vara särskilt inflytelserika.
# Kanske skullle modellen bli bättre om de togs bort.

# överlag är det en dålig modell, för ingen av antagandena verkar vara uppfyllda.

### 4.2

new_x <- data.frame(employ_dist = c(Boston_census_data[10,12]))
predicted <- predict(fit, newdata = new_x)
predicted
resid <- residuals(fit)[10]

### 4.3

# vi är i nedre vänstra hörnet på tukeys cirkel och kan därför gå neråt i 
#trappan med x eler y eller både x och y.

par(mfrow = c(4, 4))
#plot(NOx ~ employ_dist, data = Boston_census_data)
plot(NOx ~ I(sqrt(employ_dist)), data = Boston_census_data) 
plot(NOx ~ I(log(employ_dist)), data = Boston_census_data) 
plot(NOx ~ I(-(employ_dist^(-1/2))), data = Boston_census_data) 
plot(NOx ~ I(-(employ_dist^(-1))), data = Boston_census_data) 
# I(-(employ_dist^(-1/2)))
plot(sqrt(NOx) ~ employ_dist, data = Boston_census_data) 
plot(log(NOx) ~ employ_dist, data = Boston_census_data) 
plot(I(-NOx^(-1/2)) ~ employ_dist, data = Boston_census_data) 
plot(I(-NOx^(-1)) ~ employ_dist, data = Boston_census_data) 

plot(sqrt(NOx) ~ I(sqrt(employ_dist)), data = Boston_census_data) 
plot(log(NOx) ~ I(log(employ_dist)), data = Boston_census_data) 
plot(log(NOx) ~ I(-(employ_dist^-(1/2))), data = Boston_census_data) 
plot( ~ I(-(employ_dist^-(1/2))), data = Boston_census_data) 
plot(I(-NOx^(-1)) ~ I(-(employ_dist^-(1/2))), data = Boston_census_data) 
plot(I(-NOx^(-1)) ~ I(log(employ_dist)), data = Boston_census_data) # välj denna
plot(I(-NOx^(-1)) ~ I(sqrt(employ_dist)), data = Boston_census_data) 
par(mfrow = c(1, 1))


# anpassar en ny modell

#fit2 <- lm(NOx ~ log(employ_dist), data = Boston_census_data)
#plot(NOx ~ log(employ_dist), data = Boston_census_data)



#fit2 <- lm(I(-NOx^(-1/2)) ~ I(log(employ_dist)), data = Boston_census_data)
#plot(I(-NOx^(-1/2)) ~ log(employ_dist), data = Boston_census_data)

fit2 <- lm(log(NOx) ~ log(employ_dist), data = Boston_census_data)
plot(log(NOx) ~ log(employ_dist), data = Boston_census_data)

abline(fit2) #fel sorts linje

summary(fit2)
par(mfrow = c(2, 4))
plot(fit2)
plot(fit)
par(mfrow = c(1, 1))
# den nya modellen är något bättre än den gamla, men inte perfekt.
# Normal Q-Q plotten följer den diagonala raka linjen något bättre, 
# men de standardiserade residualerna är fortfarande för höga för höga och låga värden.
# residual vs. fitted visar inget samband, men är fortfarande heteroscedastisk. 
