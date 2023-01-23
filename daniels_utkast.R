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


# histogrammet visar att distributionen av crime_rate är unimodal, icke-symmetrisk och skev åt höger.
# datan har flera extrema outliers, och därför  beskriver medianen centralpunkten bättre än medelvärde.
histogram(~ crime_rate, data = Boston_census_data, breaks = 50)
densityplot(~ crime_rate, data = Boston_census_data)

n <- length(Boston_census_data$crime_rate)
min <- min(Boston_census_data$crime_rate)
max <- max(Boston_census_data$crime_rate)
range <- min - max
median <- median(Boston_census_data$crime_rate)
mean <- mean(Boston_census_data$crime_rate)
quartiles <- quantile(Boston_census_data$crime_rate)
q1 <- quartiles[2]
q3 <- quartiles[4]
iqr <- q3 - q1
variance <- var(Boston_census_data$crime_rate)
std_dev <- sd(Boston_census_data$crime_rate)

boxplot(crime_rate, data = Boston_census_data)


selected_towns <- c("Boston East Boston", "Boston Downtown", "Cambridge", "Boston Roxbury", "Boston Savin Hill")
town_subset <- Boston_census_data %>%
  filter(town %in% selected_towns)

fav_stats(town_subset$crime_rate)
boxplot(crime_rate ~ town, data = town_subset)

####################
#find the top 3 correlations between crime_rate and the other numerical variables
strictly_numerical <- Boston_census_data %>%
  select_if(is.numeric) %>%
  select(-any_of(c("crime_rate")))
correlations <- cor(Boston_census_data$crime_rate, strictly_numerical)
correlations
# now test for significance