rm(list = ls())
# Load Data
hotel <- read.csv("D:/Case Competition/RIT Hotel&Resort Data Analytics/hotel.csv")
resort <- read.csv("D:/Case Competition/RIT Hotel&Resort Data Analytics/resort.csv")

# Data Cleaning + Revenue
hotel$ArrivalDate <- as.Date(hotel$ArrivalDate, format='%Y-%B-%d')
hotel$CheckoutDate <- as.Date(hotel$CheckoutDate)
hotel$StaysDate <- hotel$CheckoutDate - hotel$ArrivalDate
hotel$StaysDate <- as.numeric(hotel$StaysDate)
hotel$Revenue <- hotel$AverageDailyRate * hotel$StaysDate

resort$ArrivalDate <- as.Date(resort$ArrivalDate, format ='%Y-%B-%d')
resort$CheckoutDate <- as.Date(resort$CheckoutDate, format='%m/%d/%Y')
resort$StaysDate <- resort$CheckoutDate - resort$ArrivalDate
resort$StaysDate <- as.numeric(resort$StaysDate)
resort$Revenue <- resort$AverageDailyRate * resort$StaysDate

# Country, Type, number of adult, baby, children
str(hotel)
summary(hotel)
summary(resort)
unique(hotel$Country)
unique(hotel$CustomerType)
unique(resort$Country)

summary(lm(Revenue ~ factor(Country)+factor(CustomerType)+NumberOfAdult+NumberOfBabies+NumberOfChildren, data=hotel)) #baseline AGO Contract
# hotel
# NumberOfAdult                         89.8850     2.0939  42.927  < 2e-16 ***
# NumberOfBabies                        -0.2665     9.4224  -0.028   0.9774    
# NumberOfChildren                      88.3254     2.9281  30.164  < 2e-16 ***

summary(lm(Revenue ~ factor(Country)+factor(CustomerType)+NumberOfAdult+NumberOfBabies+NumberOfChildren, data=resort)) #baseline AGO Contract
# resort
# NumberOfAdult                        220.785      5.845  37.771  < 2e-16 ***
# NumberOfBabies                       131.339     20.536   6.395 1.64e-10 ***
# NumberOfChildren                     172.287      6.833  25.214  < 2e-16 ***



