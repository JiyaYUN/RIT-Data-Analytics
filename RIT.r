library(earth)

hotel <- read.csv("C:/Users/youyou/Desktop/UR/case/RIT/hotel.csv")
resort <- read.csv("C:/Users/youyou/Desktop/UR/case/RIT/resort.csv")

hotel$rev <- hotel$AverageDailyRate * (hotel$StaysInWeekendNights + hotel$StaysInWeekNights)
hotel <- na.omit(hotel)

reg1 <- lm(rev ~ ArrivalWeekNumber + AssignedRoom + BookingMeal + BookingDistributionChannel + BookingChanges + BookingCompany + BookingParking + Country + CustomerType + DaysInWaitingList + DepositType + LeadTime + MarketSegment + NumberOfAdult + NumberOfBabies + NumberOfChildren + RepeatedGuest + ReservedRoom + StaysInWeekendNights + StaysInWeekNights + TotalOfSpecialRequests + TravelAgent, data = hotel)

summary(reg1)

set.seed(1)

isTraining <- runif(nrow(hotel)) < .8
trainingData <- subset(hotel, isTraining)
validationData <- subset(hotel, !isTraining)


earth1 <- earth(rev ~ ., data = hotel)
plotmo(earth1)


#TotalOfSpecialRequests            5.030e+00  8.809e-01   5.709 1.15e-08 ***
#ReservedRoomB                    -4.694e+01  5.224e+00  -8.986  < 2e-16 ***
#ReservedRoomC                     3.673e+01  4.151e+01   0.885 0.376273
#ReservedRoomD                     4.166e+01  2.321e+00  17.950  < 2e-16 ***
#ReservedRoomE                     1.045e+02  6.160e+00  16.967  < 2e-16 ***
#ReservedRoomF                     1.159e+02  7.355e+00  15.759  < 2e-16 ***
#ReservedRoomG                     2.464e+02  1.047e+01  23.530  < 2e-16 ***
#CustomerTypeGroup                -2.287e+01  8.210e+00  -2.785 0.005355 **
#CustomerTypeTransient             2.159e+01  3.178e+00   6.794 1.11e-11 ***
#CustomerTypeTransient-Party       1.650e+01  3.757e+00   4.392 1.13e-05 ***
#BookingParking                    1.495e+01  2.842e+00   5.261 1.45e-07 ***
#BookingMealFB                    -1.580e+02  4.288e+01  -3.685 0.000229 ***
#BookingMealHB                     9.688e+01  2.736e+00  35.404  < 2e-16 ***
#BookingMealSC                    -3.749e+01  2.013e+00 -18.628  < 2e-16 ***
#BookingDistributionChannelDirect -4.866e+01  1.033e+01  -4.710 2.48e-06 ***
#BookingDistributionChannelGDS    -2.713e+01  1.000e+02  -0.271 0.786268
#BookingDistributionChannelTA/TO  -1.313e+01  1.169e+01  -1.123 0.261345
#BookingChanges                   -1.396e+01  8.829e-01 -15.816  < 2e-16 ***