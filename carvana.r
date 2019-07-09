require(tidyverse)
require(caret)
require(lubridate)
require(mice)
require(naniar)
require(DMwR)
require(pROC)
require(xgboost)
require(randomForest)
require(e1071)
require(corrplot)
require(JOUSBoost)

# Reading in the data and temporarily combining the datasets to facilitate data cleaning

wd <- "C:/Users/Bryan/Google Drive/Kaggle/Carvana/"
train.set <- read.csv(paste(wd, "training.csv", sep = ""), stringsAsFactors = FALSE)
test.set <- read.csv(paste(wd, "test.csv", sep = ""), stringsAsFactors = FALSE)
test.set <- test.set %>% mutate(set = "test",
                                IsBadBuy = NA)
train.set <- train.set %>% mutate(set = "train")
data.set <- rbind(train.set, test.set)
rm(train.set); rm(test.set)

# Renaming a misspelled predictor name

names(data.set)[which(names(data.set) == "MMRAcquisitonRetailCleanPrice")] <- "MMRAcquisitionRetailCleanPrice"

# Creating new predictors based on date of vehicle purchase

data.set <- data.set %>% mutate(PurchDate = mdy(PurchDate)) %>%
                         mutate(YearPurchased = year(PurchDate),
                                MonthPurchased = month(PurchDate),
                                DayPurchased = day(PurchDate)) %>%
                         mutate(DayofWeekPurchased = as.factor(weekdays(PurchDate)),
                                AdjDaysSincePurchase = as.numeric(max(PurchDate) - PurchDate)) %>%
                         mutate(YearPurchased = as.factor(YearPurchased),
                                MonthPurchased = as.factor(MonthPurchased),
                                DayPurchased = as.factor(DayPurchased))

#

table(data.set$VehicleAge, data.set$VehYear)

# Examining categorical predictor related to which auction the vehicle was purchased from

table(data.set$Auction)
data.set$Auction <- as.factor(data.set$Auction)

# Examining and correcting vehicle make information as needed

data.set$Make <- as.factor(data.set$Make)
table(data.set$Make)
data.set$Make[data.set$Make == "PLYMOUTH"] <- "DODGE"
data.set$Make[data.set$Make == "TOYOTA SCION"] <- "SCION"
data.set$Make[data.set$Make == "HUMMER"] <- "GMC"

# Generating new predictor variables based on model information.  This serves two purposes; it separates potentially important data into
# multiple predictors and it allows for reduction in overall number of vehicle model levels.

data.set <- data.set %>% mutate(DriveType = case_when(grepl("AWD", Model) == TRUE ~ "AWD",
                                                      grepl("4WD", Model) == TRUE ~ "4WD",
                                                      grepl("AWD|4WD", Model) == FALSE ~ "2WD"),
                                NumCyls = case_when(grepl("V8|8C|V-8|V 8", Model) == TRUE ~ 8,
                                                    grepl("V6|I6|6C|V-6|I-6|V 6|I 6", Model) == TRUE ~ 6,
                                                    grepl("I4|4C|I-4|I 4", Model) == TRUE ~ 4,
                                                    grepl("V8|V6|I6|I4|8C|6C|4C|V-8|V-6|I-4|I 4|V 8|V 6|I 6", Model) == FALSE ~ 0)) %>%
                        mutate(DriveWheels = case_when(grepl("FWD", Model) == TRUE ~ "Front",
                                                       grepl("RWD", Model) == TRUE ~ "Rear",
                                                       grepl("AWD", Model) == TRUE ~ "All"),
                               Model = gsub(" AWD| 4WD| 2WD| V8| V6| I6| I4| RWD| FWD| 8C| 6C| 4C| PICKUP| V-8| V-6", "", Model)) %>%
                        mutate(Model = gsub(" I-4| I-6| I 6| I 4| V 6| V 8| PICKU| PIC", "", Model)) %>% 
                        mutate(LitersDisp = as.numeric(str_extract(Model, "[0-9]\\.[0.9]"))) %>%
                        mutate(Model = gsub(" [0-9]\\.[0-9]L| EFI| MPI| SPI| MFI| SFI|[[:punct:]]", "", Model)) %>%
                        mutate(Model = trimws(gsub("HYBRID", "", Model)))

# Normalizing entries for vehicle model data

data.set$Model <- gsub("1500.*SIERRA.*", "1500 SIERRA", data.set$Model)
data.set$Model <- gsub("1500.*SILVERADO.*", "1500 SILVERADO", data.set$Model)
data.set$Model <- gsub("2500.*SILVERADO.*", "2500 SILVERADO", data.set$Model)
data.set$Model <- gsub("300.*", "300", data.set$Model)
data.set$Model <- gsub("32 CL.*", "32 CL", data.set$Model)
data.set$Model <- gsub("32 TL.*", "32 TL", data.set$Model)
data.set$Model <- gsub("350Z.*", "350Z", data.set$Model)
data.set$Model <- gsub("626.*", "626", data.set$Model)
data.set$Model <- gsub("ACCORD.*", "ACCORD", data.set$Model)
data.set$Model <- gsub("AERIO.*", "AERIO", data.set$Model)
data.set$Model <- gsub("ALERO.*", "ALERO", data.set$Model)
data.set$Model <- gsub("ALTIMA.*", "ALTIMA", data.set$Model)
data.set$Model <- gsub("ARMADA.*", "ARMADA", data.set$Model)
data.set$Model <- gsub("AVALON.*", "AVALON", data.set$Model)
data.set$Model <- gsub("AVENGER.*", "AVENGER", data.set$Model)
data.set$Model <- gsub("AVEO.*", "AVEO", data.set$Model)
data.set$Model <- gsub("BONNEVILLE.*", "BONNEVILLE", data.set$Model)
data.set$Model <- gsub("CAMARO.*", "CAMARO", data.set$Model)
data.set$Model <- gsub("CAMRY.*SOLARA.*", "SOLARO", data.set$Model)
data.set$Model <- gsub("CAMRY.*", "CAMRY", data.set$Model)
data.set$Model <- gsub("CARAVAN SE.*", "CARAVAN", data.set$Model)
data.set$Model <- gsub("CANYON.*", "CANYON", data.set$Model)
data.set$Model <- gsub("CENTURY.*", "CENTURY", data.set$Model)
data.set$Model <- gsub("CHARGER.*", "CHARGER", data.set$Model)
data.set$Model <- gsub("CIVIC.*", "CIVIC", data.set$Model)
data.set$Model <- gsub("COMMANDER.*", "COMMANDER", data.set$Model)
data.set$Model <- gsub("CENTURY.*", "CENTURY", data.set$Model)
data.set$Model <- gsub("COOPER.*", "COOPER", data.set$Model)
data.set$Model <- gsub("COROLLA.*", "COROLLA", data.set$Model)
data.set$Model <- gsub("COUPE.*", "COUPE", data.set$Model)
data.set$Model <- gsub("CRV.*", "CRV", data.set$Model)
data.set$Model <- gsub("DURANGO.*", "DURANGO", data.set$Model)
data.set$Model <- gsub("ECHO.*", "ECHO", data.set$Model)
data.set$Model <- gsub("ECLIPSE.*", "ECLIPSE", data.set$Model)
data.set$Model <- gsub("ENVOY.*", "ENVOY", data.set$Model)
data.set$Model <- gsub("ES300.*", "ES300", data.set$Model)
data.set$Model <- gsub("ESCAPE.*", "ESCAPE", data.set$Model)
data.set$Model <- gsub("ESCORT.*", "ESCORT", data.set$Model)
data.set$Model <- gsub("EXCURSION.*", "EXCURSION", data.set$Model)
data.set$Model <- gsub("EXPEDITION.*", "EXPEDITION", data.set$Model)
data.set$Model <- gsub("F150.*", "F150", data.set$Model)
data.set$Model <- gsub("F250.*", "F250", data.set$Model)
data.set$Model <- gsub("FOCUS.*", "FOCUS", data.set$Model)
data.set$Model <- gsub("FORENZA.*", "FORENZA", data.set$Model)
data.set$Model <- gsub("FREESTYLE.*", "FREESTYLE", data.set$Model)
data.set$Model <- gsub("FUSION.*", "FUSION", data.set$Model)
data.set$Model <- gsub("G35.*", "G35", data.set$Model)
data.set$Model <- gsub("GALANT.*", "GALANT", data.set$Model)
data.set$Model <- gsub("GRAND AM.*", "GRAND AM", data.set$Model)
data.set$Model <- gsub("GRAND CHEROKEE.*", "GRAND CHEROKEE", data.set$Model)
data.set$Model <- gsub("GRAND MARQUIS.*", "GRAND MARQUIS", data.set$Model)
data.set$Model <- gsub("GRAND PRIX.*", "GRAND PRIX", data.set$Model)
data.set$Model <- gsub("GRAND VITARA.*", "GRAND VITARA", data.set$Model)
data.set$Model <- gsub("HHR.*", "HHR", data.set$Model)
data.set$Model <- gsub("HIGHLANDER.*", "HIGHLANDER", data.set$Model)
data.set$Model <- gsub("I30.*", "I30", data.set$Model)
data.set$Model <- gsub("I35.*", "I35", data.set$Model)
data.set$Model <- gsub("IMPALA.*", "IMPALA", data.set$Model)
data.set$Model <- gsub("ION.*", "ION", data.set$Model)
data.set$Model <- gsub("JETTA.*", "JETTA", data.set$Model)
data.set$Model <- gsub("L SERIES.*", "L SERIES", data.set$Model)
data.set$Model <- gsub("LE SABRE.*", "LE SABRE", data.set$Model)
data.set$Model <- gsub("^LS *", "LS", data.set$Model)
data.set$Model <- gsub("LS DO", "LS", data.set$Model)
data.set$Model <- gsub("LUCERNE.*", "LUCERNE", data.set$Model)
data.set$Model <- gsub("M45.*", "M45", data.set$Model)
data.set$Model <- gsub("MAGNUM.*", "MAGNUM", data.set$Model)
data.set$Model <- gsub("MALIBU.*", "MALIBU", data.set$Model)
data.set$Model <- gsub("MATRIX.*", "MATRIX", data.set$Model)
data.set$Model <- gsub("MAXIMA.*", "MAXIMA", data.set$Model)
data.set$Model <- gsub("MAZDA6.*", "MAZDA6", data.set$Model)
data.set$Model <- gsub("MDX.*", "MDX", data.set$Model)
data.set$Model <- gsub("MIATA.*", "MIATA", data.set$Model)
data.set$Model <- gsub("MONTANA.*", "MONTANA", data.set$Model)
data.set$Model <- gsub("MPV.*", "MPV", data.set$Model)
data.set$Model <- gsub("MOUNTAINEER.*", "MOUNTAINEER", data.set$Model)
data.set$Model <- gsub("MURANO.*", "MURANO", data.set$Model)
data.set$Model <- gsub("MUSTANG.*", "MUSTANG", data.set$Model)
data.set$Model <- gsub("NAVIGATOR.*", "NAVIGATOR", data.set$Model)
data.set$Model <- gsub("NEON.*", "NEON", data.set$Model)
data.set$Model <- gsub("OPTIMA.*", "OPTIMA", data.set$Model)
data.set$Model <- gsub("OUTLANDER.*", "OUTLANDER", data.set$Model)
data.set$Model <- gsub("PACIFICA.*", "PACIFICA", data.set$Model, ignore.case = TRUE)
data.set$Model <- gsub("PATHFINDER.*", "PATHFINDER", data.set$Model)
data.set$Model <- gsub("PILOT.*", "PILOT", data.set$Model)
data.set$Model <- gsub("PRIUS.*", "PRIUS", data.set$Model)
data.set$Model <- gsub("PRIZM.*", "PRIZM", data.set$Model)
data.set$Model <- gsub("PROTEGE 5.*", "PROTEGE 5", data.set$Model)
data.set$Model <- gsub("PT CRUISER.*", "PT CRUISER", data.set$Model)
data.set$Model <- gsub("QX4.*", "QX4", data.set$Model)
data.set$Model <- gsub("RAIDER.*", "RAIDER", data.set$Model)
data.set$Model <- gsub("RAV4.*", "RAV4", data.set$Model)
data.set$Model <- gsub("RENO.*", "RENO", data.set$Model)
data.set$Model <- gsub("RIO.*", "RIO", data.set$Model)
data.set$Model <- gsub("RONDO.*", "RONDO", data.set$Model)
data.set$Model <- gsub("RX300.*", "RX300", data.set$Model)
data.set$Model <- gsub("S SERIES.*", "S SERIES", data.set$Model)
data.set$Model <- gsub("S10.*", "S10", data.set$Model)
data.set$Model <- gsub("SABLE.*", "SABLE", data.set$Model)
data.set$Model <- gsub("SANTA FE.*", "SANTA FE", data.set$Model)
data.set$Model <- gsub("SEBRING.*", "SEBRING", data.set$Model)
data.set$Model <- gsub("SENTRA.*", "SENTRA", data.set$Model)
data.set$Model <- gsub("SIENNA.*", "SIENNA", data.set$Model)
data.set$Model <- gsub("SIERRA.*1500.*", "1500 SIERRA", data.set$Model)
data.set$Model <- gsub("SILHOUETTE.*", "SILHOUETTE", data.set$Model)
data.set$Model <- gsub("SONATA.*", "SONATA", data.set$Model)
data.set$Model <- gsub("SPECTRA.*", "SPECTRA", data.set$Model)
data.set$Model <- gsub("SPORTAGE.*", "SPORTAGE", data.set$Model)
data.set$Model <- gsub("SRX.*", "SRX", data.set$Model)
data.set$Model <- gsub("STRATUS.*", "STRATUS", data.set$Model)
data.set$Model <- gsub("SUBURBAN 1500.*", "SUBURBAN 1500", data.set$Model)
data.set$Model <- gsub("SUBURBAN 2500.*", "SUBURBAN 2500", data.set$Model)
data.set$Model <- gsub("SUNFIRE.*", "SUNFIRE", data.set$Model)
data.set$Model <- gsub("TAHOE.*", "TAHOE", data.set$Model)
data.set$Model <- gsub("TAURUS.*", "TAURUS", data.set$Model)
data.set$Model <- gsub("TC.*", "TC", data.set$Model)
data.set$Model <- gsub("TRAILBLAZER.*", "TRAILBLAZER", data.set$Model)
data.set$Model <- gsub("TUCSON.*", "TUCSON", data.set$Model)
data.set$Model <- gsub("VERONA.*", "VERONA", data.set$Model)
data.set$Model <- gsub("VIBE.*", "VIBE", data.set$Model)
data.set$Model <- gsub("VITARA.*", "VITARA", data.set$Model)
data.set$Model <- gsub("VUE.*", "VUE", data.set$Model)
data.set$Model <- gsub("XB.*", "XB", data.set$Model)
data.set$Model <- gsub("XG 300.*", "XG 300", data.set$Model)
data.set$Model <- gsub("XG 350.*", "XG 350", data.set$Model)
data.set$Model <- gsub("XL7.*", "XL7", data.set$Model)
data.set$Model <- gsub("XTERRA.*", "XTERRA", data.set$Model)
data.set$Model <- gsub("YUKON XL 1500.*", "YUKON XL 1500", data.set$Model)
data.set$Model <- as.factor(data.set$Model)

# Imputing new factor level for listing where vehicle trim is not listed

data.set$Trim[is.na(data.set$Trim)] <- "Bas"
data.set$Trim[data.set$Trim == ""] <- "Bas"
data.set$Trim <- as.factor(data.set$Trim)

# Extracting data from vehicle submodel to be used for new predictor variable, liters of displacement

isnaLD <- is.na(data.set$LitersDisp)
data.set$LitersDisp[isnaLD] <- as.numeric(str_extract(data.set$SubModel[isnaLD], "[1-9]\\.[0-9]"))
data.set$SubModel <- gsub("[1-9]\\.[0-9]L", "", data.set$SubModel)
data.set$SubModel <- gsub("[1-9]\\.[0.9]", "", data.set$SubModel)
rm(isnaLD)

# Checking data regarding engine size (LitersDisp).  Unfortunately, there are far too many NA values to be of much use.  Removing
# LitersDisp field from dataset

sum(is.na(data.set$LitersDisp))
data.set <- select(data.set, -LitersDisp)

# Cleaning and normalizing data for vehicle submodel information

subModelTerms <- paste(data.set$SubModel, collapse = " ")
subModelTerms <- str_split(subModelTerms, " ")
t <- table(subModelTerms)
length(t)
data.set$SubModel <- trimws(gsub(" *\\b[[:alnum:]]{1,2}\\b* | *\\b[[:alnum:]]{1,2}\\b*", " ", data.set$SubModel))
data.set$SubModel <- gsub(".*UTILITY.*", "SUV", data.set$SubModel)
data.set$SubModel <- gsub(".*SUV.*|.*CUV.*|.*CUV.*", "SUV", data.set$SubModel)
data.set$SubModel <- gsub(".*SEDAN.*", "SEDAN", data.set$SubModel)
data.set$SubModel <- gsub(".*COUPE.*", "COUPE", data.set$SubModel)
data.set$SubModel <- gsub(".*QUAD CAB.*", "QUAD CAB", data.set$SubModel)
data.set$SubModel <- gsub(".*MINIVAN.*", "MINIVAN", data.set$SubModel)
data.set$SubModel <- gsub(".*WAGON.*", "WAGON", data.set$SubModel)
data.set$SubModel <- gsub(".*REG.*CAB.*", "REG CAB", data.set$SubModel)
data.set$SubModel <- gsub(".*CONVERTIBLE.*", "CONVERTIBLE", data.set$SubModel)
data.set$SubModel <- gsub(".*SPYDER.*", "CONVERTIBLE", data.set$SubModel)
data.set$SubModel <- gsub(".*ROADSTER.*", "CONVERTIBLE", data.set$SubModel)
data.set$SubModel <- gsub(".*LARIAT.*", "LARIAT", data.set$SubModel)
data.set$SubModel <- gsub(".*CREW.*", "CREW", data.set$SubModel)
data.set$SubModel <- gsub(".*HARDTOP.*|.*HARTOP.*", "HARDTOP", data.set$SubModel)
data.set$SubModel <- gsub(".*SR5.*", "SR5", data.set$SubModel)
data.set$SubModel <- gsub(".*TOUR.*", "TOURING", data.set$SubModel)
data.set$SubModel <- gsub(".*HAT.*BACK.*", "HATCHBACK", data.set$SubModel)
data.set$SubModel <- gsub(".*CROSS.*", "CROSSOVER", data.set$SubModel)
data.set$SubModel <- gsub(".*EXT.*", "EXT", data.set$SubModel)
data.set$SubModel <- gsub(".*DOUBLE CAB.*", "QUAD CAB", data.set$SubModel)
data.set$SubModel <- gsub(".*SPORT.*", "SPORT", data.set$SubModel)
data.set$SubModel <- gsub(".*MEGA CAB.*", "QUAD CAB", data.set$SubModel)
data.set$SubModel <- gsub(".*PASSENGER.*", "", data.set$SubModel)
data.set$SubModel <- gsub(".*MAZDA3.*", "", data.set$SubModel)
data.set$SubModel <- gsub(".*JEEP.*", "", data.set$SubModel)
data.set$SubModel <- gsub(".*NULL.*", "", data.set$SubModel)
data.set$SubModel[data.set$SubModel == ""] <- "MINIVAN"
data.set$SubModel <- as.factor(data.set$SubModel)
rm(subModelTerms)

# Examining basic information regarding vehicle colors

table(data.set$Color)
data.set$Color[data.set$Color == "NOT AVAIL"] <- "NULL" 
data.set$Color[data.set$Color == ""] <- "NULL" 
data.set$Color <- as.factor(data.set$Color)

# Examining basic information regarding vehicle transmission type.  Since an overwhelming majority of vehicles manufactured for the US market
# are automatics, NA entries were converted to Automatic.

table(data.set$Transmission)
data.set$Transmission <- gsub("Manual", "MANUAL", data.set$Transmission)
data.set$Transmission <- gsub("NULL", "AUTO", data.set$Transmission)
data.set$Transmission[data.set$Transmission == ""] <- "AUTO"
data.set$Transmission <- as.numeric(as.factor(data.set$Transmission)) - 1

# Confirming that WheelType and WheelTypeID are very highly correlated predictors and removing WheelTypeID from the dataset

table(data.set$WheelType, data.set$WheelTypeID)
data.set$WheelType[data.set$WheelType == "NULL"] <- NA
data.set$WheelType <- as.factor(data.set$WheelType)
data.set <- select(data.set, -WheelTypeID)

# Examining, specifiying and correcting listings for country of manufacture and generating predictor reflecting manufacturer's
# parent company

table(data.set$Make, data.set$Nationality)
unique(data.set$Make[data.set$Nationality == "TOP LINE ASIAN"])
data.set <- data.set %>% mutate(Nationality = case_when(Make == "ACURA" ~ "JAPANESE",
                                                        Make == "BUICK" ~ "AMERICAN",
                                                        Make == "CADILLAC" ~ "AMERICAN",
                                                        Make == "CHEVROLET" ~ "AMERICAN", 
                                                        Make == "CHRYSLER" ~ "AMERICAN",
                                                        Make == "DODGE" ~ "AMERICAN", 
                                                        Make == "FORD" ~ "AMERICAN",
                                                        Make == "GMC" ~ "AMERICAN",
                                                        Make == "HONDA" ~ "JAPANESE",
                                                        Make == "HYUNDAI" ~ "KOREAN",
                                                        Make == "INFINITI" ~ "JAPANESE",
                                                        Make == "ISUZU" ~ "JAPANESE",
                                                        Make == "JEEP" ~ "AMERICAN",
                                                        Make == "KIA" ~ "KOREAN",
                                                        Make == "LEXUS" ~ "JAPANESE",
                                                        Make == "LINCOLN" ~ "AMERICAN",
                                                        Make == "MAZDA" ~ "JAPANESE",
                                                        Make == "MERCURY" ~ "AMERICAN",
                                                        Make == "MINI" ~ "EUROPEAN",
                                                        Make == "MITSUBISHI" ~ "JAPANESE",
                                                        Make == "NISSAN" ~ "JAPANESE",
                                                        Make == "OLDSMOBILE" ~ "AMERICAN",
                                                        Make == "PLYMOUTH" ~ "AMERICAN",
                                                        Make == "PONTIAC" ~ "AMERICAN",
                                                        Make == "SATURN" ~ "AMERICAN",
                                                        Make == "SCION" ~ "JAPANESE",
                                                        Make == "SUBARU" ~ "JAPANESE",
                                                        Make == "SUZUKI" ~ "JAPANESE",
                                                        Make == "TOYOTA" ~ "JAPANESE",
                                                        Make == "VOLKSWAGEN" ~ "EUROPEAN",
                                                        Make == "VOLVO" ~ "EUROPEAN"),
                                ParentCompany = case_when(Make == "ACURA" ~ "HONDA",
                                                          Make == "BUICK" ~ "GM",
                                                          Make == "CADILLAC" ~ "GM",
                                                          Make == "CHEVROLET" ~ "GM", 
                                                          Make == "CHRYSLER" ~ "CHRYSLER",
                                                          Make == "DODGE" ~ "CHRYSLER", 
                                                          Make == "FORD" ~ "FORD",
                                                          Make == "GMC" ~ "GMC",
                                                          Make == "HONDA" ~ "HONDA",
                                                          Make == "HYUNDAI" ~ "HYUNDAI",
                                                          Make == "INFINITI" ~ "NISSAN",
                                                          Make == "ISUZU" ~ "ISUZU",
                                                          Make == "JEEP" ~ "CHRYSLER",
                                                          Make == "KIA" ~ "KIA",
                                                          Make == "LEXUS" ~ "TOYOTA",
                                                          Make == "LINCOLN" ~ "FORD",
                                                          Make == "MAZDA" ~ "MAZDA",
                                                          Make == "MERCURY" ~ "FORD",
                                                          Make == "MINI" ~ "BMW",
                                                          Make == "MITSUBISHI" ~ "MITSUBISHI",
                                                          Make == "NISSAN" ~ "NISSAN",
                                                          Make == "OLDSMOBILE" ~ "GM",
                                                          Make == "PLYMOUTH" ~ "CHRYSLER",
                                                          Make == "PONTIAC" ~ "GM",
                                                          Make == "SATURN" ~ "GM",
                                                          Make == "SCION" ~ "TOYOTA",
                                                          Make == "SUBARU" ~ "SUBARU",
                                                          Make == "SUZUKI" ~ "SUZUKI",
                                                          Make == "TOYOTA" ~ "TOYOTA",
                                                          Make == "VOLKSWAGEN" ~ "VOLKSWAGEN",
                                                          Make == "VOLVO" ~ "VOLVO"))
data.set$Nationality <- as.factor(data.set$Nationality)
data.set$ParentCompany <- as.factor(data.set$ParentCompany)
data.set <- select(data.set, -TopThreeAmericanName)

# Examining and confirming that DriveType field doesn't contain any extraneous/NA information

table(data.set$DriveType)
sum(is.na(data.set$DriveType))
data.set$DriveType <- as.factor(data.set$DriveType)

# Examining vehicle Size field.  Converting NULL data to NA for later imputation calculations

table(data.set$Size)
data.set$Size[data.set$Size == "NULL"] <- NA
data.set$Size <- as.factor(data.set$Size)

# Examining and manually imputting data regarding vehicle DriveWheels information

table(data.set$DriveWheels)
data.set$DriveWheels <- as.factor(data.set$DriveWheels)
levels(data.set$DriveWheels) <- c(levels(data.set$DriveWheels), "Truck", "SUV")
str(data.set$DriveWheels)
sum(is.na(data.set$DriveWheels))    
data.set$DriveWheels[data.set$SubModel == "MINIVAN"] <- "Front"
data.set$DriveWheels[data.set$Make == "ACURA"] <- "Front"
data.set$DriveWheels[data.set$Make == "BUICK"] <- "Front"
data.set$DriveWheels[data.set$Make == "CHRYSLER"] <- "Front"
data.set$DriveWheels[data.set$Make == "HONDA"] <- "Front"
data.set$DriveWheels[data.set$Make == "OLDSMOBILE"] <- "Front"
data.set$DriveWheels[data.set$Make == "MINI"] <- "Front"
data.set$DriveWheels[data.set$Make == "HUMMER"] <- "All"
data.set$DriveWheels[data.set$Make == "HYUNDAI"] <- "Front"
data.set$DriveWheels[data.set$Make == "ISUZU"] <- "All"
data.set$DriveWheels[data.set$Make == "KIA"] <- "Front"
data.set$DriveWheels[data.set$Make == "SCION"] <- "Front"
data.set$DriveWheels[data.set$Make == "SUZUKI"] <- "Front"
data.set$DriveWheels[grep(".*TRUCK.*", data.set$Size)] <- "Truck"
data.set$DriveWheels[grep(".*SUV.*", data.set$Size)] <- "SUV"
data.set$DriveWheels[grep("^VAN", data.set$Size)] <- "Rear"
data.set$DriveWheels[grep("CROWN VICTORIA|GRAND MARQUIS|GS300|GS350|GS450H|HHR|M35|M45|MAGNUM|MIATA|MR2 SPYDER|MUSTANG|Q45|LS|SOLSTICE", data.set$Model)] <- "Rear"
data.set$DriveWheels[data.set$Make == "SUBARU"] <- "All"
data.set$DriveWheels[data.set$Make == "JEEP"] <- "All"
data.set$DriveWheels[data.set$Submodel == "WRANGLER"] <- "Front"
data.set$DriveWheels[is.na(data.set$DriveWheels)] <- "Front"

# Converting incorrect NumCyls data to NA for imputation calculations

data.set$NumCyls[data.set$NumCyls == 0] <- NA
data.set$NumCyls <- as.factor(data.set$NumCyls)

# Converting values in multiple fields regarding various price levels for each listing that are incomplete, incorrect, etc to NA 
# for imputation purposes

for (i in grep("MMR.*", names(data.set))) {
        n <- union(which(data.set[,i] == "0"), which(data.set[,i] == "NULL"))
        data.set[n,i] <- NA
        data.set[,i] <- as.numeric(data.set[,i])
}

# Examining PRIMEUNIT and AUCGUART fields, noting the high level of correlation between the two.  

table(data.set$PRIMEUNIT)
table(data.set$AUCGUART)
table(data.set$PRIMEUNIT, data.set$AUCGUART)
data.set <- data.set %>% mutate(PRIMEUNIT = as.factor(PRIMEUNIT),
                                PRIMEAUCDISC = ifelse(PRIMEUNIT == "NO" & AUCGUART == "GREEN", 1, 0)) %>%
                         select(-AUCGUART)

# Examining BYRNO data

table(data.set$BYRNO)
data.set$BYRNO <- as.factor(data.set$BYRNO)

# Examining zip code and state data to make sure there are no extraneous/NA values

table(data.set$VNZIP1)
table(data.set$VNST)
data.set$VNZIP1 <- as.factor(data.set$VNZIP1)
data.set$VNST <- as.factor(data.set$VNST)

# Generating means encoding function as a way to encode categorical data with large numbers of levels.  

# means_encoding <- function(dataframe, featurename, dependent) {
#         me <- select(dataframe, featurename, dependent)
#         names(me) <- c("name", "kick")
#         tme <- as.data.frame(table(me$kick))
#         navalue <- tme$Freq[tme$Var1 == 1]/sum(tme$Freq)
#         me$kick <- case_when(is.na(me$kick) == TRUE ~ navalue,
#                                  me$kick == 1 ~ 1,
#                                  me$kick == 0 ~ 0
#         )
#         meanvalue <- 0
#         for (i in 1:nrow(me)) {
#                 temptable <- as.vector(me$kick[me$name == me$name[i]])
#                 sampletable <- sample(temptable, as.integer(round(length(temptable)*0.9)))
#                 mevalue <- mean(sampletable)
#                 meanvalue[i] <- mevalue
#         }
#         meanvalue <- as.numeric(scale(meanvalue))
#         return(meanvalue)
# }

means_encoding <- function(dataframe, featurename, dependent) {
        me <- select(dataframe, featurename, dependent)
        names(me) <- c("name", "kick")
        tme <- as.data.frame(table(me$kick))
        navalue <- tme$Freq[tme$Var1 == 1]/sum(tme$Freq)
        me$kick <- case_when(is.na(me$kick) == TRUE ~ navalue,
                             me$kick == 1 ~ 1,
                             me$kick == 0 ~ 0
        )
        value <- me %>% 
                 group_by(name) %>%
                 summarize(mean_encode = mean(kick))
        m <- merge(me, value, by.x = "name", by.y = "name", all.x = TRUE)
        m <- m[,3]
        m <- m + runif(1, -0.05*m, 0.05*m)
        return(m)
}

# Generating means encoded fields based on VNZIP1, VNST, Make, Model, Nationality, Trim, Parent Company, SubModel, and BYRNO data

data.set <- data.set %>% mutate(meVNZIP1 = means_encoding(data.set, "VNZIP1", "IsBadBuy"),
                                meVNST = means_encoding(data.set, "VNST", "IsBadBuy"),
                                meMake = means_encoding(data.set, "Make", "IsBadBuy"),
                                meModel = means_encoding(data.set, "Model", "IsBadBuy"),
                                meNationality = means_encoding(data.set, "Nationality", "IsBadBuy"),
                                meTrim = means_encoding(data.set, "Trim", "IsBadBuy"),
                                meBYRNO = means_encoding(data.set, "BYRNO", "IsBadBuy"),
                                meAuction = means_encoding(data.set, "Auction", "IsBadBuy"),
                                meSubModel = means_encoding(data.set, "SubModel", "IsBadBuy"),
                                meParentCompany = means_encoding(data.set, "ParentCompany", "IsBadBuy"),
                                meColor = means_encoding(data.set, "Color", "IsBadBuy"),
                                mePRIMEUNIT = means_encoding(data.set, "PRIMEUNIT", "IsBadBuy"),
                                meDayofWeekPurchased = means_encoding(data.set, "DayofWeekPurchased", "IsBadBuy"),
                                meDriveType = means_encoding(data.set, "DriveType", "IsBadBuy"),
                                meDriveWheels = means_encoding(data.set, "DriveWheels", "IsBadBuy"),
                                meYearPurchased = means_encoding(data.set, "YearPurchased", "IsBadBuy"),
                                meMonthPurchased = means_encoding(data.set, "MonthPurchased", "IsBadBuy"),
                                meDayPurchased = means_encoding(data.set, "DayPurchased", "IsBadBuy"))

data.set <- select(data.set, -VNZIP1, -VNST, -Make, -Model, -Nationality, -Trim, -BYRNO, -PurchDate, -Auction, -ParentCompany, -SubModel,
                   -Color, -PRIMEUNIT, -DayofWeekPurchased, -DriveType, -DriveWheels, -YearPurchased, -MonthPurchased,
                   -DayPurchased)

save.image(paste(wd, "carvana.RData", sep = ""))

# Examining pricing distributions and noting any potential irregularities

g <- data.set[,grep("MMR", names(data.set))] 
g <- gather(g, key = "MMRtype")
p <- ggplot(data = g, aes(x= MMRtype, y = value)) + geom_violin()
suppressWarnings(plot(p))
rm(g)

# Vehicle pricing data can be incorrect or incomplete.  While vehicle value changes over time, certain tenets always hold true.  For example, 
# 'Clean' vehicles are always worth more than 'Average' cars and 'Auction' values are always lower than 'Retail' values.  Identifying values 
# that do not follow these basic tenets and setting them to NA for imputation purposes

data.set$MMRAcquisitionAuctionCleanPrice[data.set$MMRAcquisitionAuctionCleanPrice <= min(data.set$MMRAcquisitionAuctionAveragePrice, na.rm = TRUE)] <- NA
data.set$MMRCurrentAuctionCleanPrice[data.set$MMRCurrentAuctionCleanPrice <= min(data.set$MMRCurrentAuctionAveragePrice, na.rm = TRUE)] <- NA

p <- ggplot(data = data.set, aes(x = VehBCost)) + geom_histogram(binwidth = 2000, aes(fill = ..count..))
p <- p + stat_bin(binwidth = 2000, geom = "text", aes(label = ..count..), vjust = -1)
plot(p)
range(data.set$VehBCost)
data.set$VehBCost[data.set$VehBCost == 1] <- NA

# Visualizing missing data

missing <- sample_n(data.set, size = 0.1*nrow(data.set))
vis_miss(missing)
gg_miss_var(data.set, show_pct = TRUE)
rm(missing)

# Creating function that returns the mode of a list of numbers

mode <- function(array) {
        t <- as.data.frame(table(array))
        t <- t[order(t$Freq, decreasing = TRUE),]
        m <- head(t$array, 1)
        return(m)
}

sub.data.numcyls <- select(data.set, VehYear, meTrim, meSubModel, meModel, meMake, NumCyls)
impute.data <- mice(sub.data.numcyls, m = 7, maxit = 5)
data.set$NumCyls[is.na(data.set$NumCyls)] <- apply(impute.data$imp$NumCyls, 1, mode)
rm(sub.data.numcyls);rm(impute.data)
save.image(paste(wd, "carvana2.RData", sep = ""))

sub.data.wheels <- select(data.set, VehYear, meTrim, meSubModel, meModel, meMake, NumCyls, WheelType)
impute.data <- mice(sub.data.wheels, m = 7, maxit = 5)
data.set$WheelType[is.na(data.set$WheelType)] <- apply(impute.data$imp$WheelType, 1, mode)
rm(sub.data.wheels);rm(impute.data)
save.image(paste(wd, "carvana2.RData", sep = ""))

sub.data.MMR <- select(data.set, VehYear, meTrim, meSubModel, meModel, meMake, NumCyls, WheelType, WarrantyCost, AdjDaysSincePurchase, 
                       MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice, MMRAcquisitionRetailAveragePrice,
                       MMRAcquisitionRetailCleanPrice, MMRCurrentAuctionAveragePrice, MMRCurrentAuctionCleanPrice,
                       MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice)
impute.data <- mice(sub.data.MMR, m = 7, maxit = 5, method = "cart")
data.set$MMRAcquisitionAuctionAveragePrice[is.na(data.set$MMRAcquisitionAuctionAveragePrice)] <- apply(impute.data$imp$MMRAcquisitionAuctionAveragePrice, 1, mean)
data.set$MMRAcquisitionAuctionCleanPrice[is.na(data.set$MMRAcquisitionAuctionCleanPrice)] <- apply(impute.data$imp$MMRAcquisitionAuctionCleanPrice, 1, mean)
data.set$MMRAcquisitionRetailAveragePrice[is.na(data.set$MMRAcquisitionRetailAveragePrice)] <- apply(impute.data$imp$MMRAcquisitionRetailAveragePrice, 1, mean)
data.set$MMRAcquisitionRetailCleanPrice[is.na(data.set$MMRAcquisitionRetailCleanPrice)] <- apply(impute.data$imp$MMRAcquisitionRetailCleanPrice, 1, mean)
data.set$MMRCurrentAuctionAveragePrice[is.na(data.set$MMRCurrentAuctionAveragePrice)] <- apply(impute.data$imp$MMRCurrentAuctionAveragePrice, 1, mean)
data.set$MMRCurrentAuctionCleanPrice[is.na(data.set$MMRCurrentAuctionCleanPrice)] <- apply(impute.data$imp$MMRCurrentAuctionCleanPrice, 1, mean)
data.set$MMRCurrentRetailAveragePrice[is.na(data.set$MMRCurrentRetailAveragePrice)] <- apply(impute.data$imp$MMRCurrentRetailAveragePrice, 1, mean)
data.set$MMRCurrentRetailCleanPrice[is.na(data.set$MMRCurrentRetailCleanPrice)] <- apply(impute.data$imp$MMRCurrentRetailCleanPrice, 1, mean)
save.image(paste(wd, "carvana2.RData", sep = ""))

rm(sub.data.MMR);rm(impute.data)

sub.data.size <- select(data.set, meTrim, meModel, meMake, Size)
impute.data <- mice(sub.data.size, m = 7, maxit = 5, method = "cart")
data.set$Size[is.na(data.set$Size)] <- apply(impute.data$imp$Size, 1, mode)
rm(sub.data.size);rm(impute.data)
save.image(paste(wd, "carvana2.RData", sep = ""))

sub.data.cost <- select(data.set, meModel, meMake, VehYear, VehBCost)
impute.data <- mice(sub.data.cost, m = 7, maxit = 5)
data.set$VehBCost[is.na(data.set$VehBCost)] <- apply(impute.data$imp$VehBCost, 1, mean)
rm(sub.data.cost);rm(impute.data)
save.image(paste(wd, "carvana2.RData", sep = ""))

data.set <- data.set %>% mutate(meSize = means_encoding(data.set, "Size", "IsBadBuy"),
                                meNumCyls = means_encoding(data.set, "NumCyls", "IsBadBuy"),
                                meWheelType = means_encoding(data.set, "WheelType", "IsBadBuy")) %>%
                         select(-NumCyls, -Size, -WheelType)

MMRmean <- as.data.frame(apply(data.set[,grep("MMR", names(data.set))], 2, mean))
names(MMRmean) <- c("average")
data.set$MMRAcquisitionAuctionCleanPrice[data.set$MMRAcquisitionAuctionCleanPrice < data.set$MMRAcquisitionAuctionAveragePrice] <-
        data.set$MMRAcquisitionAuctionCleanPrice[data.set$MMRAcquisitionAuctionCleanPrice < data.set$MMRAcquisitionAuctionAveragePrice] * 
        MMRmean$average[2]/MMRmean$average[1]
data.set$MMRAcquisitionRetailAveragePrice[data.set$MMRAcquisitionRetailAveragePrice < data.set$MMRAcquisitionAuctionAveragePrice] <-
        data.set$MMRAcquisitionRetailAveragePrice[data.set$MMRAcquisitionRetailAveragePrice < data.set$MMRAcquisitionAuctionAveragePrice] * 
        MMRmean$average[3]/MMRmean$average[1]
data.set$MMRAcquisitionRetailCleanPrice[data.set$MMRAcquisitionRetailCleanPrice < data.set$MMRAcquisitionRetailAveragePrice] <-
        data.set$MMRAcquisitionRetailCleanPrice[data.set$MMRAcquisitionRetailCleanPrice < data.set$MMRAcquisitionRetailAveragePrice] * 
        MMRmean$average[4]/MMRmean$average[3]
data.set$MMRCurrentAuctionCleanPrice[data.set$MMRCurrentAuctionCleanPrice < data.set$MMRCurrentAuctionAveragePrice] <-
        data.set$MMRCurrentAuctionCleanPrice[data.set$MMRCurrentAuctionCleanPrice < data.set$MMRCurrentAuctionAveragePrice] * 
        MMRmean$average[6]/MMRmean$average[5]
data.set$MMRCurrentRetailAveragePrice[data.set$MMRCurrentRetailAveragePrice < data.set$MMRCurrentAuctionAveragePrice] <-
        data.set$MMRCurrentRetailAveragePrice[data.set$MMRCurrentRetailAveragePrice < data.set$MMRCurrentAuctionAveragePrice] * 
        MMRmean$average[7]/MMRmean$average[5]
data.set$MMRCurrentRetailCleanPrice[data.set$MMRCurrentRetailCleanPrice < data.set$MMRCurrentRetailAveragePrice] <-
        data.set$MMRCurrentRetailCleanPrice[data.set$MMRCurrentRetailCleanPrice < data.set$MMRCurrentRetailAveragePrice] * 
        MMRmean$average[8]/MMRmean$average[7]

data.set <- data.set %>% mutate(AARA = MMRAcquisitionAuctionCleanPrice - MMRAcquisitionAuctionAveragePrice,
                                ARCA = MMRAcquisitionRetailCleanPrice - MMRAcquisitionRetailAveragePrice,
                                CACA = MMRCurrentAuctionCleanPrice - MMRCurrentAuctionAveragePrice,
                                CRCA = MMRCurrentRetailCleanPrice - MMRCurrentRetailAveragePrice,
                                ARAA = MMRAcquisitionRetailAveragePrice - MMRAcquisitionAuctionAveragePrice,
                                ARAC = MMRAcquisitionRetailCleanPrice - MMRAcquisitionAuctionCleanPrice,
                                CRAA = MMRCurrentRetailAveragePrice - MMRCurrentAuctionAveragePrice,
                                CRAC = MMRCurrentRetailCleanPrice - MMRCurrentAuctionCleanPrice,
                                mpy = VehOdo/(VehicleAge + 1),
                                cpy = VehBCost/(VehicleAge + 1),
                                cpm = VehBCost/VehOdo,
                                wpc = WarrantyCost/VehBCost,
                                VehYear = as.numeric(VehYear),
                                VehicleAge = as.numeric(VehicleAge),
                                VehOdo = as.numeric(VehOdo),
                                IsOnlineSale = as.numeric(IsOnlineSale),
                                WarrantyCost = as.numeric(WarrantyCost))

nn <- select(data.set, RefId, IsBadBuy, set)
data.set <- select(data.set, -RefId, -IsBadBuy, -set)
correlation <- cor(data.set)
fc <- findCorrelation(correlation, cutoff = 0.98)
data.set <- as.data.frame(cbind(nn, data.set[,-fc]))
rm(nn)

names(data.set)[grep("=| ", names(data.set))] <- gsub("=| ", "", names(data.set)[grep("=", names(data.set))])

test.ID <- data.set$RefId[data.set$set == "test"]
train.ID <- data.set$RefId[data.set$set == "train"]

to.be.scaled <- grep("VehYear|VehicleAge|VehOdo|MMR.*|VehBCost|WarrantyCost|YearPurchased|MonthPurchased|DayPurchase|YearDiscrepancy|AdjDaysSincePurchase|
                     AARA|ARCA|CACA|CRCA|ARAC|CRAC|mpy|cpy|meNationality|meDriveWheels|meDriveTYpe", 
                     names(data.set))
data.set[,to.be.scaled] <- sapply(to.be.scaled, function(x) scale(data.set[,x]))

save.image(paste(wd, "carvana2.RData", sep = ""))

###

training.set <- data.set %>% filter(set == "train") %>%
                             select(-set, -RefId)
test.set <- data.set %>% filter(set == "test") %>%
                         select(-IsBadBuy, -set, -RefId)

# nrtr <- nrow(training.set)
# nrte <- nrow(test.set)
# training.set <- training.set %>% mutate(adjId = RefId,
#                                         scaleID = scale(RefId)) %>%
#         select(-RefId)
# test.set <- test.set %>% mutate(adjId = (as.numeric(RefId) - nrtr)*nrtr/nrte,
#                                 scaleID = scale(RefId)) %>%
#         select(-RefId)

training.set$IsBadBuy <- as.factor(training.set$IsBadBuy)

table(training.set$IsBadBuy)
training.set <- SMOTE(IsBadBuy~., data = training.set, perc.over = 315, perc.under = 115, k = 4)
table(training.set$IsBadBuy)

save.image(paste(wd, "carvana3.RData", sep = ""))

###

intrain <- createDataPartition(training.set$IsBadBuy, p = 0.7, list = FALSE)
val.set <- training.set[-intrain,]
train.set <- training.set[intrain,]
train.y <- as.matrix(train.set$IsBadBuy)
train.x <- as.matrix(select(train.set, -IsBadBuy))
val.y <- as.matrix(val.set$IsBadBuy)
val.x <- as.matrix(select(val.set, -IsBadBuy))

tg <- list(max_depth = 5,
           eta = 0.01,
           gamma = 0,
           colsample_bytree = 1,
           min_child_weight = 0.5,
           subsample = 1,
           num_class = 2,
           eval_metric = "mlogloss",
           objective = "multi:softprob")

gb <- xgboost(data = train.x,
              label = train.y,
              params = tg,
              nrounds = 7000,
              verbose = 1,
              print_every_n = 500)

gb.predict <- predict(gb, val.x)
gb.predict <- as.data.frame(matrix(data = gb.predict, nrow = 0.5*length(gb.predict), ncol = 2, byrow = TRUE))
names(gb.predict) <- c("zero", "one")
# gb.predict$class <- ifelse(gb.predict$one >= gb.predict$zero, 1, 0)
# confusionMatrix(as.factor(gb.predict$class), as.factor(val.y))
save.image(paste(wd, "carvana4.RData", sep = ""))

###

intrain <- createDataPartition(training.set$IsBadBuy, p = 0.7, list = FALSE)
val.set <- training.set[-intrain,]
train.set <- training.set[intrain,]

rf <- randomForest(IsBadBuy~., 
                   data = train.set, 
                   ntree = 1000,
                   importance = TRUE,
                   keep.forest = TRUE)

rf.predict <- predict(rf, select(val.set, -IsBadBuy))
confusionMatrix(as.factor(rf.predict), as.factor(val.y))

##

svmachine <- svm(IsBadBuy~., 
                 data = train.set,
                 kernel = "radial",
                 probability = TRUE)
sv.predict <- predict(svmachine, select(val.set, -IsBadBuy))
confusionMatrix(as.factor(sv.predict), as.factor(val.y))


save.image(paste(wd, "carvana5.RData", sep = ""))

###

intrain <- createDataPartition(training.set$IsBadBuy, p = 0.7, list = FALSE)
val.set <- training.set[-intrain,]
train.set <- training.set[intrain,]

gl <- glm(IsBadBuy~., data = train.set, family = "binomial")
gl.predict <- predict(gl, select(val.set, -IsBadBuy))
# gl.predict <- as.factor(ifelse(gl.predict <= 0, 0, 1))
# confusionMatrix(val.set$IsBadBuy, gl.predict)

li <- lm(as.numeric(IsBadBuy)~., data = train.set)
li.predict <- predict(li, select(val.set, -IsBadBuy))
# li.predict <- ifelse(li.predict < 1.5, 0, 1)
# confusionMatrix(as.factor(li.predict), val.set$IsBadBuy)

###

intrain <- createDataPartition(training.set$IsBadBuy, p = 0.7, list = FALSE)
val.set <- training.set[-intrain,]
train.set <- training.set[intrain,]
train.y <- as.matrix(as.numeric(train.set$IsBadBuy) - 1)
train.y <- ifelse(train.y == 1, 1, -1)
train.x <- as.matrix(select(train.set, -IsBadBuy))
val.y <- as.matrix(val.set$IsBadBuy)
val.x <- as.matrix(select(val.set, -IsBadBuy))

ab <- adaboost(X = train.x, y = train.y, tree_depth = 5, n_rounds = 1200)
ab.predict <- predict(ab, val.x)
# ab.predict <- ifelse(ab.predict == 1, 1, 0)
# confusionMatrix(as.factor(val.y), as.factor(ab.predict))
###


kn <- kNN(IsBadBuy~., train.set, val.set, norm = FALSE, k = 100, l = 0, prob = TRUE, use.all = TRUE)                
#####

ens <- data.frame(boosted = gb.predict$one,
                  randomforest = as.numeric(rf.predict),
                  ada = ab.predict,
                  logistic = gl.predict,
                  linear = li.predict,
                  svm = as.numeric(sv.predict),
                  knn = attributes(kn)$prob,
                  result = as.factor(val.y))

rf.ens <- randomForest(result~., 
                       data = ens, 
                       ntree = 250,
                       importance = TRUE,
                       keep.forest = TRUE)

save.image(paste(wd, "carvana6.RData", sep = ""))

final.gbm.predict <- predict(gb, as.matrix(test.set))
final.rf.predict <- predict(rf, test.set)
final.svm.predict <- predict(svmachine, test.set)
final.ada.predict <- predict(ab, as.matrix(test.set))
final.gl.predict <- predict(gl, test.set)
final.li.predict <- predict(li, test.set)
final.knn.predict <- kNN(IsBadBuy~., train.set, cbind(test.set, IsBadBuy = 0), norm = FALSE, k = 100, l = 0, prob = TRUE, use.all = TRUE)
final.gbm.predict <- as.data.frame(matrix(data = final.gbm.predict, nrow = 0.5*length(final.gbm.predict), ncol = 2, byrow = TRUE))
names(final.gbm.predict) <- c("zero", "one")
final.gbm.predict <- final.gbm.predict$one

save.image(paste(wd, "carvana7.RData", sep = ""))

edf <- data.frame(boosted = final.gbm.predict,
                  randomforest = as.numeric(final.rf.predict),
                  ada = final.ada.predict,
                  logistic = final.gl.predict,
                  linear = final.li.predict,
                  svm = as.numeric(final.svm.predict),
                  knn = attributes(final.knn.predict)$prob)             
final <- predict(rf.ens, edf)
final <- as.data.frame(cbind(test.ID, final))
names(final) <- c("RefId", "IsBadBuy")
final$IsBadBuy <- final$IsBadBuy - 1
write.csv(final, paste(wd, "finalguess.csv"), row.names = FALSE)



# 
# intrain <- createDataPartition(training.set$IsBadBuy, p = 0.7, list = FALSE)
# val.set <- training.set[-intrain,]
# train.set <- training.set[intrain,]
# 
# train.y <- as.matrix(train.set$IsBadBuy)
# train.x <- as.matrix(select(train.set, -IsBadBuy))
# val.x <- as.matrix(select(val.set, -IsBadBuy))
# val.y <- as.matrix(val.set$IsBadBuy)
# 
# train.y <- to_categorical(train.y, num_classes = 2)
# 
# set.seed(314)
# k.model <- keras_model_sequential() %>%
#         layer_dense(units = 512, activation = "relu", input_shape = dim(train.x)[[2]]) %>%
#         layer_dropout(0.40) %>%  
#         layer_dense(units = 256, activation = "sigmoid") %>%
#         layer_dropout(0.40) %>%
#         layer_dense(units = 128, activation = "sigmoid") %>%
#         layer_dropout(0.40) %>%
#         layer_dense(units = 64, activation = "sigmoid") %>%
#         layer_dropout(0.40) %>%
#         layer_dense(units = 2)
# 
# k.model %>% compile(optimizer = optimizer_adam(lr = 0.001),
#                     loss = "mean_squared_logarithmic_error",
#                     metric = c("accuracy"))
# 
# k.history <- k.model %>% fit(train.x, 
#                              train.y, 
#                              epochs = 50,
#                              batch_size = 2000, 
#                              verbose = 1,
#                              shuffle = TRUE)
# k.history
# plot(k.history)
# k.predict <- predict_classes(k.model, val.x)
# k.predict <- ifelse(k.predict > 0, "1", "0")
# confusionMatrix(as.factor(k.predict), as.factor(val.y))


