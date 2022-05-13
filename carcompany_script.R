
library(readxl)
library(ggplot2)
library(tidyverse)
library(plyr)
library(DataExplorer)
library(dplyr)
library(mice)
library(zipcodeR)
library(randomForest)
library(MLmetrics)
library(xgboost)
library(gbm)
library(Matrix)
library(caret)

### Read in data
data <- read.csv("XPEL_data.csv")
### Set the seed
set.seed(123)

##### Functions #####
### Ensure these functions are still usable even with the 'plyr' package loaded
select <- dplyr::select
arrange <- dplyr::arrange
count <- dplyr::count
desc <- dplyr::desc
rename <- dplyr::rename

##### Add the test.train column #####
data$test.train <- 0
### 1 = test, 0 = train

### Determine rows for train/test split by the presence of a FedEx shipping cost
data$test.train <- ifelse(is.na(data$Actual.Shipping.cost..FedEx.), 1, 0)


##### Make Weight Columns Numeric (CHANGE COLUMN NAME IF YOURS IS DIFFERENT) #####
data$Actual.Shipping.Weight..FedEx. <- as.numeric(data$Actual.Shipping.Weight..FedEx.)
data$Actual.Shipping.cost..FedEx. <- as.numeric(data$Actual.Shipping.cost..FedEx.)

### Dropping Shipping.Carrier because all values are the same
data <- data %>% select(-Shipping.Carrier)

### Fix column names
#for(column in colnames(data)){
#  colnames(data)[which(names(data) == column)] <- 
#    make.names(colnames(data)[which(names(data) == column)])
#}

##### Rename Columns #####
names(data)[names(data) == "Internal.ID"] <- "ID"
names(data)[names(data) == "Created.From"] <- "Sales.Order.Reference"
names(data)[names(data) == "Document.Number"] <- "Item.Fulfillment.Reference"
names(data)[names(data) == "Tracking.Numbers"] <- "Tracking.Number"
names(data)[names(data) == "Shipped.From.Location"] <- "Origin.Location"
names(data)[names(data) == "Shipped.to.Zip.Code"] <- "Destination.Zip"
names(data)[names(data) == "Ship.Via"] <- "Shipping.Mode"
names(data)[names(data) == "Stock.Box.Item.Height"] <- "Package.Height"
names(data)[names(data) == "Stock.Box.Item.Length"] <- "Package.Length"
names(data)[names(data) == "Stock.Box.Item.Width"] <- "Package.Width"
names(data)[names(data) == "Gross.Weight.of.Item.Fulfillment"] <- "Package.Weight"
names(data)[names(data) == "Actual.Shipping.cost..FedEx."] <- "FedEx.Shipping.Rate"
names(data)[names(data) == "Actual.Shipping.Weight..FedEx."] <- "FedEx.Package.Weight"

### Make XPEL.Shipping.Rate a numeric column
data$XPEL.Shipping.Rate <- as.numeric(data$XPEL.Shipping.Rate)

##### Delete Package.Weight #####
data <- data %>% select(-Package.Weight)

##### Make Tracking.Number Character #####
data$Tracking.Number <- as.character(data$Tracking.Number)

##### Air.Zone Column #####
data <- add_column(data, Air.Zone = data$Destination.Zip, .after = "Origin.Location")
data$Air.Zone <- substr(data$Air.Zone, 1, 3)
data$Air.Zone <- as.numeric(data$Air.Zone)

##### Ground.Zone Column #####
data <- add_column(data, Ground.Zone = data$Destination.Zip, .after = "Origin.Location")
data$Ground.Zone <- substr(data$Ground.Zone, 1, 3)
data$Ground.Zone <- as.numeric(data$Ground.Zone)
##### Zone 0 #####
### Air Zone 0 (NA zones)
data$Air.Zone <- ifelse((data$Air.Zone == 000 | data$Air.Zone == 001 | data$Air.Zone == 002 | 
                           data$Air.Zone == 003 | data$Air.Zone == 004 | data$Air.Zone == 213 |
                           data$Air.Zone == 269 | data$Air.Zone == 343 | data$Air.Zone == 345 |
                           data$Air.Zone == 348 | data$Air.Zone == 353 | data$Air.Zone == 419 |
                           data$Air.Zone == 428 | data$Air.Zone == 429 | data$Air.Zone == 517 |
                           data$Air.Zone == 518 | data$Air.Zone == 519 | data$Air.Zone == 529 |
                           data$Air.Zone == 533 | data$Air.Zone == 536 | data$Air.Zone == 552 |
                           data$Air.Zone == 568 | data$Air.Zone == 569 | data$Air.Zone == 578 |
                           data$Air.Zone == 579 | data$Air.Zone == 589 | data$Air.Zone == 621 |
                           data$Air.Zone == 632 | data$Air.Zone == 642 | data$Air.Zone == 643 |
                           data$Air.Zone == 659 | data$Air.Zone == 663 | data$Air.Zone == 682 |
                           data$Air.Zone == 694 | data$Air.Zone == 695 | data$Air.Zone == 696 |
                           data$Air.Zone == 697 | data$Air.Zone == 698 | data$Air.Zone == 699 |
                           data$Air.Zone == 702 | data$Air.Zone == 709 | data$Air.Zone == 715 |
                           data$Air.Zone == 732 | data$Air.Zone == 742 | data$Air.Zone == 817 |
                           data$Air.Zone == 818 | data$Air.Zone == 819 | data$Air.Zone == 839 |
                           data$Air.Zone == 848 | data$Air.Zone == 849 | data$Air.Zone == 854 |
                           data$Air.Zone == 858 | data$Air.Zone == 861 | data$Air.Zone == 862 |
                           data$Air.Zone == 866 | data$Air.Zone == 867 | data$Air.Zone == 868 |
                           data$Air.Zone == 869 | data$Air.Zone == 876 | data$Air.Zone == 886 |
                           data$Air.Zone == 887 | data$Air.Zone == 888 | data$Air.Zone == 892 |
                           data$Air.Zone == 896 | data$Air.Zone == 899 | data$Air.Zone == 909 |
                           data$Air.Zone == 929 | data$Air.Zone == 987 | data$Air.Zone == 006 |
                           data$Air.Zone == 007 | data$Air.Zone == 008 | data$Air.Zone == 009 |
                           data$Air.Zone == 969), 0, data$Air.Zone)

### Ground Zone 0 (NA zones)
data$Ground.Zone <- ifelse((data$Ground.Zone == 000 | data$Ground.Zone == 001 | data$Ground.Zone == 002 | 
                              data$Ground.Zone == 003 | data$Ground.Zone == 004 | data$Ground.Zone == 213 |
                              data$Ground.Zone == 269 | data$Ground.Zone == 343 | data$Ground.Zone == 345 |
                              data$Ground.Zone == 348 | data$Ground.Zone == 353 | data$Ground.Zone == 419 |
                              data$Ground.Zone == 428 | data$Ground.Zone == 429 | data$Ground.Zone == 517 |
                              data$Ground.Zone == 518 | data$Ground.Zone == 519 | data$Ground.Zone == 529 |
                              data$Ground.Zone == 533 | data$Ground.Zone == 536 | data$Ground.Zone == 552 |
                              data$Ground.Zone == 568 | data$Ground.Zone == 569 | data$Ground.Zone == 578 |
                              data$Ground.Zone == 579 | data$Ground.Zone == 589 | data$Ground.Zone == 621 |
                              data$Ground.Zone == 632 | data$Ground.Zone == 642 | data$Ground.Zone == 643 |
                              data$Ground.Zone == 659 | data$Ground.Zone == 663 | data$Ground.Zone == 682 |
                              data$Ground.Zone == 694 | data$Ground.Zone == 695 | data$Ground.Zone == 696 |
                              data$Ground.Zone == 697 | data$Ground.Zone == 698 | data$Ground.Zone == 699 |
                              data$Ground.Zone == 702 | data$Ground.Zone == 709 | data$Ground.Zone == 715 |
                              data$Ground.Zone == 732 | data$Ground.Zone == 742 | data$Ground.Zone == 817 |
                              data$Ground.Zone == 818 | data$Ground.Zone == 819 | data$Ground.Zone == 839 |
                              data$Ground.Zone == 848 | data$Ground.Zone == 849 | data$Ground.Zone == 854 |
                              data$Ground.Zone == 858 | data$Ground.Zone == 861 | data$Ground.Zone == 862 |
                              data$Ground.Zone == 866 | data$Ground.Zone == 867 | data$Ground.Zone == 868 |
                              data$Ground.Zone == 869 | data$Ground.Zone == 876 | data$Ground.Zone == 886 |
                              data$Ground.Zone == 887 | data$Ground.Zone == 888 | data$Ground.Zone == 892 |
                              data$Ground.Zone == 896 | data$Ground.Zone == 899 | data$Ground.Zone == 909 |
                              data$Ground.Zone == 929 | data$Ground.Zone == 987 | data$Ground.Zone == 006 | 
                              data$Ground.Zone == 007 | data$Ground.Zone == 008 | data$Ground.Zone == 009 | 
                              data$Ground.Zone == 969), 0, data$Ground.Zone)


##### MICE Imputation #####
### Plot Missing Data before MICE
#plot_missing(data)

### Run the MICE function
tempData <- mice(data, m = 5, maxit = 6, method = 'cart', seed = 500, quickpred(data))
#summary(tempData)
data <- complete(tempData,1)

### Plot Missing Data after MICE
#plot_missing(data)

### Set Test to NA for Fedex Shipping rate
data$FedEx.Shipping.Rate <- ifelse(data$test.train == 1, NA, data$FedEx.Shipping.Rate)


##### Create Origin.Zip Column #####

data$Origin.Location <- factor(data$Origin.Location, levels = c("Fullerton","San Antonio","Dallas-Lemon","Boise"))

data <- add_column(data, Origin.Zip = data$Origin.Location, .after = "Origin.Location")
data$Origin.Zip <- ifelse(data$Origin.Location =="San Antonio", 78219, data$Origin.Zip)
data$Origin.Zip <- ifelse(data$Origin.Location =="Fullerton", 92831, data$Origin.Zip)
data$Origin.Zip <- ifelse(data$Origin.Location =="Dallas-Lemon", 75209, data$Origin.Zip)
data$Origin.Zip <- ifelse(data$Origin.Location =="Boise", 83709, data$Origin.Zip)
data$Origin.Zip <- factor(data$Origin.Zip, levels = c(78219, 92831, 75209, 83709))

##### Create Destination.City and Destination.State #####

### Change problem zip codes
data$Destination.Zip <- ifelse(data$Destination.Zip == '99520', '99508', data$Destination.Zip)
### Changed Anchorage AK 99520 to 99508
data$Destination.Zip <- ifelse(data$Destination.Zip == '92878', '92882', data$Destination.Zip)
### Changed Corona CA 92878 to 92882
data$Destination.Zip <- ifelse(data$Destination.Zip == '77225', '77025', data$Destination.Zip)
### Changed Houston TX 77225 to 77025
data$Destination.Zip <- ifelse(data$Destination.Zip == '83415', '83404', data$Destination.Zip)
### Changed Idaho Falls ID 83415 to 83404
data$Destination.Zip <- ifelse(data$Destination.Zip == '33806', '33805', data$Destination.Zip)
### Changed Lakeland FL 33806 to 33805
data$Destination.Zip <- ifelse(data$Destination.Zip == '75606', '75602', data$Destination.Zip)
### Changed Longview TX 75606 to 75602
data$Destination.Zip <- ifelse(data$Destination.Zip == '33280', '33180', data$Destination.Zip)
### Changed Miami FL 33280 to 33180
data$Destination.Zip <- ifelse(data$Destination.Zip == '27374', '27295', data$Destination.Zip)
### Changed Welcome NC 27374 to 27295

data$Destination.Zip <- substr(data$Destination.Zip , start = 1, stop = 5)

### City
#data['major_city'] <- NA
#for(n in 1:nrow(data)) {
#  data[n,names(data)=="major_city"] <- 
#    reverse_zipcode(data[n,names(data)=="Destination.Zip"])$major_city
#}

### State
#data['State'] <- NA
#for(n in 1:nrow(data)) {
#  data[n,names(data)=="State"] <- 
#    reverse_zipcode(data[n,names(data)=="Destination.Zip"])$state
#}

### Create a CSV of the columns to save time by not running loop again
#MajorCity <- data %>%
#  select(Tracking.Number, major_city)
#write.csv(MajorCity, "MajorCity.csv", row.names = FALSE)

#State <- data %>%
#  select(Tracking.Number, State)
#write.csv(State, "State.csv", row.names = FALSE)

### Merge the existing CSV city and state files into the data set
major_city <- read.csv("MajorCity.csv")
state. <- read.csv("State.csv")

maj_city <- major_city$major_city
state <- state.$State

data$Destination.City <- maj_city
data$Destination.State <- state

### Move City and State columns to a different place in the data set
data <- data %>% relocate(Destination.City, .before = Destination.Zip)
data <- data %>% relocate(Destination.State, .before = Destination.Zip)

###### Create Destination.Latitude and Destination.Longitude #####
lat_lon <- data.frame(lapply(data["Destination.Zip"], geocode_zip))
data <- left_join(data, lat_lon, by = c("Destination.Zip" = "Destination.Zip.zipcode"))
data <- data %>% relocate(Destination.Zip.lat:Destination.Zip.lng, .before = Shipping.Mode)

##### Rename and rearrange the 4 new columns in the data set #####
names(data)[names(data) == "Destination.Zip.lat"] <- "Destination.Latitude"
names(data)[names(data) == "Destination.Zip.lng"] <- "Destination.Longitude"
#names(data)[names(data) == "major_city"] <- "Destination.City"
#names(data)[names(data) == "State"] <- "Destination.State"

#data <- data %>% relocate(Destination.City, .before = Destination.Latitude)
#data <- data %>% relocate(Destination.State, .before = Destination.Latitude)


##### Zone 2 #####
### Air Zone 2
data$Air.Zone <- ifelse((data$Air.Zone == 733 | data$Air.Zone == 765 | data$Air.Zone == 778 | 
                           data$Air.Zone == 779 | data$Air.Zone == 780 | data$Air.Zone == 781 |
                           data$Air.Zone == 782 | data$Air.Zone == 783 | data$Air.Zone == 784 |
                           data$Air.Zone == 786 | data$Air.Zone == 787 | data$Air.Zone == 788 |
                           data$Air.Zone == 789), 2, data$Air.Zone)
### Ground Zone 2
data$Ground.Zone <- ifelse((data$Ground.Zone == 733 | data$Ground.Zone == 765 | data$Ground.Zone == 778 | 
                              data$Ground.Zone == 779 | data$Ground.Zone == 780 | data$Ground.Zone == 781 |
                              data$Ground.Zone == 782 | data$Ground.Zone == 783 | data$Ground.Zone == 784 |
                              data$Ground.Zone == 786 | data$Ground.Zone == 787 | data$Ground.Zone == 788 |
                              data$Ground.Zone == 789), 2, data$Ground.Zone)

##### Zone 3 #####
### Air Zone 3
data$Air.Zone <- ifelse((data$Air.Zone == 706 | data$Air.Zone == 750 | data$Air.Zone == 751 | 
                           data$Air.Zone == 752 | data$Air.Zone == 753 | data$Air.Zone == 754 |
                           data$Air.Zone == 756 | data$Air.Zone == 757 | data$Air.Zone == 758 |
                           data$Air.Zone == 759 | data$Air.Zone == 760 | data$Air.Zone == 761 |
                           data$Air.Zone == 762 | data$Air.Zone == 763 | data$Air.Zone == 764 |
                           data$Air.Zone == 766 | data$Air.Zone == 767 | data$Air.Zone == 768 |
                           data$Air.Zone == 769 | data$Air.Zone == 770 | data$Air.Zone == 771 |
                           data$Air.Zone == 772 | data$Air.Zone == 773 | data$Air.Zone == 774 |
                           data$Air.Zone == 775 | data$Air.Zone == 776 | data$Air.Zone == 777 |
                           data$Air.Zone == 785 | data$Air.Zone == 795 | data$Air.Zone == 796 |
                           data$Air.Zone == 797), 3, data$Air.Zone)
### Ground Zone 3
data$Ground.Zone <- ifelse((data$Ground.Zone == 706 | data$Ground.Zone == 750 | data$Ground.Zone == 751 | 
                              data$Ground.Zone == 752 | data$Ground.Zone == 753 | data$Ground.Zone == 754 |
                              data$Ground.Zone == 756 | data$Ground.Zone == 757 | data$Ground.Zone == 758 |
                              data$Ground.Zone == 759 | data$Ground.Zone == 760 | data$Ground.Zone == 761 |
                              data$Ground.Zone == 762 | data$Ground.Zone == 763 | data$Ground.Zone == 764 |
                              data$Ground.Zone == 766 | data$Ground.Zone == 767 | data$Ground.Zone == 768 |
                              data$Ground.Zone == 769 | data$Ground.Zone == 770 | data$Ground.Zone == 771 |
                              data$Ground.Zone == 772 | data$Ground.Zone == 773 | data$Ground.Zone == 774 |
                              data$Ground.Zone == 775 | data$Ground.Zone == 776 | data$Ground.Zone == 777 |
                              data$Ground.Zone == 785 | data$Ground.Zone == 795 | data$Ground.Zone == 796 |
                              data$Ground.Zone == 797), 3, data$Ground.Zone)

##### Zone 4 #####
### Air Zone 4
data$Air.Zone <- ifelse((data$Air.Zone == 365 | data$Air.Zone == 366 | data$Air.Zone == 369 | 
                           data$Air.Zone == 387 | data$Air.Zone == 389 | data$Air.Zone == 390 |
                           data$Air.Zone == 391 | data$Air.Zone == 392 | data$Air.Zone == 393 |
                           data$Air.Zone == 394 | data$Air.Zone == 395 | data$Air.Zone == 396 |
                           data$Air.Zone == 648 | data$Air.Zone == 656 | data$Air.Zone == 657 |
                           data$Air.Zone == 658 | data$Air.Zone == 667 | data$Air.Zone == 670 |
                           data$Air.Zone == 671 | data$Air.Zone == 672 | data$Air.Zone == 673 |
                           data$Air.Zone == 678 | data$Air.Zone == 679 | data$Air.Zone == 700 |
                           data$Air.Zone == 701 | data$Air.Zone == 703 | data$Air.Zone == 704 |
                           data$Air.Zone == 705 | data$Air.Zone == 707 | data$Air.Zone == 708 |
                           data$Air.Zone == 710 | data$Air.Zone == 711 | data$Air.Zone == 712 |
                           data$Air.Zone == 713 | data$Air.Zone == 714 | data$Air.Zone == 716 |
                           data$Air.Zone == 717 | data$Air.Zone == 718 | data$Air.Zone == 719 |
                           data$Air.Zone == 720 | data$Air.Zone == 721 | data$Air.Zone == 722 |
                           data$Air.Zone == 725 | data$Air.Zone == 726 | data$Air.Zone == 727 |
                           data$Air.Zone == 728 | data$Air.Zone == 729 | data$Air.Zone == 730 |
                           data$Air.Zone == 731 | data$Air.Zone == 734 | data$Air.Zone == 735 |
                           data$Air.Zone == 736 | data$Air.Zone == 737 | data$Air.Zone == 738 |
                           data$Air.Zone == 739 | data$Air.Zone == 740 | data$Air.Zone == 741 |
                           data$Air.Zone == 743 | data$Air.Zone == 744 | data$Air.Zone == 745 |
                           data$Air.Zone == 746 | data$Air.Zone == 747 | data$Air.Zone == 748 |
                           data$Air.Zone == 749 | data$Air.Zone == 755 | data$Air.Zone == 790 |
                           data$Air.Zone == 791 | data$Air.Zone == 792 | data$Air.Zone == 793 |
                           data$Air.Zone == 794 | data$Air.Zone == 798 | data$Air.Zone == 799 |
                           data$Air.Zone == 877 | data$Air.Zone == 878 | data$Air.Zone == 879 |
                           data$Air.Zone == 880 | data$Air.Zone == 881 | data$Air.Zone == 882 |
                           data$Air.Zone == 883 | data$Air.Zone == 884 | data$Air.Zone == 885), 4, data$Air.Zone)
### Ground Zone 4
data$Ground.Zone <- ifelse((data$Ground.Zone == 365 | data$Ground.Zone == 366 | data$Ground.Zone == 369 | 
                              data$Ground.Zone == 387 | data$Ground.Zone == 389 | data$Ground.Zone == 390 |
                              data$Ground.Zone == 391 | data$Ground.Zone == 392 | data$Ground.Zone == 393 |
                              data$Ground.Zone == 394 | data$Ground.Zone == 395 | data$Ground.Zone == 396 |
                              data$Ground.Zone == 648 | data$Ground.Zone == 656 | data$Ground.Zone == 657 |
                              data$Ground.Zone == 658 | data$Ground.Zone == 667 | data$Ground.Zone == 670 |
                              data$Ground.Zone == 671 | data$Ground.Zone == 672 | data$Ground.Zone == 673 |
                              data$Ground.Zone == 678 | data$Ground.Zone == 679 | data$Ground.Zone == 700 |
                              data$Ground.Zone == 701 | data$Ground.Zone == 703 | data$Ground.Zone == 704 |
                              data$Ground.Zone == 705 | data$Ground.Zone == 707 | data$Ground.Zone == 708 |
                              data$Ground.Zone == 710 | data$Ground.Zone == 711 | data$Ground.Zone == 712 |
                              data$Ground.Zone == 713 | data$Ground.Zone == 714 | data$Ground.Zone == 716 |
                              data$Ground.Zone == 717 | data$Ground.Zone == 718 | data$Ground.Zone == 719 |
                              data$Ground.Zone == 720 | data$Ground.Zone == 721 | data$Ground.Zone == 722 |
                              data$Ground.Zone == 725 | data$Ground.Zone == 726 | data$Ground.Zone == 727 |
                              data$Ground.Zone == 728 | data$Ground.Zone == 729 | data$Ground.Zone == 730 |
                              data$Ground.Zone == 731 | data$Ground.Zone == 734 | data$Ground.Zone == 735 |
                              data$Ground.Zone == 736 | data$Ground.Zone == 737 | data$Ground.Zone == 738 |
                              data$Ground.Zone == 739 | data$Ground.Zone == 740 | data$Ground.Zone == 741 |
                              data$Ground.Zone == 743 | data$Ground.Zone == 744 | data$Ground.Zone == 745 |
                              data$Ground.Zone == 746 | data$Ground.Zone == 747 | data$Ground.Zone == 748 |
                              data$Ground.Zone == 749 | data$Ground.Zone == 755 | data$Ground.Zone == 790 |
                              data$Ground.Zone == 791 | data$Ground.Zone == 792 | data$Ground.Zone == 793 |
                              data$Ground.Zone == 794 | data$Ground.Zone == 798 | data$Ground.Zone == 799 |
                              data$Ground.Zone == 877 | data$Ground.Zone == 878 | data$Ground.Zone == 879 |
                              data$Ground.Zone == 880 | data$Ground.Zone == 881 | data$Ground.Zone == 882 |
                              data$Ground.Zone == 883 | data$Ground.Zone == 884 | data$Ground.Zone == 885), 4, data$Ground.Zone)

##### Zone 5 #####
### Air Zone 5
data$Air.Zone <- ifelse((data$Air.Zone == 287 | data$Air.Zone == 288 | data$Air.Zone == 289 | 
                           data$Air.Zone == 296 | data$Air.Zone == 298 | data$Air.Zone == 300 |
                           data$Air.Zone == 301 | data$Air.Zone == 302 | data$Air.Zone == 303 |
                           data$Air.Zone == 304 | data$Air.Zone == 305 | data$Air.Zone == 306 |
                           data$Air.Zone == 307 | data$Air.Zone == 308 | data$Air.Zone == 309 | 
                           data$Air.Zone == 310 | data$Air.Zone == 311 | data$Air.Zone == 312 | 
                           data$Air.Zone == 315 | data$Air.Zone == 316 | data$Air.Zone == 317 |
                           data$Air.Zone == 318 | data$Air.Zone == 319 | data$Air.Zone == 320 |
                           data$Air.Zone == 321 | data$Air.Zone == 322 | data$Air.Zone == 323 |
                           data$Air.Zone == 324 | data$Air.Zone == 325 | data$Air.Zone == 326 |
                           data$Air.Zone == 335 | data$Air.Zone == 336 | data$Air.Zone == 337 |
                           data$Air.Zone == 338 | data$Air.Zone == 342 | data$Air.Zone == 344 |
                           data$Air.Zone == 346 | data$Air.Zone == 350 | data$Air.Zone == 351 |
                           data$Air.Zone == 352 | data$Air.Zone == 354 | data$Air.Zone == 355 |
                           data$Air.Zone == 356 | data$Air.Zone == 357 | data$Air.Zone == 358 | 
                           data$Air.Zone == 359 | data$Air.Zone == 360 | data$Air.Zone == 361 | 
                           data$Air.Zone == 362 | data$Air.Zone == 363 | data$Air.Zone == 364 |
                           data$Air.Zone == 365 | data$Air.Zone == 366 | data$Air.Zone == 367 |
                           data$Air.Zone == 368 | data$Air.Zone == 370 | data$Air.Zone == 371 |
                           data$Air.Zone == 372 | data$Air.Zone == 373 | data$Air.Zone == 374 |
                           data$Air.Zone == 375 | data$Air.Zone == 377 | data$Air.Zone == 378 |
                           data$Air.Zone == 379 | data$Air.Zone == 380 | data$Air.Zone == 381 | 
                           data$Air.Zone == 382 | data$Air.Zone == 383 | data$Air.Zone == 384 | 
                           data$Air.Zone == 385 | data$Air.Zone == 386 | data$Air.Zone == 388 |
                           data$Air.Zone == 397 | data$Air.Zone == 398 | data$Air.Zone == 399 |
                           data$Air.Zone == 400 | data$Air.Zone == 401 | data$Air.Zone == 402 |
                           data$Air.Zone == 403 | data$Air.Zone == 404 | data$Air.Zone == 405 |
                           data$Air.Zone == 406 | data$Air.Zone == 407 | data$Air.Zone == 408 |
                           data$Air.Zone == 409 | data$Air.Zone == 413 | data$Air.Zone == 414 |
                           data$Air.Zone == 420 | data$Air.Zone == 421 | data$Air.Zone == 422 |
                           data$Air.Zone == 423 | data$Air.Zone == 424 | data$Air.Zone == 425 |
                           data$Air.Zone == 426 | data$Air.Zone == 427 | data$Air.Zone == 460 |
                           data$Air.Zone == 461 | data$Air.Zone == 462 | data$Air.Zone == 469 |
                           data$Air.Zone == 471 | data$Air.Zone == 472 | data$Air.Zone == 474 |
                           data$Air.Zone == 475 | data$Air.Zone == 476 | data$Air.Zone == 477 |
                           data$Air.Zone == 478 | data$Air.Zone == 479 | data$Air.Zone == 500 |
                           data$Air.Zone == 501 | data$Air.Zone == 502 | data$Air.Zone == 503 |
                           data$Air.Zone == 504 | data$Air.Zone == 505 | data$Air.Zone == 506 |
                           data$Air.Zone == 507 | data$Air.Zone == 508 | data$Air.Zone == 509 |
                           data$Air.Zone == 510 | data$Air.Zone == 511 | data$Air.Zone == 512 |
                           data$Air.Zone == 513 | data$Air.Zone == 514 | data$Air.Zone == 515 |
                           data$Air.Zone == 516 | data$Air.Zone == 520 | data$Air.Zone == 522 |
                           data$Air.Zone == 523 | data$Air.Zone == 524 | data$Air.Zone == 525 |
                           data$Air.Zone == 526 | data$Air.Zone == 527 | data$Air.Zone == 528 |
                           data$Air.Zone == 538 | data$Air.Zone == 561 | data$Air.Zone == 570 |
                           data$Air.Zone == 571 | data$Air.Zone == 573 | data$Air.Zone == 609 |
                           data$Air.Zone == 612 | data$Air.Zone == 613 | data$Air.Zone == 614 |
                           data$Air.Zone == 615 | data$Air.Zone == 616 | data$Air.Zone == 617 |
                           data$Air.Zone == 618 | data$Air.Zone == 619 | data$Air.Zone == 620 |
                           data$Air.Zone == 622 | data$Air.Zone == 623 | data$Air.Zone == 624 |
                           data$Air.Zone == 625 | data$Air.Zone == 626 | data$Air.Zone == 627 |
                           data$Air.Zone == 628 | data$Air.Zone == 629 | data$Air.Zone == 630 |
                           data$Air.Zone == 631 | data$Air.Zone == 633 | data$Air.Zone == 634 |
                           data$Air.Zone == 635 | data$Air.Zone == 636 | data$Air.Zone == 637 |
                           data$Air.Zone == 638 | data$Air.Zone == 639 | data$Air.Zone == 640 |
                           data$Air.Zone == 641 | data$Air.Zone == 644 | data$Air.Zone == 645 |
                           data$Air.Zone == 646 | data$Air.Zone == 647 | data$Air.Zone == 649 |
                           data$Air.Zone == 650 | data$Air.Zone == 651 | data$Air.Zone == 652 |
                           data$Air.Zone == 653 | data$Air.Zone == 654 | data$Air.Zone == 655 |
                           data$Air.Zone == 660 | data$Air.Zone == 661 | data$Air.Zone == 662 |
                           data$Air.Zone == 664 | data$Air.Zone == 665 | data$Air.Zone == 666 |
                           data$Air.Zone == 668 | data$Air.Zone == 669 | data$Air.Zone == 674 |
                           data$Air.Zone == 675 | data$Air.Zone == 676 | data$Air.Zone == 677 |
                           data$Air.Zone == 680 | data$Air.Zone == 681 | data$Air.Zone == 683 |
                           data$Air.Zone == 684 | data$Air.Zone == 685 | data$Air.Zone == 686 |
                           data$Air.Zone == 687 | data$Air.Zone == 688 | data$Air.Zone == 689 |
                           data$Air.Zone == 690 | data$Air.Zone == 691 | data$Air.Zone == 692 |
                           data$Air.Zone == 693 | data$Air.Zone == 723 | data$Air.Zone == 724 |
                           data$Air.Zone == 800 | data$Air.Zone == 801 | data$Air.Zone == 802 |
                           data$Air.Zone == 803 | data$Air.Zone == 804 | data$Air.Zone == 805 |
                           data$Air.Zone == 806 | data$Air.Zone == 807 | data$Air.Zone == 808 |
                           data$Air.Zone == 809 | data$Air.Zone == 810 | data$Air.Zone == 811 |
                           data$Air.Zone == 812 | data$Air.Zone == 813 | data$Air.Zone == 814 |
                           data$Air.Zone == 815 | data$Air.Zone == 816 | data$Air.Zone == 820 |
                           data$Air.Zone == 822 | data$Air.Zone == 823 | data$Air.Zone == 845 |
                           data$Air.Zone == 850 | data$Air.Zone == 851 | data$Air.Zone == 852 |
                           data$Air.Zone == 853 | data$Air.Zone == 855 | data$Air.Zone == 856 |
                           data$Air.Zone == 857 | data$Air.Zone == 859 | data$Air.Zone == 860 |
                           data$Air.Zone == 863 | data$Air.Zone == 865 | data$Air.Zone == 870 |
                           data$Air.Zone == 871 | data$Air.Zone == 872 | data$Air.Zone == 873 |
                           data$Air.Zone == 874 | data$Air.Zone == 875), 5, data$Air.Zone)

### Ground Zone 5
data$Ground.Zone <- ifelse((data$Ground.Zone == 287 | data$Ground.Zone == 288 | data$Ground.Zone == 289 | 
                              data$Ground.Zone == 296 | data$Ground.Zone == 298 | data$Ground.Zone == 300 |
                              data$Ground.Zone == 301 | data$Ground.Zone == 302 | data$Ground.Zone == 303 |
                              data$Ground.Zone == 304 | data$Ground.Zone == 305 | data$Ground.Zone == 306 |
                              data$Ground.Zone == 307 | data$Ground.Zone == 308 | data$Ground.Zone == 309 | 
                              data$Ground.Zone == 310 | data$Ground.Zone == 311 | data$Ground.Zone == 312 | 
                              data$Ground.Zone == 315 | data$Ground.Zone == 316 | data$Ground.Zone == 317 |
                              data$Ground.Zone == 318 | data$Ground.Zone == 319 | data$Ground.Zone == 320 |
                              data$Ground.Zone == 321 | data$Ground.Zone == 322 | data$Ground.Zone == 323 |
                              data$Ground.Zone == 324 | data$Ground.Zone == 325 | data$Ground.Zone == 326 |
                              data$Ground.Zone == 335 | data$Ground.Zone == 336 | data$Ground.Zone == 337 |
                              data$Ground.Zone == 338 | data$Ground.Zone == 342 | data$Ground.Zone == 344 |
                              data$Ground.Zone == 346 | data$Ground.Zone == 350 | data$Ground.Zone == 351 |
                              data$Ground.Zone == 352 | data$Ground.Zone == 354 | data$Ground.Zone == 355 |
                              data$Ground.Zone == 356 | data$Ground.Zone == 357 | data$Ground.Zone == 358 | 
                              data$Ground.Zone == 359 | data$Ground.Zone == 360 | data$Ground.Zone == 361 | 
                              data$Ground.Zone == 362 | data$Ground.Zone == 363 | data$Ground.Zone == 364 |
                              data$Ground.Zone == 365 | data$Ground.Zone == 366 | data$Ground.Zone == 367 |
                              data$Ground.Zone == 368 | data$Ground.Zone == 370 | data$Ground.Zone == 371 |
                              data$Ground.Zone == 372 | data$Ground.Zone == 373 | data$Ground.Zone == 374 |
                              data$Ground.Zone == 375 | data$Ground.Zone == 377 | data$Ground.Zone == 378 |
                              data$Ground.Zone == 379 | data$Ground.Zone == 380 | data$Ground.Zone == 381 | 
                              data$Ground.Zone == 382 | data$Ground.Zone == 383 | data$Ground.Zone == 384 | 
                              data$Ground.Zone == 385 | data$Ground.Zone == 386 | data$Ground.Zone == 388 |
                              data$Ground.Zone == 397 | data$Ground.Zone == 398 | data$Ground.Zone == 399 |
                              data$Ground.Zone == 400 | data$Ground.Zone == 401 | data$Ground.Zone == 402 |
                              data$Ground.Zone == 403 | data$Ground.Zone == 404 | data$Ground.Zone == 405 |
                              data$Ground.Zone == 406 | data$Ground.Zone == 407 | data$Ground.Zone == 408 |
                              data$Ground.Zone == 409 | data$Ground.Zone == 413 | data$Ground.Zone == 414 |
                              data$Ground.Zone == 420 | data$Ground.Zone == 421 | data$Ground.Zone == 422 |
                              data$Ground.Zone == 423 | data$Ground.Zone == 424 | data$Ground.Zone == 425 |
                              data$Ground.Zone == 426 | data$Ground.Zone == 427 | data$Ground.Zone == 460 |
                              data$Ground.Zone == 461 | data$Ground.Zone == 462 | data$Ground.Zone == 469 |
                              data$Ground.Zone == 471 | data$Ground.Zone == 472 | data$Ground.Zone == 474 |
                              data$Ground.Zone == 475 | data$Ground.Zone == 476 | data$Ground.Zone == 477 |
                              data$Ground.Zone == 478 | data$Ground.Zone == 479 | data$Ground.Zone == 500 |
                              data$Ground.Zone == 501 | data$Ground.Zone == 502 | data$Ground.Zone == 503 |
                              data$Ground.Zone == 504 | data$Ground.Zone == 505 | data$Ground.Zone == 506 |
                              data$Ground.Zone == 507 | data$Ground.Zone == 508 | data$Ground.Zone == 509 |
                              data$Ground.Zone == 510 | data$Ground.Zone == 511 | data$Ground.Zone == 512 |
                              data$Ground.Zone == 513 | data$Ground.Zone == 514 | data$Ground.Zone == 515 |
                              data$Ground.Zone == 516 | data$Ground.Zone == 520 | data$Ground.Zone == 522 |
                              data$Ground.Zone == 523 | data$Ground.Zone == 524 | data$Ground.Zone == 525 |
                              data$Ground.Zone == 526 | data$Ground.Zone == 527 | data$Ground.Zone == 528 |
                              data$Ground.Zone == 538 | data$Ground.Zone == 561 | data$Ground.Zone == 570 |
                              data$Ground.Zone == 571 | data$Ground.Zone == 573 | data$Ground.Zone == 609 |
                              data$Ground.Zone == 612 | data$Ground.Zone == 613 | data$Ground.Zone == 614 |
                              data$Ground.Zone == 615 | data$Ground.Zone == 616 | data$Ground.Zone == 617 |
                              data$Ground.Zone == 618 | data$Ground.Zone == 619 | data$Ground.Zone == 620 |
                              data$Ground.Zone == 622 | data$Ground.Zone == 623 | data$Ground.Zone == 624 |
                              data$Ground.Zone == 625 | data$Ground.Zone == 626 | data$Ground.Zone == 627 |
                              data$Ground.Zone == 628 | data$Ground.Zone == 629 | data$Ground.Zone == 630 |
                              data$Ground.Zone == 631 | data$Ground.Zone == 633 | data$Ground.Zone == 634 |
                              data$Ground.Zone == 635 | data$Ground.Zone == 636 | data$Ground.Zone == 637 |
                              data$Ground.Zone == 638 | data$Ground.Zone == 639 | data$Ground.Zone == 640 |
                              data$Ground.Zone == 641 | data$Ground.Zone == 644 | data$Ground.Zone == 645 |
                              data$Ground.Zone == 646 | data$Ground.Zone == 647 | data$Ground.Zone == 649 |
                              data$Ground.Zone == 650 | data$Ground.Zone == 651 | data$Ground.Zone == 652 |
                              data$Ground.Zone == 653 | data$Ground.Zone == 654 | data$Ground.Zone == 655 |
                              data$Ground.Zone == 660 | data$Ground.Zone == 661 | data$Ground.Zone == 662 |
                              data$Ground.Zone == 664 | data$Ground.Zone == 665 | data$Ground.Zone == 666 |
                              data$Ground.Zone == 668 | data$Ground.Zone == 669 | data$Ground.Zone == 674 |
                              data$Ground.Zone == 675 | data$Ground.Zone == 676 | data$Ground.Zone == 677 |
                              data$Ground.Zone == 680 | data$Ground.Zone == 681 | data$Ground.Zone == 683 |
                              data$Ground.Zone == 684 | data$Ground.Zone == 685 | data$Ground.Zone == 686 |
                              data$Ground.Zone == 687 | data$Ground.Zone == 688 | data$Ground.Zone == 689 |
                              data$Ground.Zone == 690 | data$Ground.Zone == 691 | data$Ground.Zone == 692 |
                              data$Ground.Zone == 693 | data$Ground.Zone == 723 | data$Ground.Zone == 724 |
                              data$Ground.Zone == 800 | data$Ground.Zone == 801 | data$Ground.Zone == 802 |
                              data$Ground.Zone == 803 | data$Ground.Zone == 804 | data$Ground.Zone == 805 |
                              data$Ground.Zone == 806 | data$Ground.Zone == 807 | data$Ground.Zone == 808 |
                              data$Ground.Zone == 809 | data$Ground.Zone == 810 | data$Ground.Zone == 811 |
                              data$Ground.Zone == 812 | data$Ground.Zone == 813 | data$Ground.Zone == 814 |
                              data$Ground.Zone == 815 | data$Ground.Zone == 816 | data$Ground.Zone == 820 |
                              data$Ground.Zone == 822 | data$Ground.Zone == 823 | data$Ground.Zone == 845 |
                              data$Ground.Zone == 850 | data$Ground.Zone == 851 | data$Ground.Zone == 852 |
                              data$Ground.Zone == 853 | data$Ground.Zone == 855 | data$Ground.Zone == 856 |
                              data$Ground.Zone == 857 | data$Ground.Zone == 859 | data$Ground.Zone == 860 |
                              data$Ground.Zone == 863 | data$Ground.Zone == 865 | data$Ground.Zone == 870 |
                              data$Ground.Zone == 871 | data$Ground.Zone == 872 | data$Ground.Zone == 873 |
                              data$Ground.Zone == 874 | data$Ground.Zone == 875), 5, data$Ground.Zone)

##### Zone 6 #####
### Air Zone 6
data$Air.Zone <- ifelse((data$Air.Zone == 147 | data$Air.Zone == 150 | data$Air.Zone == 151 | 
                           data$Air.Zone == 152 | data$Air.Zone == 153 | data$Air.Zone == 154 |
                           data$Air.Zone == 155 | data$Air.Zone == 156 | data$Air.Zone == 157 |
                           data$Air.Zone == 158 | data$Air.Zone == 159 | data$Air.Zone == 160 |
                           data$Air.Zone == 161 | data$Air.Zone == 162 | data$Air.Zone == 163 | 
                           data$Air.Zone == 164 | data$Air.Zone == 165 | data$Air.Zone == 166 | 
                           data$Air.Zone == 167 | data$Air.Zone == 168 | data$Air.Zone == 173 |
                           data$Air.Zone == 174 | data$Air.Zone == 200 | data$Air.Zone == 201 |
                           data$Air.Zone == 202 | data$Air.Zone == 203 | data$Air.Zone == 204 |
                           data$Air.Zone == 205 | data$Air.Zone == 206 | data$Air.Zone == 207 |
                           data$Air.Zone == 208 | data$Air.Zone == 209 | data$Air.Zone == 210 |
                           data$Air.Zone == 211 | data$Air.Zone == 212 | data$Air.Zone == 214 |
                           data$Air.Zone == 215 | data$Air.Zone == 216 | data$Air.Zone == 217 |
                           data$Air.Zone == 220 | data$Air.Zone == 221 | data$Air.Zone == 222 |
                           data$Air.Zone == 223 | data$Air.Zone == 224 | data$Air.Zone == 225 | 
                           data$Air.Zone == 226 | data$Air.Zone == 227 | data$Air.Zone == 228 | 
                           data$Air.Zone == 229 | data$Air.Zone == 230 | data$Air.Zone == 231 |
                           data$Air.Zone == 232 | data$Air.Zone == 233 | data$Air.Zone == 234 |
                           data$Air.Zone == 235 | data$Air.Zone == 236 | data$Air.Zone == 237 |
                           data$Air.Zone == 238 | data$Air.Zone == 239 | data$Air.Zone == 240 |
                           data$Air.Zone == 241 | data$Air.Zone == 242 | data$Air.Zone == 243 |
                           data$Air.Zone == 244 | data$Air.Zone == 245 | data$Air.Zone == 246 | 
                           data$Air.Zone == 247 | data$Air.Zone == 248 | data$Air.Zone == 249 | 
                           data$Air.Zone == 250 | data$Air.Zone == 251 | data$Air.Zone == 252 |
                           data$Air.Zone == 253 | data$Air.Zone == 254 | data$Air.Zone == 255 |
                           data$Air.Zone == 256 | data$Air.Zone == 257 | data$Air.Zone == 258 |
                           data$Air.Zone == 259 | data$Air.Zone == 260 | data$Air.Zone == 261 |
                           data$Air.Zone == 262 | data$Air.Zone == 263 | data$Air.Zone == 264 |
                           data$Air.Zone == 265 | data$Air.Zone == 266 | data$Air.Zone == 267 |
                           data$Air.Zone == 268 | data$Air.Zone == 270 | data$Air.Zone == 271 |
                           data$Air.Zone == 272 | data$Air.Zone == 273 | data$Air.Zone == 274 |
                           data$Air.Zone == 275 | data$Air.Zone == 276 | data$Air.Zone == 277 |
                           data$Air.Zone == 278 | data$Air.Zone == 279 | data$Air.Zone == 280 |
                           data$Air.Zone == 281 | data$Air.Zone == 282 | data$Air.Zone == 283 |
                           data$Air.Zone == 284 | data$Air.Zone == 285 | data$Air.Zone == 286 |
                           data$Air.Zone == 290 | data$Air.Zone == 291 | data$Air.Zone == 292 |
                           data$Air.Zone == 293 | data$Air.Zone == 294 | data$Air.Zone == 295 |
                           data$Air.Zone == 297 | data$Air.Zone == 299 | data$Air.Zone == 313 |
                           data$Air.Zone == 314 | data$Air.Zone == 327 | data$Air.Zone == 328 |
                           data$Air.Zone == 329 | data$Air.Zone == 330 | data$Air.Zone == 331 |
                           data$Air.Zone == 332 | data$Air.Zone == 333 | data$Air.Zone == 334 |
                           data$Air.Zone == 339 | data$Air.Zone == 340 | data$Air.Zone == 341 |
                           data$Air.Zone == 347 | data$Air.Zone == 349 | data$Air.Zone == 376 |
                           data$Air.Zone == 410 | data$Air.Zone == 412 | data$Air.Zone == 415 |
                           data$Air.Zone == 416 | data$Air.Zone == 417 | data$Air.Zone == 418 |
                           data$Air.Zone == 430 | data$Air.Zone == 448 | data$Air.Zone == 470 |
                           data$Air.Zone == 431 | data$Air.Zone == 449 | data$Air.Zone == 473 |
                           data$Air.Zone == 432 | data$Air.Zone == 450 | data$Air.Zone == 480 |
                           data$Air.Zone == 433 | data$Air.Zone == 451 | data$Air.Zone == 481 |
                           data$Air.Zone == 434 | data$Air.Zone == 452 | data$Air.Zone == 482 |
                           data$Air.Zone == 435 | data$Air.Zone == 454 | data$Air.Zone == 483 |
                           data$Air.Zone == 436 | data$Air.Zone == 453 | data$Air.Zone == 484 |
                           data$Air.Zone == 437 | data$Air.Zone == 455 | data$Air.Zone == 485 |
                           data$Air.Zone == 438 | data$Air.Zone == 457 | data$Air.Zone == 486 |
                           data$Air.Zone == 439 | data$Air.Zone == 456 | data$Air.Zone == 487 |
                           data$Air.Zone == 440 | data$Air.Zone == 458 | data$Air.Zone == 488 |
                           data$Air.Zone == 441 | data$Air.Zone == 459 | data$Air.Zone == 489 |
                           data$Air.Zone == 442 | data$Air.Zone == 463 | data$Air.Zone == 490 |
                           data$Air.Zone == 443 | data$Air.Zone == 464 | data$Air.Zone == 491 |
                           data$Air.Zone == 444 | data$Air.Zone == 465 | data$Air.Zone == 492 |
                           data$Air.Zone == 445 | data$Air.Zone == 466 | data$Air.Zone == 493 |
                           data$Air.Zone == 446 | data$Air.Zone == 467 | data$Air.Zone == 494 |
                           data$Air.Zone == 447 | data$Air.Zone == 468 | data$Air.Zone == 495 |
                           data$Air.Zone == 521 | data$Air.Zone == 530 | data$Air.Zone == 496 |
                           data$Air.Zone == 531 | data$Air.Zone == 532 | data$Air.Zone == 497 |
                           data$Air.Zone == 533 | data$Air.Zone == 534 | data$Air.Zone == 499 |
                           data$Air.Zone == 535 | data$Air.Zone == 537 | data$Air.Zone == 498 |
                           data$Air.Zone == 539 | data$Air.Zone == 554 | data$Air.Zone == 572 |
                           data$Air.Zone == 540 | data$Air.Zone == 555 | data$Air.Zone == 574 |
                           data$Air.Zone == 541 | data$Air.Zone == 556 | data$Air.Zone == 575 |
                           data$Air.Zone == 542 | data$Air.Zone == 557 | data$Air.Zone == 576 |
                           data$Air.Zone == 543 | data$Air.Zone == 558 | data$Air.Zone == 577 |
                           data$Air.Zone == 544 | data$Air.Zone == 559 | data$Air.Zone == 580 |
                           data$Air.Zone == 545 | data$Air.Zone == 560 | data$Air.Zone == 581 |
                           data$Air.Zone == 546 | data$Air.Zone == 562 | data$Air.Zone == 582 |
                           data$Air.Zone == 547 | data$Air.Zone == 563 | data$Air.Zone == 583 |
                           data$Air.Zone == 548 | data$Air.Zone == 564 | data$Air.Zone == 584 |
                           data$Air.Zone == 549 | data$Air.Zone == 565 | data$Air.Zone == 585 |
                           data$Air.Zone == 550 | data$Air.Zone == 566 | data$Air.Zone == 586 |
                           data$Air.Zone == 551 | data$Air.Zone == 567 | data$Air.Zone == 587 |
                           data$Air.Zone == 553 | data$Air.Zone == 588 | data$Air.Zone == 590 | 
                           data$Air.Zone == 591 | data$Air.Zone == 592 | data$Air.Zone == 593 | 
                           data$Air.Zone == 597 | data$Air.Zone == 600 | data$Air.Zone == 601 | 
                           data$Air.Zone == 602 | data$Air.Zone == 603 | data$Air.Zone == 604 | 
                           data$Air.Zone == 605 | data$Air.Zone == 606 |
                           data$Air.Zone == 607 | data$Air.Zone == 608 | data$Air.Zone == 610 |
                           data$Air.Zone == 611 | data$Air.Zone == 821 | data$Air.Zone == 824|
                           data$Air.Zone == 825 | data$Air.Zone == 834 |
                           data$Air.Zone == 826 | data$Air.Zone == 836 | data$Air.Zone == 847 |
                           data$Air.Zone == 827 | data$Air.Zone == 837 | data$Air.Zone == 864 |
                           data$Air.Zone == 828 | data$Air.Zone == 840 | data$Air.Zone == 889 |
                           data$Air.Zone == 829 | data$Air.Zone == 841 | data$Air.Zone == 890 |
                           data$Air.Zone == 830 | data$Air.Zone == 842 | data$Air.Zone == 891 |
                           data$Air.Zone == 831 | data$Air.Zone == 843 | data$Air.Zone == 893 |
                           data$Air.Zone == 832 | data$Air.Zone == 844 | data$Air.Zone == 894 |
                           data$Air.Zone == 833 | data$Air.Zone == 846 | data$Air.Zone == 895 |
                           data$Air.Zone == 897 | data$Air.Zone == 898 | data$Air.Zone == 900 |
                           data$Air.Zone == 901 | data$Air.Zone == 902 | data$Air.Zone == 903 |
                           data$Air.Zone == 904 | data$Air.Zone == 905 | data$Air.Zone == 906 |
                           data$Air.Zone == 907 | data$Air.Zone == 908 | data$Air.Zone == 910 |
                           data$Air.Zone == 911 | data$Air.Zone == 916 | data$Air.Zone == 921 |
                           data$Air.Zone == 912 | data$Air.Zone == 917 | data$Air.Zone == 922 |
                           data$Air.Zone == 913 | data$Air.Zone == 918 | data$Air.Zone == 923 |
                           data$Air.Zone == 914 | data$Air.Zone == 919 | data$Air.Zone == 924 |
                           data$Air.Zone == 915 | data$Air.Zone == 920 | data$Air.Zone == 925 |
                           data$Air.Zone == 926 | data$Air.Zone == 927 | data$Air.Zone == 928 |
                           data$Air.Zone == 930 | data$Air.Zone == 931 | data$Air.Zone == 932 |
                           data$Air.Zone == 933 | data$Air.Zone == 934 | data$Air.Zone == 935 |
                           data$Air.Zone == 936 | data$Air.Zone == 937 | data$Air.Zone == 938 |
                           data$Air.Zone == 961 | data$Air.Zone == 979 ), 6, data$Air.Zone)

### Ground Zone 6
data$Ground.Zone <- ifelse((data$Ground.Zone == 147 | data$Ground.Zone == 150 | data$Ground.Zone == 151 | 
                              data$Ground.Zone == 152 | data$Ground.Zone == 153 | data$Ground.Zone == 154 |
                              data$Ground.Zone == 155 | data$Ground.Zone == 156 | data$Ground.Zone == 157 |
                              data$Ground.Zone == 158 | data$Ground.Zone == 159 | data$Ground.Zone == 160 |
                              data$Ground.Zone == 161 | data$Ground.Zone == 162 | data$Ground.Zone == 163 | 
                              data$Ground.Zone == 164 | data$Ground.Zone == 165 | data$Ground.Zone == 166 | 
                              data$Ground.Zone == 167 | data$Ground.Zone == 168 | data$Ground.Zone == 173 |
                              data$Ground.Zone == 174 | data$Ground.Zone == 200 | data$Ground.Zone == 201 |
                              data$Ground.Zone == 202 | data$Ground.Zone == 203 | data$Ground.Zone == 204 |
                              data$Ground.Zone == 205 | data$Ground.Zone == 206 | data$Ground.Zone == 207 |
                              data$Ground.Zone == 208 | data$Ground.Zone == 209 | data$Ground.Zone == 210 |
                              data$Ground.Zone == 211 | data$Ground.Zone == 212 | data$Ground.Zone == 214 |
                              data$Ground.Zone == 215 | data$Ground.Zone == 216 | data$Ground.Zone == 217 |
                              data$Ground.Zone == 220 | data$Ground.Zone == 221 | data$Ground.Zone == 222 |
                              data$Ground.Zone == 223 | data$Ground.Zone == 224 | data$Ground.Zone == 225 | 
                              data$Ground.Zone == 226 | data$Ground.Zone == 227 | data$Ground.Zone == 228 | 
                              data$Ground.Zone == 229 | data$Ground.Zone == 230 | data$Ground.Zone == 231 |
                              data$Ground.Zone == 232 | data$Ground.Zone == 233 | data$Ground.Zone == 234 |
                              data$Ground.Zone == 235 | data$Ground.Zone == 236 | data$Ground.Zone == 237 |
                              data$Ground.Zone == 238 | data$Ground.Zone == 239 | data$Ground.Zone == 240 |
                              data$Ground.Zone == 241 | data$Ground.Zone == 242 | data$Ground.Zone == 243 |
                              data$Ground.Zone == 244 | data$Ground.Zone == 245 | data$Ground.Zone == 246 | 
                              data$Ground.Zone == 247 | data$Ground.Zone == 248 | data$Ground.Zone == 249 | 
                              data$Ground.Zone == 250 | data$Ground.Zone == 251 | data$Ground.Zone == 252 |
                              data$Ground.Zone == 253 | data$Ground.Zone == 254 | data$Ground.Zone == 255 |
                              data$Ground.Zone == 256 | data$Ground.Zone == 257 | data$Ground.Zone == 258 |
                              data$Ground.Zone == 259 | data$Ground.Zone == 260 | data$Ground.Zone == 261 |
                              data$Ground.Zone == 262 | data$Ground.Zone == 263 | data$Ground.Zone == 264 |
                              data$Ground.Zone == 265 | data$Ground.Zone == 266 | data$Ground.Zone == 267 |
                              data$Ground.Zone == 268 | data$Ground.Zone == 270 | data$Ground.Zone == 271 |
                              data$Ground.Zone == 272 | data$Ground.Zone == 273 | data$Ground.Zone == 274 |
                              data$Ground.Zone == 275 | data$Ground.Zone == 276 | data$Ground.Zone == 277 |
                              data$Ground.Zone == 278 | data$Ground.Zone == 279 | data$Ground.Zone == 280 |
                              data$Ground.Zone == 281 | data$Ground.Zone == 282 | data$Ground.Zone == 283 |
                              data$Ground.Zone == 284 | data$Ground.Zone == 285 | data$Ground.Zone == 286 |
                              data$Ground.Zone == 290 | data$Ground.Zone == 291 | data$Ground.Zone == 292 |
                              data$Ground.Zone == 293 | data$Ground.Zone == 294 | data$Ground.Zone == 295 |
                              data$Ground.Zone == 297 | data$Ground.Zone == 299 | data$Ground.Zone == 313 |
                              data$Ground.Zone == 314 | data$Ground.Zone == 327 | data$Ground.Zone == 328 |
                              data$Ground.Zone == 329 | data$Ground.Zone == 330 | data$Ground.Zone == 331 |
                              data$Ground.Zone == 332 | data$Ground.Zone == 333 | data$Ground.Zone == 334 |
                              data$Ground.Zone == 339 | data$Ground.Zone == 340 | data$Ground.Zone == 341 |
                              data$Ground.Zone == 347 | data$Ground.Zone == 349 | data$Ground.Zone == 376 |
                              data$Ground.Zone == 410 | data$Ground.Zone == 412 | data$Ground.Zone == 415 |
                              data$Ground.Zone == 416 | data$Ground.Zone == 417 | data$Ground.Zone == 418 |
                              data$Ground.Zone == 430 | data$Ground.Zone == 448 | data$Ground.Zone == 470 |
                              data$Ground.Zone == 431 | data$Ground.Zone == 449 | data$Ground.Zone == 473 |
                              data$Ground.Zone == 432 | data$Ground.Zone == 450 | data$Ground.Zone == 480 |
                              data$Ground.Zone == 433 | data$Ground.Zone == 451 | data$Ground.Zone == 481 |
                              data$Ground.Zone == 434 | data$Ground.Zone == 452 | data$Ground.Zone == 482 |
                              data$Ground.Zone == 435 | data$Ground.Zone == 454 | data$Ground.Zone == 483 |
                              data$Ground.Zone == 436 | data$Ground.Zone == 453 | data$Ground.Zone == 484 |
                              data$Ground.Zone == 437 | data$Ground.Zone == 455 | data$Ground.Zone == 485 |
                              data$Ground.Zone == 438 | data$Ground.Zone == 457 | data$Ground.Zone == 486 |
                              data$Ground.Zone == 439 | data$Ground.Zone == 456 | data$Ground.Zone == 487 |
                              data$Ground.Zone == 440 | data$Ground.Zone == 458 | data$Ground.Zone == 488 |
                              data$Ground.Zone == 441 | data$Ground.Zone == 459 | data$Ground.Zone == 489 |
                              data$Ground.Zone == 442 | data$Ground.Zone == 463 | data$Ground.Zone == 490 |
                              data$Ground.Zone == 443 | data$Ground.Zone == 464 | data$Ground.Zone == 491 |
                              data$Ground.Zone == 444 | data$Ground.Zone == 465 | data$Ground.Zone == 492 |
                              data$Ground.Zone == 445 | data$Ground.Zone == 466 | data$Ground.Zone == 493 |
                              data$Ground.Zone == 446 | data$Ground.Zone == 467 | data$Ground.Zone == 494 |
                              data$Ground.Zone == 447 | data$Ground.Zone == 468 | data$Ground.Zone == 495 |
                              data$Ground.Zone == 521 | data$Ground.Zone == 530 | data$Ground.Zone == 496 |
                              data$Ground.Zone == 531 | data$Ground.Zone == 532 | data$Ground.Zone == 497 |
                              data$Ground.Zone == 533 | data$Ground.Zone == 534 | data$Ground.Zone == 499 |
                              data$Ground.Zone == 535 | data$Ground.Zone == 537 | data$Ground.Zone == 498 |
                              data$Ground.Zone == 539 | data$Ground.Zone == 554 | data$Ground.Zone == 572 |
                              data$Ground.Zone == 540 | data$Ground.Zone == 555 | data$Ground.Zone == 574 |
                              data$Ground.Zone == 541 | data$Ground.Zone == 556 | data$Ground.Zone == 575 |
                              data$Ground.Zone == 542 | data$Ground.Zone == 557 | data$Ground.Zone == 576 |
                              data$Ground.Zone == 543 | data$Ground.Zone == 558 | data$Ground.Zone == 577 |
                              data$Ground.Zone == 544 | data$Ground.Zone == 559 | data$Ground.Zone == 580 |
                              data$Ground.Zone == 545 | data$Ground.Zone == 560 | data$Ground.Zone == 581 |
                              data$Ground.Zone == 546 | data$Ground.Zone == 562 | data$Ground.Zone == 582 |
                              data$Ground.Zone == 547 | data$Ground.Zone == 563 | data$Ground.Zone == 583 |
                              data$Ground.Zone == 548 | data$Ground.Zone == 564 | data$Ground.Zone == 584 |
                              data$Ground.Zone == 549 | data$Ground.Zone == 565 | data$Ground.Zone == 585 |
                              data$Ground.Zone == 550 | data$Ground.Zone == 566 | data$Ground.Zone == 586 |
                              data$Ground.Zone == 551 | data$Ground.Zone == 567 | data$Ground.Zone == 587 |
                              data$Ground.Zone == 553 | data$Ground.Zone == 588 | data$Ground.Zone == 590 | 
                              data$Ground.Zone == 591 | data$Ground.Zone == 592 | data$Ground.Zone == 593 | 
                              data$Ground.Zone == 597 | data$Ground.Zone == 600 | data$Ground.Zone == 601 | 
                              data$Ground.Zone == 602 | data$Ground.Zone == 603 |
                              data$Ground.Zone == 604 | data$Ground.Zone == 605 | data$Ground.Zone == 606 |
                              data$Ground.Zone == 607 | data$Ground.Zone == 608 | data$Ground.Zone == 610 |
                              data$Ground.Zone == 611 | data$Ground.Zone == 821 | data$Ground.Zone == 824|
                              data$Ground.Zone == 825 | data$Ground.Zone == 834 |
                              data$Ground.Zone == 826 | data$Ground.Zone == 836 | data$Ground.Zone == 847 |
                              data$Ground.Zone == 827 | data$Ground.Zone == 837 | data$Ground.Zone == 864 |
                              data$Ground.Zone == 828 | data$Ground.Zone == 840 | data$Ground.Zone == 889 |
                              data$Ground.Zone == 829 | data$Ground.Zone == 841 | data$Ground.Zone == 890 |
                              data$Ground.Zone == 830 | data$Ground.Zone == 842 | data$Ground.Zone == 891 |
                              data$Ground.Zone == 831 | data$Ground.Zone == 843 | data$Ground.Zone == 893 |
                              data$Ground.Zone == 832 | data$Ground.Zone == 844 | data$Ground.Zone == 894 |
                              data$Ground.Zone == 833 | data$Ground.Zone == 846 | data$Ground.Zone == 895 |
                              data$Ground.Zone == 897 | data$Ground.Zone == 898 | data$Ground.Zone == 900 |
                              data$Ground.Zone == 901 | data$Ground.Zone == 902 | data$Ground.Zone == 903 |
                              data$Ground.Zone == 904 | data$Ground.Zone == 905 | data$Ground.Zone == 906 |
                              data$Ground.Zone == 907 | data$Ground.Zone == 908 | data$Ground.Zone == 910 |
                              data$Ground.Zone == 911 | data$Ground.Zone == 916 | data$Ground.Zone == 921 |
                              data$Ground.Zone == 912 | data$Ground.Zone == 917 | data$Ground.Zone == 922 |
                              data$Ground.Zone == 913 | data$Ground.Zone == 918 | data$Ground.Zone == 923 |
                              data$Ground.Zone == 914 | data$Ground.Zone == 919 | data$Ground.Zone == 924 |
                              data$Ground.Zone == 915 | data$Ground.Zone == 920 | data$Ground.Zone == 925 |
                              data$Ground.Zone == 926 | data$Ground.Zone == 927 | data$Ground.Zone == 928 |
                              data$Ground.Zone == 930 | data$Ground.Zone == 931 | data$Ground.Zone == 932 |
                              data$Ground.Zone == 933 | data$Ground.Zone == 934 | data$Ground.Zone == 935 |
                              data$Ground.Zone == 936 | data$Ground.Zone == 937 | data$Ground.Zone == 938 |
                              data$Ground.Zone == 961 | data$Ground.Zone == 979 ), 6, data$Ground.Zone)

##### Zone 7 #####
### Air Zone 7
data$Air.Zone <- ifelse((data$Air.Zone == 005 | data$Air.Zone == 010 | data$Air.Zone == 011 | 
                           data$Air.Zone == 012 | data$Air.Zone == 013 | data$Air.Zone == 014 |
                           data$Air.Zone == 015 | data$Air.Zone == 016 | data$Air.Zone == 017 |
                           data$Air.Zone == 018 | data$Air.Zone == 019 | data$Air.Zone == 020 |
                           data$Air.Zone == 021 | data$Air.Zone == 022 | data$Air.Zone == 023 |
                           data$Air.Zone == 024 | data$Air.Zone == 025 | data$Air.Zone == 026 |
                           data$Air.Zone == 027 | data$Air.Zone == 028 | data$Air.Zone == 029 |
                           data$Air.Zone == 030 | data$Air.Zone == 031 | data$Air.Zone == 032 |
                           data$Air.Zone == 033 | data$Air.Zone == 034 | data$Air.Zone == 035 |
                           data$Air.Zone == 036 | data$Air.Zone == 037 | data$Air.Zone == 038 |
                           data$Air.Zone == 039 | data$Air.Zone == 050 | data$Air.Zone == 051 | 
                           data$Air.Zone == 052 |
                           data$Air.Zone == 053 | data$Air.Zone == 054 | data$Air.Zone == 055 |
                           data$Air.Zone == 056 | data$Air.Zone == 057 | data$Air.Zone == 058 |
                           data$Air.Zone == 059 | data$Air.Zone == 060 | data$Air.Zone == 061 |
                           data$Air.Zone == 062 | data$Air.Zone == 063 | data$Air.Zone == 064 |
                           data$Air.Zone == 065 | data$Air.Zone == 066 | data$Air.Zone == 067 |
                           data$Air.Zone == 068 | data$Air.Zone == 069 | data$Air.Zone == 070 |
                           data$Air.Zone == 071 | data$Air.Zone == 072 | data$Air.Zone == 073 |
                           data$Air.Zone == 074 | data$Air.Zone == 075 | data$Air.Zone == 076 |
                           data$Air.Zone == 077 | data$Air.Zone == 078 | data$Air.Zone == 079 |
                           data$Air.Zone == 080 | data$Air.Zone == 081 | data$Air.Zone == 082 | 
                           data$Air.Zone == 083 | data$Air.Zone == 084 | data$Air.Zone == 085 | 
                           data$Air.Zone == 086 | data$Air.Zone == 087 | data$Air.Zone == 088 | 
                           data$Air.Zone == 089 | data$Air.Zone == 090 | data$Air.Zone == 091 | 
                           data$Air.Zone == 092 | data$Air.Zone == 093 | data$Air.Zone == 094 |
                           data$Air.Zone == 095 | data$Air.Zone == 096 | data$Air.Zone == 097 |
                           data$Air.Zone == 098 | data$Air.Zone == 099 | data$Air.Zone == 100 |
                           data$Air.Zone == 101 | data$Air.Zone == 102 | data$Air.Zone == 103 |
                           data$Air.Zone == 104 | data$Air.Zone == 105 | data$Air.Zone == 106 |
                           data$Air.Zone == 107 | data$Air.Zone == 108 | data$Air.Zone == 109 | 
                           data$Air.Zone == 110 | data$Air.Zone == 111 | data$Air.Zone == 112 | 
                           data$Air.Zone == 113 | data$Air.Zone == 114 | data$Air.Zone == 115 | 
                           data$Air.Zone == 116 | data$Air.Zone == 117 | data$Air.Zone == 118 | 
                           data$Air.Zone == 119 | data$Air.Zone == 120 | data$Air.Zone == 121 |
                           data$Air.Zone == 122 | data$Air.Zone == 123 | data$Air.Zone == 124 |
                           data$Air.Zone == 125 | data$Air.Zone == 126 | data$Air.Zone == 127 |
                           data$Air.Zone == 128 | data$Air.Zone == 129 | data$Air.Zone == 130 |
                           data$Air.Zone == 131 | data$Air.Zone == 132 | data$Air.Zone == 133 |
                           data$Air.Zone == 134 | data$Air.Zone == 135 | data$Air.Zone == 136 | 
                           data$Air.Zone == 137 | data$Air.Zone == 138 | data$Air.Zone == 139 | 
                           data$Air.Zone == 140 | data$Air.Zone == 141 | data$Air.Zone == 142 | 
                           data$Air.Zone == 143 | data$Air.Zone == 144 | data$Air.Zone == 145 | 
                           data$Air.Zone == 146 | data$Air.Zone == 148 | data$Air.Zone == 149 |
                           data$Air.Zone == 169| data$Air.Zone == 170 | data$Air.Zone == 171 |
                           data$Air.Zone == 172 | data$Air.Zone == 175 | data$Air.Zone == 176 |
                           data$Air.Zone == 177 | data$Air.Zone == 178 | data$Air.Zone == 179 |
                           data$Air.Zone == 180 | data$Air.Zone == 181 | data$Air.Zone == 182 |
                           data$Air.Zone == 183 | data$Air.Zone == 184 | data$Air.Zone == 185 |
                           data$Air.Zone == 186 | data$Air.Zone == 187 | data$Air.Zone == 188 |
                           data$Air.Zone == 189 | data$Air.Zone == 190 | data$Air.Zone == 191 |
                           data$Air.Zone == 192 | data$Air.Zone == 193 | data$Air.Zone == 194 |
                           data$Air.Zone == 195 | data$Air.Zone == 196 | data$Air.Zone == 197 |
                           data$Air.Zone == 198 | data$Air.Zone == 199 | data$Air.Zone == 218 |
                           data$Air.Zone == 219 | data$Air.Zone == 594 | data$Air.Zone == 595 |
                           data$Air.Zone == 596 | data$Air.Zone == 598 | data$Air.Zone == 599 |
                           data$Air.Zone == 835| data$Air.Zone == 838 | data$Air.Zone == 939 |
                           data$Air.Zone == 940 | data$Air.Zone == 941 | data$Air.Zone == 942 |
                           data$Air.Zone == 943 | data$Air.Zone == 944 | data$Air.Zone == 945 |
                           data$Air.Zone == 946 | data$Air.Zone == 947 | data$Air.Zone == 948 |
                           data$Air.Zone == 949 | data$Air.Zone == 950 | data$Air.Zone == 951 |
                           data$Air.Zone == 952 | data$Air.Zone == 953 | data$Air.Zone == 954 |
                           data$Air.Zone == 955 | data$Air.Zone == 956 | data$Air.Zone == 957 |
                           data$Air.Zone == 958 | data$Air.Zone == 959 | data$Air.Zone == 960 |
                           data$Air.Zone == 962 | data$Air.Zone == 963 | data$Air.Zone == 964 |
                           data$Air.Zone == 965 | data$Air.Zone == 966 | data$Air.Zone == 970 |
                           data$Air.Zone == 971 | data$Air.Zone == 972 | data$Air.Zone == 973 |
                           data$Air.Zone == 974 | data$Air.Zone == 975 | data$Air.Zone == 976 |
                           data$Air.Zone == 977 | data$Air.Zone == 978 | data$Air.Zone == 980 |
                           data$Air.Zone == 981 | data$Air.Zone == 982 | data$Air.Zone == 983 |
                           data$Air.Zone == 984 | data$Air.Zone == 985 | data$Air.Zone == 986 |
                           data$Air.Zone == 988 | data$Air.Zone == 989 | data$Air.Zone == 990 |
                           data$Air.Zone == 991 | data$Air.Zone == 992 | data$Air.Zone == 993 |
                           data$Air.Zone == 994), 7, data$Air.Zone)

### Ground Zone 7
data$Ground.Zone <- ifelse((data$Ground.Zone == 005 | data$Ground.Zone == 010 | data$Ground.Zone == 011 | 
                              data$Ground.Zone == 012 | data$Ground.Zone == 013 | data$Ground.Zone == 014 |
                              data$Ground.Zone == 015 | data$Ground.Zone == 016 | data$Ground.Zone == 017 |
                              data$Ground.Zone == 018 | data$Ground.Zone == 019 | data$Ground.Zone == 020 |
                              data$Ground.Zone == 021 | data$Ground.Zone == 022 | data$Ground.Zone == 023 |
                              data$Ground.Zone == 024 | data$Ground.Zone == 025 | data$Ground.Zone == 026 |
                              data$Ground.Zone == 027 | data$Ground.Zone == 028 | data$Ground.Zone == 029 |
                              data$Ground.Zone == 030 | data$Ground.Zone == 031 | data$Ground.Zone == 032 |
                              data$Ground.Zone == 033 | data$Ground.Zone == 034 | data$Ground.Zone == 035 |
                              data$Ground.Zone == 036 | data$Ground.Zone == 037 | data$Ground.Zone == 038 |
                              data$Ground.Zone == 039 | data$Ground.Zone == 050 | data$Ground.Zone == 051 | 
                              data$Ground.Zone == 052 |
                              data$Ground.Zone == 053 | data$Ground.Zone == 054 | data$Ground.Zone == 055 |
                              data$Ground.Zone == 056 | data$Ground.Zone == 057 | data$Ground.Zone == 058 |
                              data$Ground.Zone == 059 | data$Ground.Zone == 060 | data$Ground.Zone == 061 |
                              data$Ground.Zone == 062 | data$Ground.Zone == 063 | data$Ground.Zone == 064 |
                              data$Ground.Zone == 065 | data$Ground.Zone == 066 | data$Ground.Zone == 067 |
                              data$Ground.Zone == 068 | data$Ground.Zone == 069 | data$Ground.Zone == 070 |
                              data$Ground.Zone == 071 | data$Ground.Zone == 072 | data$Ground.Zone == 073 |
                              data$Ground.Zone == 074 | data$Ground.Zone == 075 | data$Ground.Zone == 076 |
                              data$Ground.Zone == 077 | data$Ground.Zone == 078 | data$Ground.Zone == 079 |
                              data$Ground.Zone == 080 | data$Ground.Zone == 081 | data$Ground.Zone == 082 | 
                              data$Ground.Zone == 083 | data$Ground.Zone == 084 | data$Ground.Zone == 085 | 
                              data$Ground.Zone == 086 | data$Ground.Zone == 087 | data$Ground.Zone == 088 | 
                              data$Ground.Zone == 089 | data$Ground.Zone == 090 | data$Ground.Zone == 091 | 
                              data$Ground.Zone == 092 | data$Ground.Zone == 093 | data$Ground.Zone == 094 |
                              data$Ground.Zone == 095 | data$Ground.Zone == 096 | data$Ground.Zone == 097 |
                              data$Ground.Zone == 098 | data$Ground.Zone == 099 | data$Ground.Zone == 100 |
                              data$Ground.Zone == 101 | data$Ground.Zone == 102 | data$Ground.Zone == 103 |
                              data$Ground.Zone == 104 | data$Ground.Zone == 105 | data$Ground.Zone == 106 |
                              data$Ground.Zone == 107 | data$Ground.Zone == 108 | data$Ground.Zone == 109 | 
                              data$Ground.Zone == 110 | data$Ground.Zone == 111 | data$Ground.Zone == 112 | 
                              data$Ground.Zone == 113 | data$Ground.Zone == 114 | data$Ground.Zone == 115 | 
                              data$Ground.Zone == 116 | data$Ground.Zone == 117 | data$Ground.Zone == 118 | 
                              data$Ground.Zone == 119 | data$Ground.Zone == 120 | data$Ground.Zone == 121 |
                              data$Ground.Zone == 122 | data$Ground.Zone == 123 | data$Ground.Zone == 124 |
                              data$Ground.Zone == 125 | data$Ground.Zone == 126 | data$Ground.Zone == 127 |
                              data$Ground.Zone == 128 | data$Ground.Zone == 129 | data$Ground.Zone == 130 |
                              data$Ground.Zone == 131 | data$Ground.Zone == 132 | data$Ground.Zone == 133 |
                              data$Ground.Zone == 134 | data$Ground.Zone == 135 | data$Ground.Zone == 136 | 
                              data$Ground.Zone == 137 | data$Ground.Zone == 138 | data$Ground.Zone == 139 | 
                              data$Ground.Zone == 140 | data$Ground.Zone == 141 | data$Ground.Zone == 142 | 
                              data$Ground.Zone == 143 | data$Ground.Zone == 144 | data$Ground.Zone == 145 | 
                              data$Ground.Zone == 146 | data$Ground.Zone == 148 | data$Ground.Zone == 149 |
                              data$Ground.Zone == 169| data$Ground.Zone == 170 | data$Ground.Zone == 171 |
                              data$Ground.Zone == 172 | data$Ground.Zone == 175 | data$Ground.Zone == 176 |
                              data$Ground.Zone == 177 | data$Ground.Zone == 178 | data$Ground.Zone == 179 |
                              data$Ground.Zone == 180 | data$Ground.Zone == 181 | data$Ground.Zone == 182 |
                              data$Ground.Zone == 183 | data$Ground.Zone == 184 | data$Ground.Zone == 185 |
                              data$Ground.Zone == 186 | data$Ground.Zone == 187 | data$Ground.Zone == 188 |
                              data$Ground.Zone == 189 | data$Ground.Zone == 190 | data$Ground.Zone == 191 |
                              data$Ground.Zone == 192 | data$Ground.Zone == 193 | data$Ground.Zone == 194 |
                              data$Ground.Zone == 195 | data$Ground.Zone == 196 | data$Ground.Zone == 197 |
                              data$Ground.Zone == 198 | data$Ground.Zone == 199 | data$Ground.Zone == 218 |
                              data$Ground.Zone == 219 | data$Ground.Zone == 594 | data$Ground.Zone == 595 |
                              data$Ground.Zone == 596 | data$Ground.Zone == 598 | data$Ground.Zone == 599 |
                              data$Ground.Zone == 835| data$Ground.Zone == 838 | data$Ground.Zone == 939 |
                              data$Ground.Zone == 940 | data$Ground.Zone == 941 | data$Ground.Zone == 942 |
                              data$Ground.Zone == 943 | data$Ground.Zone == 944 | data$Ground.Zone == 945 |
                              data$Ground.Zone == 946 | data$Ground.Zone == 947 | data$Ground.Zone == 948 |
                              data$Ground.Zone == 949 | data$Ground.Zone == 950 | data$Ground.Zone == 951 |
                              data$Ground.Zone == 952 | data$Ground.Zone == 953 | data$Ground.Zone == 954 |
                              data$Ground.Zone == 955 | data$Ground.Zone == 956 | data$Ground.Zone == 957 |
                              data$Ground.Zone == 958 | data$Ground.Zone == 959 | data$Ground.Zone == 960 |
                              data$Ground.Zone == 962 | data$Ground.Zone == 963 | data$Ground.Zone == 964 |
                              data$Ground.Zone == 965 | data$Ground.Zone == 966 | data$Ground.Zone == 970 |
                              data$Ground.Zone == 971 | data$Ground.Zone == 972 | data$Ground.Zone == 973 |
                              data$Ground.Zone == 974 | data$Ground.Zone == 975 | data$Ground.Zone == 976 |
                              data$Ground.Zone == 977 | data$Ground.Zone == 978 | data$Ground.Zone == 980 |
                              data$Ground.Zone == 981 | data$Ground.Zone == 982 | data$Ground.Zone == 983 |
                              data$Ground.Zone == 984 | data$Ground.Zone == 985 | data$Ground.Zone == 986 |
                              data$Ground.Zone == 988 | data$Ground.Zone == 989 | data$Ground.Zone == 990 |
                              data$Ground.Zone == 991 | data$Ground.Zone == 992 | data$Ground.Zone == 993 |
                              data$Ground.Zone == 994), 7, data$Ground.Zone)

##### Zone 8 #####
### Air Zone 8
data$Air.Zone <- ifelse((data$Air.Zone == 040 | data$Air.Zone == 041 | data$Air.Zone == 042 | 
                           data$Air.Zone == 043 | data$Air.Zone == 044 | data$Air.Zone == 045 |
                           data$Air.Zone == 046 | data$Air.Zone == 047 | data$Air.Zone == 048 | 
                           data$Air.Zone == 049 | data$Air.Zone == 042 | data$Air.Zone == 043), 8, data$Air.Zone)
### Ground Zone 8
data$Ground.Zone <- ifelse((data$Ground.Zone == 040 | data$Ground.Zone == 041 | data$Ground.Zone == 042 | 
                              data$Ground.Zone == 043 | data$Ground.Zone == 044 | data$Ground.Zone == 045 |
                              data$Ground.Zone == 046 | data$Ground.Zone == 047 | data$Ground.Zone == 048 | 
                              data$Ground.Zone == 049 | data$Ground.Zone == 042 | data$Ground.Zone == 043), 8, data$Ground.Zone)

##### Zone 9 #####
data$Destination.Zip <- substr(data$Destination.Zip , start = 1, stop = 5)
data$Destination.Zip <- as.numeric(data$Destination.Zip)

### Air Zone 9
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99501,99524))){
    data[n,names(data)=="Air.Zone"] <- 9
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99529,99530))){
    data[n,names(data)=="Air.Zone"] <- 9
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99501,99524))){
    data[n,names(data)=="Air.Zone"] <- 9
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 99540){
    data[n,names(data)=="Air.Zone"] <- 9
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 99567){
    data[n,names(data)=="Air.Zone"] <- 9
  }
}

### Ground Zone 9
data$Ground.Zone <- ifelse((data$Ground.Zone == 967 | data$Ground.Zone == 968), 9, data$Ground.Zone)


##### Zone 10 #####
### Air Zone 10
data$Air.Zone <-  ifelse((data$Destination.Zip == 96701 | data$Destination.Zip == 96706 | data$Destination.Zip == 96707 | 
                            data$Destination.Zip == 96701 | data$Destination.Zip == 96706 | data$Destination.Zip == 96707 | 
                            data$Destination.Zip == 96709 | data$Destination.Zip == 96711 | data$Destination.Zip == 96712 | 
                            data$Destination.Zip == 96717 | data$Destination.Zip == 96730 | data$Destination.Zip == 96731 | 
                            data$Destination.Zip == 96734 | data$Destination.Zip == 96744 | data$Destination.Zip == 96758 | 
                            data$Destination.Zip == 96759 | data$Destination.Zip == 96762 | data$Destination.Zip == 96775 | 
                            data$Destination.Zip == 96782 | data$Destination.Zip == 96789 | data$Destination.Zip == 96791 | 
                            data$Destination.Zip == 96792 | data$Destination.Zip == 96794 | data$Destination.Zip == 96795 | 
                            data$Destination.Zip == 96797), 10, data$Air.Zone)

for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96801,96863))){
    data[n,names(data)=="Air.Zone"] <- 10
  }
}
### Ground Zone 10 
### Not Applicable

##### Zone 11 #####

### Air Zone 11
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 99500){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99525,99528))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99525,99528))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99531,99539))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99541,99566))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99568,99576))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 99578,99999))){
    data[n,names(data)=="Air.Zone"] <- 11
  }
}

### Ground Zone 11
### Not Applicable

##### Zone 12 #####
### Air Zone 12
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96700){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96702,96705))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96708){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96710){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96713,96716))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96718,96729))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96732,96733))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96732,96733))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96735,96743))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96745,96757))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96760,96761))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96763,96774))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96776,96781))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96783,96788))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96790){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96793){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96796){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96798){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(data[n,names(data)=="Destination.Zip"] == 96800){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
for(n in 1:nrow(data)) {
  if(unlist(between(data[n,names(data)=="Destination.Zip"], 96864,96899))){
    data[n,names(data)=="Air.Zone"] <- 12
  }
}
### Ground Zone 12


##### Zone 17 #####
### Air Zone 17
### Not Applicable

### Ground Zone 17
data$Ground.Zone <- ifelse((data$Ground.Zone == 995 | data$Ground.Zone == 996), 17, data$Ground.Zone)







##### Shipping Distances #####

### Ensure all Destination.Zip observations have 5 characters, if 4 then add a 0 to the front
for(n in 1:nrow(data)) {
  if(nchar(data[n,names(data)=="Destination.Zip"]) == 4) {
    data[n,names(data)=="Destination.Zip"] <- paste0(0, data[n,names(data)=="Destination.Zip"])
  }
}

### Generate the distance for each package from origin to destination in miles
### San.Antonio.Distance column
#data['San.Antonio.Distance'] <- NA
#for(n in 1:nrow(data)) {
#  data[n,names(data)=="San.Antonio.Distance"] <- 
#    as.numeric(unname(unlist(zip_distance(78219,
#                                          data[n,names(data)=="Destination.Zip"]))))[3]
#}

### Fullerton.Distance Column
#data['Fullerton.Distance'] <- NA
#for(n in 1:nrow(data)) {
#  data[n,names(data)=="Fullerton.Distance"] <- 
#    as.numeric(unname(unlist(zip_distance(92831,
#                                          data[n,names(data)=="Destination.Zip"]))))[3]
#}

### Check for NAs in the new columns
#sum(is.na(data$San.Antonio.Distance))
#sum(is.na(data$Fullerton.Distance))

### Create a CSV of the columns to save time by not running loop again
#San.Antonio.Distance <- data %>%
#  select(Tracking.Number, San.Antonio.Distance)
#write.csv(San.Antonio.Distance, "San.Antonio.Distance.csv", row.names = FALSE)

#Fullerton.Distance <- data %>%
#  select(Tracking.Number, Fullerton.Distance)
#write.csv(Fullerton.Distance, "Fullerton.Distance.csv", row.names = FALSE)

### Merge the CSV distance columns into the data set
Actual.Shipping.Distance <- read.csv("Shipping_Distance.csv")
San.Antonio.Distance <- read.csv("San.Antonio.Distance.csv")
Fullerton.Distance <- read.csv("Fullerton.Distance.csv")

dis <- Actual.Shipping.Distance$Distance
sat <- San.Antonio.Distance$San.Antonio.Distance
full <- Fullerton.Distance$Fullerton.Distance

data$Actual.Shipping.Distance <- dis
data$San.Antonio.Distance <- sat
data$Fullerton.Distance <- full

### Move Shipping Distance columns to a different place in the data set
data <- data %>% relocate(San.Antonio.Distance, .before = Ground.Zone)
data <- data %>% relocate(Fullerton.Distance, .before = Ground.Zone)
data <- data %>% relocate(Actual.Shipping.Distance, .before = San.Antonio.Distance)

###### Determine if the current shipping location is "right" or "wrong" #####
data$Shipped.From.Optimal.Location <- ifelse((data$Origin.Location == 'San Antonio' & data$Actual.Shipping.Distance < data$Fullerton.Distance)
                                             | (data$Origin.Location == 'Fullerton' & data$Actual.Shipping.Distance < data$San.Antonio.Distance), 
                                             0, 1)
### If the package is already shipped from the ideal location, denote a 0.
### If the package could be shipped from a more convenient location, denote a 1.

##### If shipping location is "wrong", calculate miles saved if location is changed to "right" #####
data$Miles.Saved <- ifelse(data$Shipped.From.Optimal.Location == 1, abs(data$San.Antonio.Distance - data$Fullerton.Distance), 0)

### Move new columns to better place in the data set
data <- data %>% relocate(Shipped.From.Optimal.Location, .before = Ground.Zone)
data <- data %>% relocate(Miles.Saved, .before = Ground.Zone)

##### Revalue Shipping.Mode levels #####

### Make Shipping.Mode a factor
data$Shipping.Mode <- as.factor(data$Shipping.Mode)
### View a table of the observation distribution between levels
table(data$Shipping.Mode)

### Combine applicable levels to make modeling more accurate
data$Shipping.Mode <- revalue(data$Shipping.Mode, c("FedEx Freight" = "FedEx Ground"))
data$Shipping.Mode <- revalue(data$Shipping.Mode, c("FedEx 2Day AM" = "FedEx 2Day"))
data$Shipping.Mode <- revalue(data$Shipping.Mode, c("FedEx First Overnight" = "FedEx Priority Overnight"))
data$Shipping.Mode <- revalue(data$Shipping.Mode, c("FedEx Priority Overnight Sat" = "FedEx Priority Overnight"))
data$Shipping.Mode <- revalue(data$Shipping.Mode, c("FedEx Express Saver" = "FedEx 2Day"))

##### Change Column Data Types, if necessary #####
data$Air.Zone <- as.factor(data$Air.Zone)
data$Ground.Zone <- as.factor(data$Ground.Zone)
data$Destination.State <- as.factor(data$Destination.State)
data$Shipped.From.Optimal.Location <- as.factor(data$Shipped.From.Optimal.Location)
data$Destination.Zip <- as.numeric(data$Destination.Zip)

data$XPEL.Shipping.Rate <- gsub(",","", data$XPEL.Shipping.Rate)
data$XPEL.Shipping.Rate <- as.numeric(data$XPEL.Shipping.Rate)

##### Test Train Split #####
train <- data[data$test.train == 0,]
test <- data[data$test.train == 1,]

### Remove the ID and train.test columns from the new data sets
train <- train[ , -c(1, 2, 3, 4, 14, 24, 30)]
test <- test[ , -c(1, 2, 3, 4, 14, 24, 30)]

### Create train/test data sets for extreme boosting model
#xtrm_train <- train
#xtrm_test <- test

##### Pretrain Validation Split #####

### First, create a sample size of 60% of the row numbers in the train data set
smp_size <- floor(0.6 * nrow(train))
### Then, get a random sample of 60% of the original data size to use for the pretrain data set
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
#xtrm_train_ind <- sample(seq_len(nrow(xtrm_train)), size = 6675)
### The pretrain data set is train_ind, and the validation data set is the remainder of 
###   the train data set that was not selected in train_ind
### Split into pretrain and validation for X and Y
pretrain <- train[train_ind, ]
valid <- train[-train_ind, ]

### Create pretrain/validation for extreme boosting model
#xtrm_pretrain <- data.matrix(xtrm_train[, -22])
#xtrm_valid <- xtrm_train[, 22]

#test_x <- data.matrix(xtrm_test[, -22])
#test_y <- xtrm_test[, 22]
##### Calculate total miles saved if optimal shipping locations are used in the Train set #####
sum(train$Miles.Saved)
### If all the optimal shipping locations are used, XPEL would save
### 1,796,502 miles would be saved. 















##################################### Models ##################################
####### Random Forest Model #######
##### Pretrain/Validation #####

### Create the Random Forest model for the pretrain data set
rf_pretrain <- randomForest(FedEx.Shipping.Rate ~ ., pretrain, mtry = 10, 
                            type = 'regression', nodesize = 5, ntree = 700, 
                            importance = T, replace = T)

### Predict the model on the validation data set
predrf_valid <- predict(rf_pretrain, newdata = valid)
predrf_valid <- as.numeric(predrf_valid)

### Calculate and print the MAPE
rf_mape <- MAPE(valid$FedEx.Shipping.Rate, predrf_valid)
print(rf_mape)
### MAPE : 0.1615121


##### Train/Test #####

### When running the function for the train/test model, the best mtry will be the same 
### as it was in the pretrain/validation model. Use mtry = 10.

### Create the Random Forest model for the train data set
rf <- randomForest(FedEx.Shipping.Rate ~ ., train, mtry = 10, 
                   type = 'regression', nodesize = 5, ntree = 700, 
                   importance = T, replace = T)

### Predict the model on the test data set
predrf <- predict(rf, newdata = test)
predrf <- as.numeric(predrf)

### Create a data frame of Tracking.Number and FedEx.Shipping.Rate from the data set
rf_results <- data %>%
  select(Tracking.Number, FedEx.Shipping.Rate) %>%
  filter(is.na(FedEx.Shipping.Rate)) %>%
  arrange(Tracking.Number)

### Add results to the data frame
rf_results$FedEx.Shipping.Rate <- predrf

### Rename the columns
names(rf_results)[names(rf_results) == "Tracking.Number"] <- "Tracking"
names(rf_results)[names(rf_results) == "FedEx.Shipping.Rate"] <- "Response"
rf_results$Tracking <- as.numeric(rf_results$Tracking)

### Input results to a csv file
write.csv(rf_results, "Kash_1.csv", row.names = FALSE)


####### Bagging Model #######
##### Pretrain/Validation #####

### Create bag using pretrain data set
bag_pretrain <- randomForest(FedEx.Shipping.Rate ~ ., n.trees = 550, 
                             importance = T, type = 'regression', pretrain)

### Predict the validation data set using bag
predbag_valid <- predict(bag_pretrain, newdata = valid)

### Calculate and print the MAPE
bag_mape <- MAPE(valid$FedEx.Shipping.Rate, predbag_valid)
print(bag_mape)
### MAPE : 0.1627453


##### Train/Test #####

### Create bag using train data set
bag <- randomForest(FedEx.Shipping.Rate ~ ., n.trees = 550, 
                    importance = T, type = 'regression', train)

### Predict the test data set using bag
predbag <- predict(bag, newdata = test)

### Create a data frame of Tracking.Number and FedEx.Shipping.Rate from the data set
bag_results <- data %>%
  select(Tracking.Number, FedEx.Shipping.Rate) %>%
  filter(is.na(FedEx.Shipping.Rate)) %>%
  arrange(Tracking.Number)

### Add results to the data frame
bag_results$FedEx.Shipping.Rate <- predbag

### Rename the columns
names(bag_results)[names(bag_results) == "Tracking.Number"] <- "Tracking"
names(bag_results)[names(bag_results) == "FedEx.Shipping.Rate"] <- "Response"
bag_results$Tracking <- as.numeric(bag_results$Tracking)

### Input results to a csv file
write.csv(bag_results, "Kash_2.csv", row.names = FALSE)



####### Boosting Model #######
##### Pretrain/Validation #####

### Create a boost model using all factor and numeric variables in the pretrain data set
boost_pretrain1 <- gbm(FedEx.Shipping.Rate ~ ., pretrain, distribution = "gaussian", 
                       n.trees = 80, bag.fraction = .7)

### Predict the validation data set using the boost model with all variables
predboost_pretrain1 <- predict.gbm(boost_pretrain1, newdata = valid, distribution = "gaussian", 
                                   n.trees = 80, bag.fraction = .7)
### Calculate and print the MAPE
boost_mape1 <- MAPE(valid$FedEx.Shipping.Rate, predboost_pretrain1)
print(boost_mape1)
### MAPE : 0.240343   (best was 0.2395072)
### View model summary to determine which variables are impactful in the model
summary(boost_pretrain1)
### The most impactful variables in the model are: 
###   Shipping.Mode, XPEL.Shipping.Rate, Package.Length, FedEx.Package.Weight,
###   Destination.State, and Actual.Shipping.Distance
### All other variables in the model had a rel.inf below .01 or = 0, meaning they are not 
### necessary in the model's calculations.

### Create a new model containing only the impactful variables
boost_pretrain <- gbm(FedEx.Shipping.Rate ~ Shipping.Mode + XPEL.Shipping.Rate + 
                        Package.Length + FedEx.Package.Weight + Actual.Shipping.Distance
                      + Destination.State, pretrain, distribution = "gaussian", 
                      n.trees = 80, bag.fraction = .7)

### Predict the validation data set using boost model
predboost_pretrain <- predict.gbm(boost_pretrain, newdata = valid, distribution = "gaussian", 
                                  n.trees = 80, bag.fraction = .7)

### Calculate and print the MAPE
boost_mape <- MAPE(valid$FedEx.Shipping.Rate, predboost_pretrain)
print(boost_mape)
### MAPE : 0.2376393
### Since this MAPE score is lower than the MAPE generated when all variables were used
###   in the first model, use the second model for train/test modeling


##### Train/Test #####

### Create the boost model
boost <- gbm(FedEx.Shipping.Rate ~ Shipping.Mode + XPEL.Shipping.Rate + 
               Package.Length + FedEx.Package.Weight + Actual.Shipping.Distance
             + Destination.State, train, distribution = "gaussian", 
             n.trees = 80, bag.fraction = .7)

### Predict the test data set using boost model
predboost <- predict.gbm(boost_pretrain, newdata = test, distribution = "gaussian", 
                         n.trees = 80, bag.fraction = .7)
### Create a data frame of Tracking.Number and FedEx.Shipping.Rate from the data set
boost_results <- data %>%
  select(Tracking.Number, FedEx.Shipping.Rate) %>%
  filter(is.na(FedEx.Shipping.Rate)) %>%
  arrange(Tracking.Number)

### Add results to the data frame
boost_results$FedEx.Shipping.Rate <- predboost

### Rename the columns
names(boost_results)[names(boost_results) == "Tracking.Number"] <- "Tracking"
names(boost_results)[names(boost_results) == "FedEx.Shipping.Rate"] <- "Response"
boost_results$Tracking <- as.numeric(boost_results$Tracking)

### Input results to a csv file
write.csv(boost_results, "Kash_3.csv", row.names = FALSE)


