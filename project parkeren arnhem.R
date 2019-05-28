library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

##function which returns a dataset containing data about parking (gargage) traffic in Arnhem (NL)
##Users needs to specify the start and end date of the period
parkingtraffic <- function(start,end){
  ##check if date input is correct and stop the function when it's not
  if (!grepl("[2][0][0-1][0-9]([0][1-9]|[1][1-2])", start) | 
      !grepl("[2][0][0-1][0-9]([0][1-9]|[1][1-2])", end)) {
    stop ("invalid date selection")
  }
  ##create a sequence that can be used in the for loop so that the files of all months can be 
  ##dowloaded
  yearmonth <- (start:end)
  ##only selecting year month combinations from the sequence
  yearmonth <- yearmonth[grepl("[2][0][0-1][0-9]([0][1-9]|[1][1-2])", yearmonth)]
  ##url without the file names, these will be provided in the for loop
  url <- "https://monitdata.s3.amazonaws.com/arnhem/offstreet/transactions/arnhem_opendata_offstreet_"
  ##empty list that will be used to store the data coming from the for loop
  output <- vector(mode = "list",length = length(yearmonth))
for (i in 1:length(yearmonth)){
  ##checks if file does not exists, if not then these will we downloaded - otherwise data will
  ##be read from the existing csv
  if(!file.exists(paste("parkerenarnhem", yearmonth[i], ".csv", sep=""))){
  download.file(paste(url, yearmonth[i], ".csv", sep=""), destfile = paste("parkerenarnhem", yearmonth[i], ".csv", sep=""))}
  output[[i]] <- read.csv(paste("parkerenarnhem", yearmonth[i],".csv", sep = ""), sep = ";")
}
  ##creating a data frame from the list
  data <- do.call(rbind, output)
  ##assigning the data object to a global data object in the globalenv
  assign("data", data, envir=globalenv())
}
##formatting the data
data <- tbl_df()
data$start_parking_dt <- ymd_hms(data$start_parking_dt)
data$start_parking_date <- as.Date(data$start_parking_dt)
data$pay_parking_dt <- ymd_hms(data$pay_parking_dt)
data$end_parking_dt <- ymd_hms(data$end_parking_dt)

##creating time variables
data$parking_duration <- as.numeric(difftime(data$end_parking_dt,data$start_parking_dt, 
                                  units = c("secs")))
data$year <- year(data$start_parking_date)
data$week <- isoweek(data$start_parking_date)


##summarizing data - average cars parking per day/week/month
ndays <- length(unique(as.Date(data$start_parking_dt)))
nweeks <- ndays/7
avgtraffic <- data %>% mutate() %>% tbl_df() %>% group_by(garage_nm,card_type_nm) %>% 
  summarise(daily_avg = n() / ndays,weekly_avg = n() / nweeks, 
            fourweekly_avg = n() / (nweeks/4))

##percentage abbonementen by garage name
data_perc <-  data %>%
  group_by(garage_nm,card_type_nm) %>%
  summarise(total = n()) %>%
  dcast(total + garage_nm ~ card_type_nm, fill=0) %>%
  group_by(garage_nm) %>%
  summarise(abbo_perc = percent(sum(Abonnement)/(sum(Abonnement)+sum(`Kort parkeren`))))
                                                                       
##boxplots of parking duration per garage
duration <- data %>% group_by(parking_duration,garage_nm) %>% summarise(total = n())
durationplot <- ggplot(data, aes(x = factor(garage_nm), y = parking_duration/3600,fill = garage_nm))
durationplot + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(.~data$card_type_nm) + 
  coord_cartesian(ylim = c(0,20)) +
  labs(y = "hours", x = "garage name", title = "parking duration by garagae name")

##trendline per week
weeklydata <-
  data %>%
  group_by(year_week = floor_date(data$start_parking_date, "1 week"),
           garage_nm, card_type_nm) %>%
  summarize(count = n())

ggplot(weeklydata, aes(x = year_week, y = count, color = garage_nm)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  theme_bw() + 
  scale_x_date(breaks = "6 month", minor_breaks = "2 month") + 
  facet_grid(.~card_type_nm, scales = "free")


##histogram of parking volume per day of week
plot2 <- ggplot(data,aes(weekdays(as.Date(data$start_parking_dt)), color = card_type_nm))
plot2 + geom_histogram(stat = "count") + facet_wrap(.~data$garage_nm)


  
