# Loading the needed libraries

library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(skimr)
library(binsreg)
library(splines)
library(sf)
library(estimatr)
library(fixest)

# Simon working directory:   "G:/My Drive/In Course Project/"

# Steve working directory:  "C:/Users/Owner/Documents/GitHub/Project-ECNS-560"

# set working directory before continuing!

#Immigration Data (Inflow Data)
#2019/2020
data2=read_csv("countyinflow1920.csv")
inflow_1920 = data2[grep("Total Migration-US and Foreign",data2$y1_countyname), ]
inflow_1920 = inflow_1920 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1920 = inflow_1920|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1920=inflow_1920|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1920) = c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2018/2019
data3=read_csv("countyinflow1819.csv")
inflow_1819 = data3[grep("Total Migration-US and Foreign",data3$y1_countyname), ]
inflow_1819 = inflow_1819 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1819 = inflow_1819|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1819=inflow_1819|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1819) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2017/2018
data4=read_csv("countyinflow1718.csv")
inflow_1718 = data4[grep("Total Migration-US and Foreign",data4$y1_countyname), ]
inflow_1718 = inflow_1718 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1718 = inflow_1718|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1718=inflow_1718|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1718) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2016/2017
data5=read_csv("countyinflow1617.csv")
inflow_1617 = data5[grep("Total Migration-US and Foreign",data5$y1_countyname), ]
inflow_1617 = inflow_1617 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1617 = inflow_1617|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1617=inflow_1617|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1617) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2015/2016
data6=read_csv("countyinflow1516.csv")
inflow_1516 = data6[grep("Total Migration-US and Foreign",data6$y1_countyname), ]
inflow_1516 = inflow_1516 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1516 = inflow_1516|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1516=inflow_1516|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1516) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2014/2015
data7=read_csv("countyinflow1415.csv")
inflow_1415 = data7[grep("Total Migration-US and Foreign",data7$y1_countyname), ]
inflow_1415 = inflow_1415 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1415 = inflow_1415|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1415=inflow_1415|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1415) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2013/2014
data8=read_csv("countyinflow1314.csv")
inflow_1314 = data8[grep("Total Migration-US and Foreign",data8$y1_countyname), ]
inflow_1314 = inflow_1314 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1314 = inflow_1314|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1314=inflow_1314|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1314) =  c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#2012/2013
data9=read_csv("countyinflow1213.csv")
inflow_1213 = data9[grep("Total Migration-US and Foreign",data9$y1_countyname), ]
inflow_1213 = inflow_1213 |>
  mutate(FIPS=paste(y2_statefips,y2_countyfips,sep = ""))
inflow_1213 = inflow_1213|>
  filter(y2_countyfips !="000")
# Removes the Total Migration-US and Foreign 
inflow_1213=inflow_1213|>
  mutate(y1_countyname=str_replace(y1_countyname,"Total Migration-US and Foreign",""))
colnames(inflow_1213) = c("Y2_STATEFIPS","Y2_COUNTYFIPS","Y1_STATEFIPS","Y1_COUNTYFIPS","Y1_STATE","Y1_COUNTYNAME","NO_OF_RETURN(INFLOW)","NO_OF_INDIVIDUALS(INFLOW)","AGI_INFLOW","FIPS" )

#-----------------------------------
#Housing Prices data
new_data_1=read_csv("UADAggs_county_v3_2.csv")

filtered_data <- new_data_1 |>
  #keeps only observation of the fifth quarter and purchase and drops the 2023 observations.  
  filter(QUARTER == 5, PURPOSE == "Purchase",YEAR != 2023)|>
  arrange(YEAR,GEONAME)|> # 
  filter(SERIES=="Count of Appraisals")|> #Keeps the observations with series ID equals count of appraisal
  arrange(STATEFIPS)
#Removes row 31135 to row 31917 from the data set
dropped_data = filtered_data[-c(31135:31917), ]
# Drops the columns 
housing_prices=dropped_data|>
  select(-c("TRACT","FREQUENCY", "METRO","APPRAISALSOURCE","SERIES","SERIESID","SOURCE","CHARACTERISTIC1", "GEOLEVEL", "CATEGORY1"))
colnames(housing_prices) = c("Y1_COUNTYNAME","STATEPOSTAL","STATEFIPS","FIPS","PURPOSE","YEAR","QUARTER","SUPPRESSED","VALUE")

#Subseting the housing prices according to years
d_13 = housing_prices|>
  filter(YEAR==2013)
d_14= housing_prices|>
  filter(YEAR==2014)
d_15 = housing_prices|>
  filter(YEAR==2015)
d_16 = housing_prices|>
  filter(YEAR==2016)
d_17 = housing_prices|>
  filter(YEAR==2017)
d_18 = housing_prices|>
  filter(YEAR==2018)
d_19 = housing_prices|>
  filter(YEAR==2019)
d_20 = housing_prices|>
  filter(YEAR==2020)

#-----------------------------------
#Merging the results of the two data set in bits according to years 
# 1st
d_13 = housing_prices|>
  filter(YEAR==2013)
merge_1 = inflow_1213|>
  left_join(d_13, by= "FIPS")

# 2nd
d_14= housing_prices|>
  filter(YEAR==2014)
merge_2 = inflow_1314|>
  left_join(d_14, by = "FIPS")

#3rd 
d_15 = housing_prices|>
  filter(YEAR==2015)
merge_3 = inflow_1415|>
  left_join(d_15, by = "FIPS")

#4th
d_16 = housing_prices|>
  filter(YEAR==2016)
merge_4 = inflow_1516|>
  left_join(d_16, by = "FIPS")

#5th 
d_17 = housing_prices|>
  filter(YEAR==2017)
merge_5 = inflow_1617|>
  left_join(d_17, by = "FIPS")

#6th 
d_18 = housing_prices|>
  filter(YEAR==2018)
merge_6 =inflow_1718|>
  left_join(d_18, by = "FIPS")

#7th 
d_19 = housing_prices|>
  filter(YEAR==2019)
merge_7 = inflow_1819|>
  left_join(d_19, by = "FIPS")

#8th 
d_20 = housing_prices|>
  filter(YEAR==2020)
merge_8 = inflow_1920|>
  left_join(d_20, by = "FIPS")


#Combining the merged data sets using rbind
main_data = rbind(merge_1,merge_2,merge_3,merge_4,merge_5,merge_6,merge_7,merge_8)

#provides the statistical summary of the data
skim(main_data)

clean_data = main_data[!is.na(main_data$VALUE),]

#Saving the data
save(clean_data, file =  "clean_data.RData")


# Getting the needed interest rates from the JP Morgan website using Web Scraping

url = "https://www.jpmorganchase.com/legal/historical-prime-rate"

page = read_html(url)

table=page|>html_elements("table")

Prime15to24 = table[[1]] |> html_table()
Prime07to12 = table[[2]] |> html_table()

# Combining two tables to make one covering the needed years for our analysis

prime_rates = rbind(Prime15to24, Prime07to12)

# Clean the data frame, change dates and rates from strings to date format and numerical format
prime_clean = prime_rates

# Change Column Name to Date - which is the date a rate took effect
colnames(prime_clean)[1] = "Date"

# Create column for dates in standard format for R
prime_clean = prime_clean |> mutate(date_std = str_replace_all(Date, "\\[.*", ""))

# Create a column for date in MDY format
prime_clean = prime_clean |>
  mutate(DateMDY = mdy(date_std))

# Remove % from prime rates and change to numeric format, result in new column
prime_clean = prime_clean |>
  mutate(PrimeRate = as.numeric(str_replace_all(Rate, "%","")))

# Save prime_clean in case needed
#save(prime_clean, file = "prime_clean.RData")

# Continue to process the prime rate data so that an average by year can be calculated

# Only keep needed columns
PrimeRatesClean = select(prime_clean, DateMDY, PrimeRate)

ggplot(PrimeRatesClean, aes(x=DateMDY, y=PrimeRate)) + geom_point() + theme_light() +
  labs(title = "Prime Lending Rate", x = "Date", y = "Prime Interest Rate")

# Extract Year and Day of Year
PrimeRatesClean = PrimeRatesClean |>
  mutate(Year = year(DateMDY))

PrimeRatesClean = PrimeRatesClean |>
  mutate(Day = yday(DateMDY))

# Create vector of first and last days of year from 2012 to 2022
zerorate = seq(0,0, length.out = 22)
Years = c(seq(2012, 2022, 1),seq(2012, 2022, 1))
Day = c(seq(1,1,length.out=11), seq(365,365,length.out=11))

# Put together day and year to make a vector of dates of first and last day of year, format as mdy
DateTest = as.Date(ifelse(Day==1, 0, Day), format = "%j", origin = gsub(" ","",paste("1.1.",Years)))
DateTest = c(gsub(" ","",paste("1.1.",Years[1:11])),gsub(" ","",paste("12.31.",Years[12:22])))
DateTest = mdy(DateTest)

# Create data frame of first and last days of year, use 0 for prime rate
PRYear = data.frame(DateMDY = DateTest, PrimeRate = zerorate, Year = Years, Day)

# bind together the prime rates data and the first and last day of month data
AvePrimeRates = rbind(PrimeRatesClean, PRYear)
AvePrimeRates = arrange(AvePrimeRates, Year, Day)

# Replace 0 rates with most recent rate
AvePrimeRates$PrimeRate = ifelse(AvePrimeRates$PrimeRate == 0, lag(AvePrimeRates$PrimeRate), AvePrimeRates$PrimeRate)

AvePrimeRates$PrimeRate = ifelse(AvePrimeRates$PrimeRate == 0, 3.25, AvePrimeRates$PrimeRate)
AvePrimeRates$PrimeRate

AvePR = filter(AvePrimeRates, AvePrimeRates$Year>2010 & AvePrimeRates$Year<2023)
AvePR = filter(AvePR, Day != 365)   # Removed end of year, not needed for simple average

# Summarize by year, calculate an average
AvePRbyYear = AvePR |>
  group_by(Year) |>
  summarise(MeanPR = mean(PrimeRate))

save(AvePRbyYear, file = "AvePRbyYear.RData")

#Attach average prime rate as a column to the data frame by year.

# Create a column for the prime rate interest rate
clean_data_PR = clean_data |>
  mutate(AvePR = 0)

# Look up the average prime interest rate for each year in the data set
for (i in seq(nrow(clean_data_PR))){
  index = clean_data_PR$YEAR[i]
  rate = AvePRbyYear$MeanPR[AvePRbyYear$Year==index]
  clean_data_PR$AvePR[i] = rate
}

#Saving the data
save(clean_data_PR, file =  "Cleaned Data/clean_data_PR.RData")

