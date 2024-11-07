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

#Immigration Data (Inflow Data)
#2019/2020
data2=read_csv("G:/My Drive/In Course Project/countyinflow1920.csv")
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
data3=read_csv("G:/My Drive/In Course Project/countyinflow1819.csv")
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
data4=read_csv("G:/My Drive/In Course Project/countyinflow1718.csv")
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
data5=read_csv("G:/My Drive/In Course Project/countyinflow1617.csv")
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
data6=read_csv("G:/My Drive/In Course Project/countyinflow1516.csv")
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
data7=read_csv("G:/My Drive/In Course Project/countyinflow1415.csv")
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
data8=read_csv("G:/My Drive/In Course Project/countyinflow1314.csv")
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
data9=read_csv("G:/My Drive/In Course Project/countyinflow1213.csv")
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
new_data_1=read_csv("G:/My Drive/In Course Project/UADAggs_county_v3_2.csv")

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
write.csv(clean_data, "clean_data.csv", row.names = FALSE) 
