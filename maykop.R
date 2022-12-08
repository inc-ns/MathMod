
setwd("D:/Suslova_AS")

getwd()
rm(list=ls())

library(tidyverse)
library(lubridate)
library(rnoaa) 
#station_data = ghcnd_stations() 
#write.csv(station_data,"station_data.csv")
#Может занять несколько минут лучше 
#выполнить один раз в месте с хорошим интернетом и сохранить результат
station_data = read.csv("station_data.csv")
#После получения всписка всех станций, 
#получите список станций ближайших к столице вашего региона,
#создав таблицу с именем региона и координатами его столицы
maykop = data.frame(id = "maykop", latitude = 44.6078,  longitude = 40.1058)
maykop_around = meteo_nearby_stations(lat_lon_df = maykop, station_data = station_data,
                                    limit = 20, var = c("PRCP", "TAVG"),
                                    year_min = 2017, year_max = 2023)
class(maykop_around)
maykop_around=maykop_around [(20)]

maykop_id = maykop_around [1.1]
#tula_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Тулы, очевидно что первым элементом таблицы будет идентификатор метеостанции Тулы, его то мы и попытаемся получить
maykop_id = maykop_around[["maykop"]][["id"]][1]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_maykop_data = meteo_tidy_ghcnd (stationid = maykop_id)


all_maykop_data$prcp# Precipitation - осадки
all_maykop_data$snwd# Snow - высота зиимних осадков
all_maykop_data$tavg# Temperature air averaged - средняя температура воздуха за день
all_maykop_data$tmax# Temperature air max - максимальная температура за день
all_maykop_data$tmin# Temperature air min - минимальная температура за день


all_maykop_data = all_maykop_data %>% nutate(
  year = year(data), mounth=mounth(data), doy = yday(date)
)

all_maykop_data = all_maykop_data %>% filter(year <2023)

all_maykop_data = all_maykop_data %>% nutate(
  prcp = prcp /10 , snwd = snvd/10 , tavg= tavg /10 ,
  tmax = tmax/10 , tmin= tmin /10)


#active_temp = summarise, не активные температуры стали равны 0

