
library(tidyverse)
library(readr)
library(sf)
library(ggplot2)

# #################################
Первая домашняя работа

library(readr)
library(readr)
library(readr)
citytrees2 <- read_delim("citytrees2.csv", 
                         ";", escape_double = FALSE, col_types = cols(Tno = col_double(), 
                                                                      HR = col_character(), `% Variation` = col_double()), 
                         locale = locale(decimal_mark = ","), 
                         trim_ws = TRUE)
View(citytrees2)

plot(citytrees2$`Ht (m)`, citytrees2$`Crown Diameter (m)`)
plot(citytrees2$`Ht (m)`,citytrees2$`Crown Diameter (m)`, xlab = "Высота", ylab = "Диаметр кроны")


ggplot(test2, aes(x = `Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()

Вторая домашняя работа
# #################################
# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне
# #########################################

Все переменные имеют корректный тип данных

Повторяющиеся переменные убраны

dbh mm
HR

citytrees2 = citytrees2 %>% select(-`dbh (mm)`, -HR)

Из имен переменных убраны размерности

citytrees2 = citytrees2 %>% rename(dbh = `dbh (m)`)
citytrees2 = citytrees2 %>% rename(Ht = `Ht (m)`)
citytrees2 = citytrees2 %>% rename(Clearance_Ht = `Clearance Ht (m)`)
citytrees2 = citytrees2 %>% rename(Crown_Depth = `Crown Depth (m)`)
citytrees2 = citytrees2 %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
citytrees2 = citytrees2 %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
citytrees2 = citytrees2 %>% rename(Crown_Diameter = `Crown Diameter (m)`)
citytrees2 = citytrees2 %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
citytrees2 = citytrees2 %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
citytrees2 = citytrees2 %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
citytrees2 = citytrees2 %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)

Всем переменам заданы их реальные размерности
library(units)

units(citytrees2$dbh) = as_units("m")
units(citytrees2$Ht) = as_units("m")
units(citytrees2$Clearance_Ht) = as_units("m")
units(citytrees2$Crown_Depth) = as_units("m")
units(citytrees2$Average_Radial_Crown_spread) = as_units("m")
units(citytrees2$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(citytrees2$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(citytrees2$Crown_Diameter) = as_units("m")
units(citytrees2$Stem_diameter_Jan_2017) = as_units("mm")
units(citytrees2$Two_yr_dia_gain) = as_units("mm")
units(citytrees2$Annual_Girth_Increment) = as_units("mm")
units(citytrees2$`Predicted crown diamet using combined formulla`) = as_units("m")
units(citytrees2$`Predicted Crown Diameter`) = as_units("m")


citytrees2 %>% as.data.frame()


Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

citytrees2 = citytrees2 %>% mutate(error = `Predicted crown diamet using combined formulla` - Crown_Diameter)

citytrees2$error

citytrees2 = citytrees2 %>% rename(Crown_Diameter_Using_Combined_Formulla_Error = Crown_Diameter_Error)

citytrees2 = citytrees2 %>% mutate(Crown_Diameter_Error = `Predicted Crown Diameter` - Crown_Diameter)

citytrees2 = citytrees2 %>% select(-Difference, Diference)

Категориальные переменные должны быть факторами

library(forcats)

names(citytrees2)

citytrees2$`Age Index 1=Y 2=SM 3=EM 4=M`
citytrees2 = citytrees2 %>% mutate(`Age Index 1=Y 2=SM 3=EM 4=M` = as.numeric(`Age Index 1=Y 2=SM 3=EM 4=M`))

citytrees2 = citytrees2 %>%
  mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>%
  mutate(AgeIndex = fct_recode(AgeIndex,Y = "1", SM = "2",EM = "3", M = "4"))

citytrees2$AgeIndex[citytrees2$AgeIndex == "<NA>"]

citytrees2$AgeIndex

citytrees2$`Data Set      1=Norwich                0= Peterborough`
citytrees2 = citytrees2 %>% 
  mutate(DataSet = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(DataSet = fct_recode(DataSet, Norwich = "1", Peterborough = "0"))

citytrees2$DataSet

citytrees2$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`
citytrees2 = citytrees2 %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)) %>%
  mutate(PruningIndex = fct_recode(PruningIndex,`pruned within 5yrs` = "5", `pruned between 5 and 10yrs` = "10"))

citytrees2$PruningIndex
citytrees2$`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`
citytrees2 = citytrees2 %>%
  mutate(TypeOfPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
  mutate(TypeOfPruning = fct_recode(TypeOfPruning,None = "0", CR = "1", Other = "2", Both = "3"))
citytrees2$TypeOfPruning

citytrees2$`Soil Code 1=sand and gravel 2= Clay 3=silt`
citytrees2 = citytrees2 %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(SoilCode = fct_recode(SoilCode,`Sand and Gravel` = "1", Clay = "2", Slit = "3"))
citytrees2$SoilCode


library(tidyverse)

citytrees2$SoilCode

citytrees2$SoilCode %>% as.integer()

citytrees2 = citytrees2 %>% rename(geology = `Superfical Geology From British Geological Survey Geology of Britain Viewer`)
citytrees2$geology

citytrees2 = citytrees2 %>% 
  mutate(is_river = geology %>% str_detect("River"))
mutate(Soil= case_when(
  is_river & SoilCode == "Sand and Gravel" ~ "River Sand and Gravel",
  is_river & SoilCode == "Clay" ~ "River Clay",
  is_river & SoilCode == "Silt" ~ "River Silt",
  TRUE ~ as.character(Soil)
) )

citytrees2$is_river

Виды должны быть переименованы на латыне
# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis

citytrees2$Species
citytrees2$Species[citytrees2$Species == "Oak"] = "Quercus robur"
citytrees2$Species[citytrees2$Species == "Norway maple"] = "Acer platanoides"
citytrees2$Species[citytrees2$Species == "Norway Maple"] = "Acer platanoides"
citytrees2$Species[citytrees2$Species == "Silver Birch"] = "Betula pendula"
citytrees2$Species[citytrees2$Species == "Sycamore"] = "Platanus occidentalis"

Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84

library(stringr)
citytrees2$`Grid Reference`
coord = str_replace_all(citytrees2$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E +600000,
  quadr == "TG" ~ E +700000,
  quadr == "TL" ~ E +600000,
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N +300000,
  quadr == "TG" ~ N +300000,
  quadr == "TL" ~ N +200000,
))

table_c = na.exclude(table_c)


library(sf)

table_WGS = 
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head

citytrees2$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)

citytrees2

