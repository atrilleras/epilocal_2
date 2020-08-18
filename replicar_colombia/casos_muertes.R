library(epitrix)
library(EpiEstim)
library(tidyverse)
library(stringi)
library(incidence)
library(reshape2)
library(distcrete)
library(projections)
library(cowplot)
library(openxlsx)
rm(list=ls())
source("funs_z.R")
source("functions.R")

###Funciones
numero_casos <- function(base1){
    datos_inc <- base1 %>%
        mutate(date = as.Date (fis),
               type=case_when(tipo=="Importado"~"imported",
                              tipo=="Relacionado"~"local",
                              tipo=="En estudio"~"local"))
    datos_inc$fis_clean <- clean_labels(datos_inc$fis)
    datos_inc$asymptomatic <- 0
    datos_inc$asymptomatic[datos_inc$fis_clean ==  "asintomatico"] <- 1
    datos_1 <- datos_inc %>% filter(asymptomatic == 0)
    #rezago <- 14
    inc_multiple  <- incidence(datos_1$date, groups = datos_1$type) 
    casos <- as.data.frame(inc_multiple$counts)
    fechas <- as.data.frame(inc_multiple$dates)
    base_incidencia <- cbind(casos,fechas)
    base_incidencia <- base_incidencia %>% select("inc_multiple$dates","imported","local")
    base_incidencia <- base_incidencia %>% mutate(total = imported +local)%>%rename(date="inc_multiple$dates")
    return(base_incidencia)
}

#____ DATASET

cases <- read_csv("data/Casos_positivos_de_COVID-19_03_08_20.csv")
#datos_1 <- read_csv("data/Casos_positivos_de_COVID-19_02_08_20_E.csv")
names(cases) <- epitrix::clean_labels(names(cases))
cases$departamento_o_distrito <- epitrix::clean_labels(cases$departamento_o_distrito)
cases$ciudad_de_ubicacion <- epitrix::clean_labels(cases$ciudad_de_ubicacion)
unique(cases$departamento_o_distrito)
options(mc.cores=parallel::detectCores())# esta instrucci?n permite que se ejecuten cadenas en paralelo y no en secuencia

###Bogota
base1 <- cases%>%filter(departamento_o_distrito=="bogota_d_c")
place_cases <- fCleanData(datos = cases,  place = "bogota_d_c", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
bogota <- left_join(casos,death_place, by = c("date"))
bogota <- bogota %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
bogota$Deaths[is.na(bogota$Deaths)] <- 0
bogota <- data.frame(bogota,Country="Bogota")

###Cali
base1        <- cases%>%filter(ciudad_de_ubicacion=="cali")
place_cases <- fCleanData(datos = cases,  place = "cali", type = "ciudad")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
Cali <- left_join(casos,death_place, by = c("date"))
Cali <- Cali %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
Cali$Deaths[is.na(Cali$Deaths)] <- 0
Cali <- data.frame(Cali,Country="Cali")

###Medellin
datos        <- cases%>%filter(ciudad_de_ubicacion=="medellin")
place_cases <- fCleanData(datos = cases,  place = "medellin", type = "ciudad")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
medellin <- left_join(casos,death_place, by = c("date"))
medellin <- medellin %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
medellin$Deaths[is.na(medellin$Deaths)] <- 0
medellin <- data.frame(medellin,Country="Medellin")

###Barranquilla
datos       <- cases%>%filter(departamento_o_distrito=="barranquilla_d_e")
place_cases <- fCleanData(datos = cases,  place = "barranquilla_d_e", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
barranquilla <- left_join(casos,death_place, by = c("date"))
barranquilla <- barranquilla %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
barranquilla$Deaths[is.na(barranquilla$Deaths)] <- 0
barranquilla <- data.frame(barranquilla,Country="Barranquilla")

### Cartagena
datos <- cases%>%filter(departamento_o_distrito=="cartagena_d_t_y_c")
place_cases <- fCleanData(datos = cases,  place = "cartagena_d_t_y_c", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
cartagena <- left_join(casos,death_place, by = c("date"))
cartagena <- cartagena %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
cartagena$Deaths[is.na(cartagena$Deaths)] <- 0
cartagena <- data.frame(cartagena,Country="Cartagena")

###Cundinamarca
base1 <- cases%>%filter(departamento_o_distrito=="cundinamarca")
place_cases <- fCleanData(datos = cases,  place = "cundinamarca", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
cundinamarca <- left_join(casos,death_place, by = c("date"))
cundinamarca <- cundinamarca %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
cundinamarca$Deaths[is.na(cundinamarca$Deaths)] <- 0
cundinamarca <- data.frame(cundinamarca,Country="Cundinamarca")


###Valle
valle_1 <- cases%>%filter(departamento_o_distrito=="valle_del_cauca")
valle_2 <- cases%>%filter(departamento_o_distrito=="buenaventura_d_e")
base1 <- bind_rows(valle_1,valle_2)
place_cases <- fCleanData_2(base1)
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
Valle <- left_join(casos,death_place, by = c("date"))
Valle <- Valle %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
Valle$Deaths[is.na(Valle$Deaths)] <- 0
Valle <- data.frame(Valle,Country="Valle")


## Antioquia
base1 <- cases%>%filter(departamento_o_distrito=="antioquia")
place_cases <- fCleanData(datos = cases,  place = "antioquia", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
antioquia <- left_join(casos,death_place, by = c("date"))
antioquia <- antioquia %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
antioquia$Deaths[is.na(antioquia$Deaths)] <- 0
antioquia <- data.frame(antioquia,Country="Antioquia")

###Bolivar
bolivar_1 <- cases%>%filter(departamento_o_distrito=="bolivar")
bolivar_2 <- cases%>%filter(departamento_o_distrito=="cartagena_d_t_y_c")
base1 <-bind_rows(bolivar_1,bolivar_2)
place_cases <- fCleanData_2(base1)
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
bolivar <- left_join(casos,death_place, by = c("date"))
bolivar <- bolivar %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
bolivar$Deaths[is.na(bolivar$Deaths)] <- 0
bolivar <- data.frame(bolivar,Country="Bolivar")

###Atl?ntico
base1 <- cases%>%filter(departamento_o_distrito=="atlantico")
place_cases <- fCleanData(datos = cases,  place = "atlantico", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
atlantico <- left_join(casos,death_place, by = c("date"))
atlantico <- atlantico %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
atlantico$Deaths[is.na(atlantico$Deaths)] <- 0
atlantico <- data.frame(atlantico,Country="Atlantico")

###Nari?o
base1 <- cases%>%filter(departamento_o_distrito=="narino")
place_cases <- fCleanData(datos = cases,  place = "narino", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
narino <- left_join(casos,death_place, by = c("date"))
narino <- narino %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
narino$Deaths[is.na(narino$Deaths)] <- 0
narino <- data.frame(narino,Country="Narino")


###Boyacá
base1 <- cases%>%filter(departamento_o_distrito=="boyaca")
place_cases <- fCleanData(datos = cases,  place = "boyaca", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
boyaca <- left_join(casos,death_place, by = c("date"))
boyaca <- boyaca %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
boyaca$Deaths[is.na(boyaca$Deaths)] <- 0
boyaca <- data.frame(boyaca,Country="Boyaca")


###Caquetá
base1 <- cases%>%filter(departamento_o_distrito=="caqueta")
place_cases <- fCleanData(datos = cases,  place = "caqueta", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
caqueta <- left_join(casos,death_place, by = c("date"))
caqueta <- caqueta %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
caqueta$Deaths[is.na(caqueta$Deaths)] <- 0
caqueta <- data.frame(caqueta,Country="Caqueta")


###Cauca
base1 <- cases%>%filter(departamento_o_distrito=="cauca")
place_cases <- fCleanData(datos = cases,  place = "cauca", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
cauca <- left_join(casos,death_place, by = c("date"))
cauca <- cauca %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
cauca$Deaths[is.na(cauca$Deaths)] <- 0
cauca <- data.frame(cauca,Country="Cauca")

###Cesar
base1 <- cases%>%filter(departamento_o_distrito=="cesar")
place_cases <- fCleanData(datos = cases,  place = "cesar", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
cesar <- left_join(casos,death_place, by = c("date"))
cesar <- cesar %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
cesar$Deaths[is.na(cesar$Deaths)] <- 0
cesar <- data.frame(cesar,Country="cesar")

###Chocó
base1 <- cases%>%filter(departamento_o_distrito=="choco")
place_cases <- fCleanData(datos = cases,  place = "choco", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
choco <- left_join(casos,death_place, by = c("date"))
choco <- choco %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
choco$Deaths[is.na(choco$Deaths)] <- 0
choco <- data.frame(choco,Country="Choco")

###Huila
base1 <- cases%>%filter(departamento_o_distrito=="huila")
place_cases <- fCleanData(datos = cases,  place = "huila", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
huila <- left_join(casos,death_place, by = c("date"))
huila <- huila %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
huila$Deaths[is.na(huila$Deaths)] <- 0
huila <- data.frame(huila,Country="Huila")


##La Guajira
base1 <- cases%>%filter(departamento_o_distrito=="la_guajira")
place_cases <- fCleanData(datos = cases,  place = "la_guajira", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
la_guajira <- left_join(casos,death_place, by = c("date"))
la_guajira <- la_guajira %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
la_guajira$Deaths[is.na(la_guajira$Deaths)] <- 0
la_guajira <- data.frame(la_guajira,Country="la_guajira")


##Meta
base1 <- cases%>%filter(departamento_o_distrito=="meta")
place_cases <- fCleanData(datos = cases,  place = "meta", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
meta <- left_join(casos,death_place, by = c("date"))
meta <- meta %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
meta$Deaths[is.na(meta$Deaths)] <- 0
meta <- data.frame(meta,Country="meta")


##Norte de Santander
base1 <- cases%>%filter(departamento_o_distrito=="norte_de_santander")
place_cases <- fCleanData(datos = cases,  place = "norte_de_santander", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
norte_de_santander <- left_join(casos,death_place, by = c("date"))
norte_de_santander <- norte_de_santander %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
norte_de_santander$Deaths[is.na(norte_de_santander$Deaths)] <- 0
norte_de_santander <- data.frame(norte_de_santander,Country="Norte_de_santander")


##Putumayo
base1 <- cases%>%filter(departamento_o_distrito=="putumayo")
place_cases <- fCleanData(datos = cases,  place = "putumayo", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
putumayo <- left_join(casos,death_place, by = c("date"))
putumayo <- putumayo %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
putumayo$Deaths[is.na(putumayo$Deaths)] <- 0
putumayo <- data.frame(putumayo,Country="Putumayo")


##Risaralda
base1 <- cases%>%filter(departamento_o_distrito=="risaralda")
place_cases <- fCleanData(datos = cases,  place = "risaralda", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
risaralda <- left_join(casos,death_place, by = c("date"))
risaralda <- risaralda %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
risaralda$Deaths[is.na(risaralda$Deaths)] <- 0
risaralda <- data.frame(risaralda,Country="risaralda")


##Santander
base1 <- cases%>%filter(departamento_o_distrito=="santander")
place_cases <- fCleanData(datos = cases,  place = "santander", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
santander <- left_join(casos,death_place, by = c("date"))
santander <- santander %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
santander$Deaths[is.na(santander$Deaths)] <- 0
santander <- data.frame(santander,Country="santander")


##Tolima
base1 <- cases%>%filter(departamento_o_distrito=="tolima")
place_cases <- fCleanData(datos = cases,  place = "tolima", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
tolima <- left_join(casos,death_place, by = c("date"))
tolima <- tolima %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
tolima$Deaths[is.na(tolima$Deaths)] <- 0
tolima <- data.frame(tolima,Country="Tolima")


###Magdalena
magdalena_1 <- cases%>%filter(departamento_o_distrito=="magdalena")
magdalena_2 <- cases%>%filter(departamento_o_distrito=="santa_marta_d_t_y_c")
datos <-bind_rows(magdalena_1,magdalena_2)
place_cases <- fCleanData_2(datos)
death_place <- get_deaths(place_cases)
casos <- numero_casos(datos)
magdalena <- left_join(casos,death_place, by = c("date"))
magdalena <- magdalena %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
magdalena$Deaths[is.na(magdalena$Deaths)] <- 0
magdalena <- data.frame(magdalena,Country="Magdalena")


# ## Córdoba
# base1 <- cases%>%filter(departamento_o_distrito=="cordoba")
# place_cases <- fCleanData(datos = cases,  place = "cordoba", type = "depto")
# death_place <- get_deaths(place_cases)
# death_place <- get_deaths(place_cases)
# casos <- numero_casos(base1)
# cordoba <- left_join(casos,death_place, by = c("date"))
# cordoba <- tolima %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
# cordoba$Deaths[is.na(cordoba$Deaths)] <- 0
# cordoba <- data.frame(cordoba,Country="cordoba")
# 
# 
# ## Sucre
# base1 <- cases%>%filter(departamento_o_distrito=="sucre")
# place_cases <- fCleanData(datos = cases,  place = "sucre", type = "depto")
# death_place <- get_deaths(place_cases)
# death_place <- get_deaths(place_cases)
# casos <- numero_casos(base1)
# sucre <- left_join(casos,death_place, by = c("date"))
# sucre <- sucre %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
# sucre$Deaths[is.na(sucre$Deaths)] <- 0
# sucre <- data.frame(sucre,Country="sucre")

d <- bind_rows(bogota,Cali,medellin,barranquilla,cartagena,cundinamarca,Valle,antioquia,bolivar,atlantico,narino)

d_2 <- bind_rows(boyaca,caqueta,cauca,cesar,choco,huila,la_guajira,magdalena,meta,norte_de_santander,putumayo,risaralda,santander,tolima)


#####crear tablas en Excel
wb <- createWorkbook(
  creator = "Me",
  title = "title here",
  subject = "this & that",
  category = "something"
)
addWorksheet(wb, "casos")
writeData(wb,sheet="casos",d_2)
saveWorkbook(wb, "casos_y_muertes_2.xlsx", overwrite = TRUE)


