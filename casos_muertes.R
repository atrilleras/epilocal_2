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
source("rt_unified/functions.R")

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
options(mc.cores=parallel::detectCores())# esta instrucción permite que se ejecuten cadenas en paralelo y no en secuencia

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

###Atlántico
base1 <- cases%>%filter(departamento_o_distrito=="atlantico")
place_cases <- fCleanData(datos = cases,  place = "atlantico", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
atlantico <- left_join(casos,death_place, by = c("date"))
atlantico <- atlantico %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
atlantico$Deaths[is.na(atlantico$Deaths)] <- 0
atlantico <- data.frame(atlantico,Country="Atlantico")

###Nariño
base1 <- cases%>%filter(departamento_o_distrito=="narino")
place_cases <- fCleanData(datos = cases,  place = "narino", type = "depto")
death_place <- get_deaths(place_cases)
casos <- numero_casos(base1)
narino <- left_join(casos,death_place, by = c("date"))
narino <- narino %>% select(date,total,deaths)%>%rename(DateRep="date",Cases="total",Deaths="deaths")
narino$Deaths[is.na(narino$Deaths)] <- 0
narino <- data.frame(narino,Country="Narino")

d <- bind_rows(bogota,Cali,medellin,barranquilla,cartagena,cundinamarca,Valle,antioquia,bolivar,atlantico,narino)
d <- saveRDS(d,"d.R") 


