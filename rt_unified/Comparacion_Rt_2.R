library (RCurl)
library(squire)
library(patchwork)
library(ggplot2)
library(dplyr)
library(epitrix)
library(readr)
library(incidence)
library(readxl)
library(EpiEstim)
library(openxlsx)
library(gridExtra)
library(lubridate)
library(stringr)
library(epidemia)
library(gridExtra)
library(cowplot)
rm(list=ls())
source("funs_z.R")
source("rt_unified/functions.R")
#____ DATASET

cases <- read.xlsx("data/Casos_positivos_de_COVID-19_02_08_20_E.xlsx")
#datos_1 <- read_csv("data/Casos_positivos_de_COVID-19_02_08_20_E.csv")
names(cases) <- epitrix::clean_labels(names(cases))
cases$departamento_o_distrito <- epitrix::clean_labels(cases$departamento_o_distrito)
cases$ciudad_de_ubicacion <- epitrix::clean_labels(cases$ciudad_de_ubicacion)
unique(cases$departamento_o_distrito)
options(mc.cores=parallel::detectCores())# esta instrucción permite que se ejecuten cadenas en paralelo y no en secuencia

##Seleccionar lugar y tipo
#bogota_d_c
datos <- cases%>%filter(departamento_o_distrito=="bogota_d_c")
place_cases <- fCleanData(datos = cases,  place = "bogota_d_c", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim_2(datos)
complete_dates<-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
                           deaths=c(0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Bogotá",pop=8380801)
plot_epidemia <- plot_rt(fit)
rt_epiestim_2 <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data

base_bogota <- readRDS("base_bogota.RDS")
rt_epiestim <- base_bogota [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_bogota_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_bogota_2,"base_bogota_2.RDS")

base_bogota_2 <- readRDS("base_bogota_2.RDS")
rt_epiestim <- base_bogota_2 [[1]]# numeroEfectivo
rt_epidemia <- base_bogota_2 [[2]] #data_boyaca
rt_epiestim_2 <- base_bogota_2 [[3]]
# rm(base_bogota)

rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
bogota_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())

#bogota <- plot_rt_comp(rtu)
saveRDS(bogota_2,"bogota_2.RDS")

#bogota_infeciones <-plot_infections(fit)
#bogota_infeciones$data
### Barranquilla
datos       <- cases%>%filter(departamento_o_distrito=="barranquilla_d_e")
place_cases <- fCleanData(datos = cases,  place = "barranquilla_d_e", type = "depto")
death_place <- get_deaths(place_cases)
data_rt     <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-07","2020-03-08","2020-03-11","2020-03-31","2020-04-08"),
                            deaths=c(0, 0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Barranquilla",pop=1274250)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_barranquilla <- readRDS("base_barranquilla.RDS")
rt_epiestim <- base_barranquilla [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_barranquilla_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_barranquilla_2,"base_barranquilla_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
barranquilla_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(barranquilla_2,"baranquilla_2.RDS")


### Atlántico
atlantico_1      <- cases%>%filter(departamento_o_distrito=="atlantico")
atlantico_2      <- cases%>%filter(departamento_o_distrito=="barranquilla_d_e")
datos            <- bind_rows(atlantico_1,atlantico_2)
place_cases      <- fCleanData_2(datos)
death_place      <- get_deaths(place_cases)
data_rt          <- get_rt_epiestim(datos)
###################################
# complete_dates <-data.frame(date=c("2020-03-04","2020-03-05","2020-03-06","2020-03-07","2020-03-08",
#                                "2020-03-09","2020-03-10","2020-03-11","2020-03-12","2020-03-14",
#                                "2020-03-18","2020-03-23","2020-03-25","2020-03-26","2020-03-29",
#                                "2020-03-30","2020-04-09","2020-04-11"),
#                         deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# datos_epidemia <- rbind(death_place,complete_dates)
# datos_epidemia <-datos_epidemia %>% arrange(date)
complete_dates <-data.frame(date=c("2020-03-04","2020-03-05","2020-03-07","2020-03-08",
                                   "2020-03-11"),deaths=c(0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Atlantico",pop=2272128)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_atlantico <- readRDS("base_atlantico.RDS")
rt_epiestim <- base_atlantico [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_atlantico_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_atlantico_2,"base_atlantico_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
atlantico_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(atlantico_2,"atlantico_2.RDS")

###Cartagena
datos <- cases%>%filter(departamento_o_distrito=="cartagena_d_t_y_c")
place_cases <- fCleanData(datos = cases,  place = "cartagena_d_t_y_c", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-03","2020-03-04","2020-03-05","2020-03-07",
                                   "2020-03-08","2020-03-09","2020-03-10"),
                            deaths=c(0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Cartagena",pop=950000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_cartagena <- readRDS("base_cartagena.RDS")
rt_epiestim <- base_cartagena [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_cartagena_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_cartagena_2,"base_cartagena_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
cartagena_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(cartagena_2,"cartagena_2.RDS")

###Bolivar
bolivar_1 <- cases%>%filter(departamento_o_distrito=="bolivar")
bolivar_2 <- cases%>%filter(departamento_o_distrito=="cartagena_d_t_y_c")
datos <-bind_rows(bolivar_1,bolivar_2)
place_cases <- fCleanData_2(datos)
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
# complete_dates <-data.frame(date=c("2020-03-13","2020-03-14","2020-03-16","2020-03-18","2020-03-20",
#                                 "2020-03-22","2020-03-23","2020-03-24","2020-03-25","2020-03-27",
#                                 "2020-03-28","2020-03-29","2020-03-31","2020-04-01","2020-04-02",
#                                 "2020-04-04","2020-04-06","2020-04-11","2020-04-12",
#                                 "2020-04-14","2020-04-15","2020-04-18","2020-04-21",
#                                 "2020-05-01"),
#                          deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
# datos_epidemia <- rbind(death_place,complete_dates)
# datos_epidemia <-datos_epidemia %>% arrange(date)

complete_dates <-data.frame(date=c("2020-03-03","2020-03-04","2020-03-05","2020-03-07","2020-03-08",
                                   "2020-03-09","2020-03-10"),
                            deaths=c(0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Bolivar",pop=2130000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_bolivar <- readRDS("base_bolivar.RDS")
rt_epiestim <- base_bolivar [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_bolivar_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_bolivar_2,"base_bolivar_2.RDS")

rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
bolivar_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())

saveRDS(bolivar_2,"bolivar_2.RDS")

###choco
datos <- cases%>%filter(departamento_o_distrito=="choco")
place_cases <- fCleanData(datos = cases,  place = "choco", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-24","2020-03-25","2020-03-26","2020-03-27",
                                   "2020-03-28","2020-03-29","2020-03-30","2020-03-31",
                                   "2020-04-03","2020-04-07","2020-04-08","2020-04-10",
                                   "2020-04-11","2020-04-21","2020-04-22","2020-07-30",
                                   "2020-07-31"),
                            deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Choco",pop=549225)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_choco <- readRDS("base_choco.RDS")
rt_epiestim <- base_choco [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_choco_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_choco_2,"base_choco_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
choco_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())

saveRDS(choco_2,"choco_2.RDS")

###Amazonas
datos <- cases%>%filter(departamento_o_distrito=="amazonas")
place_cases <- fCleanData(datos = cases,  place = "amazonas", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-15","2020-03-16","2020-03-17","2020-03-18","2020-03-19",
                                   "2020-03-20","2020-03-21","2020-03-22","2020-03-23","2020-03-24",
                                   "2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29",
                                   "2020-03-30","2020-04-01","2020-04-02","2020-04-03","2020-04-04",
                                   "2020-04-06","2020-04-08","2020-06-27"),
                            deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Amazonas",pop=80464)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_amazonas <- readRDS("base_amazonas.RDS")
rt_epiestim <- base_amazonas [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_amazonas_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_amazonas_2,"base_amazonas_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
amazonas_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())

saveRDS(amazonas_2,"amazonas_2.RDS")

###Cali
datos <- cases%>%filter(ciudad_de_ubicacion=="cali")
place_cases <- fCleanData(datos = cases,  place = "cali", type = "ciudad")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-03","2020-03-04","2020-03-06","2020-03-08"),
                            deaths=c(0, 0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Cali",pop=2252616)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_cali <- readRDS("base_cali.RDS")
rt_epiestim <- base_cali [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_cali_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_cali_2,"base_cali_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
cali_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(cali_2,"cali_2.RDS")

#####Valle del Cauca

valle_1 <- cases%>%filter(departamento_o_distrito=="valle_del_cauca")
valle_2 <- cases%>%filter(departamento_o_distrito=="buenaventura_d_e")
datos <- bind_rows(valle_1,valle_2)
place_cases <- fCleanData_2(datos)
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-03","2020-03-06"),
                            deaths=c(0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="valle",pop=4476000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_valle <- readRDS("base_valle.RDS")
rt_epiestim <- base_valle [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_valle_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_valle_2,"base_valle_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
valle_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(valle_2,"valle_2.RDS")

###Medellin
datos <- cases%>%filter(ciudad_de_ubicacion=="medellin")
place_cases <- fCleanData(datos = cases,  place = "medellin", type = "ciudad")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-01","2020-03-03","2020-03-04","2020-03-07","2020-04-30"),
                            deaths=c(0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Medellin",pop=2500000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_medellin <- readRDS("base_medellin.RDS")
rt_epiestim <- base_medellin [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_medellin_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_medellin_2,"base_medellin_2.RDS")

rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
medellin_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
saveRDS(medellin_2,"medellin_2.RDS")

## Antioquia
datos <- cases%>%filter(departamento_o_distrito=="antioquia")
place_cases <- fCleanData(datos = cases,  place = "antioquia", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-01","2020-03-03","2020-03-04"),
                            deaths=c(0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Antioquia",pop=6407000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_antioquia <- readRDS("base_antioquia.RDS")
rt_epiestim <- base_antioquia [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_antioquia_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_antioquia_2,"base_antioquia_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
antioquia_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())

saveRDS(antioquia_2,"antioquia_2.RDS")

##Meta
datos <- cases%>%filter(departamento_o_distrito=="meta")
place_cases <- fCleanData(datos = cases,  place = "meta", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-07","2020-03-08","2020-03-10","2020-03-11","2020-03-16",
                                   "2020-03-18","2020-03-23","2020-03-25","2020-03-30","2020-04-26",
                                   "2020-04-28","2020-05-04","2020-05-09","2020-05-12","2020-05-18",
                                   "2020-05-21","2020-05-26","2020-06-02"),
                            deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Meta",pop=1040000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_meta <- readRDS("base_meta.RDS")
rt_epiestim <- base_meta [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_meta_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_meta_2,"base_meta_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
meta_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(meta_2,"meta_2.RDS")

## Boyacá
datos <- cases%>%filter(departamento_o_distrito=="boyaca")
place_cases <- fCleanData(datos = cases,  place = "boyaca", type = "depto")
death_place <- get_deaths(place_cases)
data_rt <- get_rt_epiestim(datos)
###################################
complete_dates <-data.frame(date=c("2020-03-14","2020-03-15","2020-03-16","2020-03-17","2020-03-19",
                                   "2020-03-24","2020-03-26","2020-03-31","2020-04-03","2020-04-05",
                                   "2020-04-06","2020-04-07","2020-04-08","2020-04-09","2020-04-11",
                                   "2020-04-12","2020-04-13","2020-04-14","2020-04-15","2020-04-16",
                                   "2020-04-17","2020-04-18","2020-04-24","2020-04-26","2020-06-01",
                                   "2020-07-26","2020-07-27"),
                            deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)

####################################
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Meta",pop=1217000)
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
base_boyaca <- readRDS("base_boyaca.RDS")
rt_epiestim <- base_boyaca [[1]]# numeroEfectivo
#rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

base_boyaca_2 <- list(rt_epiestim,rt_epidemia,rt_epiestim_2)
saveRDS(base_boyaca_2,"base_boyaca_2.RDS")


rtu <- get_rt_unified_2(rt_epiestim,rt_epiestim_2, rt_epidemia)
boyaca_2 <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025_2, ymax = epiestim_q0975_2, fill = method_epiestim_2), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = '#7997FF') +
    geom_line(aes(x = date, y = epiestim_mean_2), colour = '#FF5C00') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue","orange")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
#bogota <- plot_rt_comp(rtu)
saveRDS(boyaca_2,"boyaca_2.RDS")

