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
cases <- read_csv("data/Casos_positivos_de_COVID-19_03_08_20.csv")
#datos_1 <- read_csv("data/Casos_positivos_de_COVID-19_27_07_20.csv")
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
data_rt <- get_rt_epiestim(datos)
complete_dates<-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
                           deaths=c(0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
si <- as.vector(data_rt$RTo$si_distr)
fit <- fit_epidemia(datos_epidemia,country="Bogotá",pop=8380801)
plot_epidemia <- plot_rt(fit, date_breaks="1 week")
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data

base_bogota <- list(rt_epiestim,rt_epidemia)
saveRDS(base_bogota,"base_bogota.RDS")
# base_bogota <- readRDS("base_bogota.RDS")
# rt_epiestim <- base_bogota [[1]]# numeroEfectivo
# rt_epidemia <- base_bogota [[2]] #data_boyaca
# rm(base_bogota)

rtu <- get_rt_unified(rt_epiestim, rt_epidemia)
bogota_2W <- ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = 'blue') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    coord_cartesian(ylim=c(0,5))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue")) +
    labs (subtitle = paste0("( Número de muertes=", sum(death_place$deaths), ")"))+
    theme(legend.position = c(0.5, 0.8), legend.title = element_blank())

#bogota <- plot_rt_comp(rtu)
saveRDS(bogota_2W,"bogota_2W.RDS")