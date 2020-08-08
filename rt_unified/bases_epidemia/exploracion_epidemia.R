rm(list=ls())
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

source("funs_z.R")
source("rt_unified/functions.R")
#____ DATASET
cases <- read_csv("data/Casos_positivos_de_COVID-19_27_07_20.csv")
#datos_1 <- read_csv("data/Casos_positivos_de_COVID-19_27_07_20.csv")
names(cases) <- epitrix::clean_labels(names(cases))
cases$departamento_o_distrito <- epitrix::clean_labels(cases$departamento_o_distrito)
cases$ciudad_de_ubicacion <- epitrix::clean_labels(cases$ciudad_de_ubicacion)
unique(cases$departamento_o_distrito)
options(mc.cores=parallel::detectCores())# esta instrucción permite que se ejecuten cadenas en paralelo y no en secuencia

##Seleccionar lugar y tipo
#bogota_d_c
#atlantico
datos <- cases%>%filter(departamento_o_distrito=="bogota_d_c")
place_cases <- fCleanData(datos = cases,  place = "bogota_d_c", type = "depto")

##### Prueba para Bogotá
get_deaths <- function(place_cases)
{
new_cases <-
    place_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths <- 
    place_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases, new_deaths, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
deaths <- deaths[1:(NROW(deaths)-14),]
death_place <- deaths %>% select(date,deaths)
return(death_place)
}

death_place <- get_deaths(place_cases)

get_rt_epiestim <- function(datos)
{
datos$fecha_diagnostico <- as.Date(as.numeric(datos$fecha_diagnostico),
                                   origin = "1900-01-01")
datos_inc <- datos %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))
datos_inc$fis_clean <- clean_labels(datos_inc$fis)
datos_inc$asymptomatic <- 0
datos_inc$asymptomatic[datos_inc$fis_clean ==  "asintomatico"] <- 1
datos <- datos_inc %>% filter(asymptomatic == 0)
rezago <- 14
fechaConfiable <- as.Date(max(datos$fecha_de_notificacion),origin = "1900-01-01") - rezago
inc_multiple  <- incidence(datos$date, groups = datos$type)[1:(max(datos$date)-min(datos$date)- rezago)] 
days <- seq_along(inc_multiple$dates)[2:(length(inc_multiple$dates)-7)]
RTo  <- estimate_R(inc_multiple, method = "parametric_si",
                   config = make_config(list(
                       mean_si = 6.48, std_si = 3.83,
                       t_start=days, t_end=days+7)))
data_rt <- RTo$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple$dates) + RTo$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple$dates) + RTo$R$t_end 
rt <- list (data_rt = data_rt,
                        RTo = RTo)
return(rt)
}

data_rt <- get_rt_epiestim(datos)
# data_rt <- RTo$R
# data_rt$Fecha_Inicio_Ventana <- min(inc_multiple$dates) + RTo$R$t_start 
# data_rt$Fecha_Fin_Ventana    <- min(inc_multiple$dates) + RTo$R$t_end 
# 
# rt <- list (data_rt = data_rt,
#             RTo = RTo,
#             inc_multiple = inc_multiple,
#             fechaConfiable = fechaConfiable,
#             datos = datos,
#             datos_inc = datos_inc)
# numeroEfectivo <- rt$data_rt
# datos <- rt$datos
# fechaReporte <- Sys.Date()
# fechaCuarentena <- as.Date("2020-03-23")
# fechaApertura   <- as.Date("2020-04-27")
# fechaConfiable  <- rt$fechaConfiable
# fechaend <- Sys.Date() + 5
# date_start  <- as.Date("2020-03-09", origin = "1899-12-30")
# date_end   <-  as.Date(fechaend, origin = "1899-12-30")
# primer_caso_reportado <- min(datos$fecha_diagnostico, na.rm = T)
# size <- 15

### CORRER LIBRERÍA "epidemia"

###### Bogotá muertes
#incidence_bog <- deaths %>% select(date,deaths)
complete_dates<-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
                    deaths=c(0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date) 


###Atlantico

complete_dates <-data.frame(date=c("2020-03-04","2020-03-05","2020-03-06","2020-03-07","2020-03-08",
                               "2020-03-09","2020-03-10","2020-03-11","2020-03-12","2020-03-14",
                               "2020-03-18","2020-03-23","2020-03-25","2020-03-26","2020-03-29",
                               "2020-03-30","2020-04-09","2020-04-11"),
                        deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
datos_epidemia <- rbind(death_place,complete_dates)
datos_epidemia <-datos_epidemia %>% arrange(date)
# #######Bogotá casos####################
# incidencia_bog <- deaths%>% select(date,cases)
# inicio_2 <-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
#                    detahs=c(0, 0,0))
# incidencia_bog <- rbind(incidencia_bog,inicio_2)
# incidencia_bog <-incidencia_bog %>% arrange(date)

# ########################################
# eventos_publicos <- c(rep(0,14),rep(1,124))
# colegios_publicos <- c(rep(0,18),rep(1,120))
# cuarentena <- c(rep(0,22),rep(1,116))
# total <- nrow(incidence_bog)
# country <-c(rep("bogota",total))
# intervenciones <- data.frame(country,date=incidence_bog$date,eventos_publicos,colegios_publicos,cuarentena)
#obs_1 <- !is.na(incidence_bog$deaths)
si <- as.vector(data_rt$RTo$si_distr)



distribution_death <- as.numeric(c(0.0000013, 0.0000425, 0.0002286, 0.0007811, 0.0019284, 0.0038080, 0.0064709, 0.0099405, 0.0140458, 0.0186132,
                       0.0234308, 0.0281585, 0.0327993, 0.0367371, 0.0402502, 0.0431422, 0.0450086, 0.0463019, 0.0470288, 0.0466678,
                       0.0459662, 0.0444947, 0.0428173, 0.0408481, 0.0385647, 0.0359808, 0.0333518, 0.0308189, 0.0281390, 0.0255677,
                       0.0231206, 0.0207830, 0.0186044, 0.0165310, 0.0145487, 0.0130146, 0.0113813, 0.0099255, 0.0086952, 0.0075301,
                       0.0064709, 0.0056318, 0.0048089, 0.0041819, 0.0035421, 0.0030410, 0.0025729, 0.0022184, 0.0018534, 0.0015667,
                       0.0013203, 0.0011176, 0.0009250, 0.0007994, 0.0006565, 0.0005659, 0.0004530, 0.0003807, 0.0003245, 0.0002616,
                       0.0002165, 0.0001826, 0.0001466, 0.0001171, 0.0000991, 0.0000794, 0.0000713, 0.0000587, 0.0000491, 0.0000404,
                       0.0000334, 0.0000272, 0.0000238, 0.0000157, 0.0000149, 0.0000118, 0.0000088, 0.0000083, 0.0000068, 0.0000052,
                       0.0000047, 0.0000024, 0.0000025, 0.0000030, 0.0000016, 0.0000011, 0.0000015, 0.0000013, 0.0000007, 0.0000005,
                       0.0000004, 0.0000001, 0.0000001, 0.0000004, 0.0000003, 0.0000003, 0.0000002, 0.0000002, 0.0000002, 0.0000001,
                       0.0000001))
############# Análisis para Bogotá
fit_epidemia <- function (datos_epidemia,country=" ",pop=8380801){

args<- list(
    data = data.frame(country=country,date=datos_epidemia$date,
                      week=format(datos_epidemia$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country=country,datos_epidemia),
            rates=list(means=data.frame(country=country,ifr=0.004),
                       scale=.1),
            pvec=distribution_death
        )
    ),
    pops=data.frame(country=country,pop=pop),
    si=si 
)
args$algorithm <- "sampling"
args$sampling_args <- list(iter=80,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args$group_subset <- c(country)
args$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args$prior <- rstanarm::normal(location=0,scale=.5)
args$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit <- do.call("epim", args)

return(fit)
}
fit <- fit_epidemia(datos_epidemia,country="Bogotá",pop=8380801)

infeciones <-plot_infections(fit)
datos_inf <-infeciones$data
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data

rtu <- get_rt_unified(rt_epiestim, rt_epidemia)

plot_rt_comp <- function(rtu){

    ggplot(rtu) + 
    geom_ribbon(aes(x = date, ymin = epidemia_low_50, ymax = epidemia_up_50, fill = method_epidemia), alpha = 0.5) +
    geom_ribbon(aes(x = date, ymin = epidemia_low_95, ymax = epidemia_up_95, fill = method_epidemia), alpha = 0.2) +
    geom_ribbon(aes(x = date, ymin = epiestim_q0025, ymax = epiestim_q0975, fill = method_epiestim), alpha = 0.5) +
    geom_line(aes(x = date, y = epiestim_mean), colour = 'blue') +
    cowplot::theme_minimal_grid(20)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '1 week') +
    xlab("") + ylab("Número reproductivo R(t)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("darkgreen", "blue")) +
    theme(legend.position = c(0.6, 0.8), legend.title = element_blank())
    
    return(plot_rt_comp)
}
    

###plot rt_Epiestim

# test <- list(numeroEfectivo,data_bogota)
# saveRDS(test,"test.RDS")))

rm(list=ls())

test <- readRDS("test.RDS")
rm(test)

rt_epiestim <- test [[2]]# numeroEfectivo
rt_epidemia <- test [[1]] #data_bogota





# grafica <-
size = 10
    ggplot(numeroEfectivo, aes(x = Fecha_Fin_Ventana, 
                           y = `Mean(R)`)) + 
    geom_ribbon(aes(ymin = `Quantile.0.025(R)`, 
                    ymax = `Quantile.0.975(R)`), 
                fill = 'darkgreen', alpha = 0.4) +
    geom_line(colour = 'steelblue', size = 0.75) +
    theme_bw(size)+
    geom_ribbon(data = data_bogota, 
                aes(x = date, ymin = low, ymax = up), 
                fill = "skyblue1", alpha = 0.5, inherit.aes = F)+
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red")+
    scale_x_date(date_breaks = '2 weeks') +
    xlab("") + ylab("Número reproductivo R(t)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))



data_bogota_1 <-rt_bogota$data


plot_pr <- rt_bogota
plot_rt_1 <- plot_rt(fit_1)+ ggtitle("Bogotá")
plot_grid(plot_pr, plot_rt, nrow = 2, 
          rel_heights = c(1.1, 1.3),
          align = "v") 

plot_grid

plot_rt(fit_1)
#####
rm(list=ls())
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

source("funs_z.R")
source("rt_unified/functions.R")
#____ DATASET
cases <- read_csv("data/Casos_positivos_de_COVID-19_27_07_20.csv")
#datos_1 <- read_csv("data/Casos_positivos_de_COVID-19_27_07_20.csv")
names(cases) <- epitrix::clean_labels(names(cases))
cases$departamento_o_distrito <- epitrix::clean_labels(cases$departamento_o_distrito)
cases$ciudad_de_ubicacion <- epitrix::clean_labels(cases$ciudad_de_ubicacion)
unique(cases$departamento_o_distrito)
options(mc.cores=parallel::detectCores())# esta instrucción permite que se ejecuten cadenas en paralelo y no en secuencia

##Seleccionar lugar y tipo
#bogota_d_c
#atlantico
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
plot_epidemia <- plot_rt(fit)
rt_epiestim <- data_rt$data_rt
rt_epidemia <- plot_epidemia$data
rtu <- get_rt_unified(rt_epiestim, rt_epidemia)

bogota <-plot_rt_comp(rtu)

bogota




#### Análisis para Barranquilla
barranquilla<- cases%>%filter(departamento_o_distrito=="barranquilla_d_e")
bar_cases <- fCleanData(datos = cases,  place = "barranquilla_d_e", type = "depto")

new_cases_bar <-
    bar_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_bar <- 
    bar_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_bar, new_deaths_bar, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
#incidence$X <- 1:NROW(incidence)
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_bar <- deaths[1:(NROW(deaths)-14),]

################ Rt_barranquilla
names(barranquilla) <- epitrix::clean_labels(names(barranquilla))

barranquilla$fecha_diagnostico <- as.Date(as.numeric(barranquilla$fecha_diagnostico),
                                   origin = "1900-01-01")
datos_inc_bar <- barranquilla %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_bar$fis_clean <- clean_labels(datos_inc_bar$fis)
datos_inc_bar$asymptomatic <- 0
datos_inc_bar$asymptomatic[datos_inc_bar$fis_clean ==  "asintomatico"] <- 1

barranquilla <- datos_inc_bar %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(barranquilla$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_bar  <- incidence(barranquilla$date, groups = barranquilla$type)[1:(max(barranquilla$date)-min(barranquilla$date)- rezago)] 
days <- seq_along(inc_multiple_bar$dates)[2:(length(inc_multiple_bar$dates)-7)]
RTo_bar  <- estimate_R(inc_multiple_bar, method = "parametric_si",
                   config = make_config(list(
                       mean_si = 6.48, std_si = 3.83,
                       t_start=days, t_end=days+7)))

intervalo_serial_bar <- as.vector(RTo_bar$si_distr)

data_rt <- RTo_bar$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_bar$dates) + RTo_bar$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_bar$dates) + RTo_bar$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_bar,
            inc_multiple = inc_multiple_bar,
            fechaConfiable = fechaConfiable,
            datos = barranquilla,
            datos_inc = datos_inc_bar)
rt_bar <-fPlotRt_1(rt)

############################completar Serie
incidencia_bar <- deaths%>% select(date,deaths)
inicio_bar <-data.frame(date=c("2020-03-07","2020-03-08","2020-03-11","2020-03-31","2020-04-08"),
                      deaths=c(0, 0,0,0,0))
incidencia_bar <- rbind(incidencia_bar,inicio_bar)
incidencia_bar <-incidencia_bar %>% arrange(date)

########################################
eventos_publicos_bar <- c(rep(0,6),rep(1,122))
colegios_publicos_bar <- c(rep(0,10),rep(1,118))
cuarentena_bar <- c(rep(0,19),rep(1,109))
total <- nrow(incidencia_bar)
country <-c(rep("Barranquilla",total))
intervenciones_bar <- data.frame(country,date=incidencia_bar$date,eventos_publicos_bar,colegios_publicos_bar,cuarentena_bar)
obs_2 <- !is.na(incidencia_bar$deaths)
si_bar <- intervalo_serial_bar

############# Análisis para Barranquilla
args_2 <- list(
    data = data.frame(country=intervenciones_bar$country,date=intervenciones_bar$date,
                      week=format(intervenciones_bar$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country,incidencia_bar),
            rates=list(means=data.frame(country="Barranquilla",ifr=0.004),
                       scale=.1),
           pvec=prueba_5
    
        )
    ),
    pops=data.frame(country="Barranquilla",pop=1274250),
    si=si_bar 
)

args_2$algorithm <- "sampling"
args_2$sampling_args <- list(iter=500,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_2$group_subset <- c("Barranquilla")
args_2$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_2$prior <- rstanarm::normal(location=0,scale=.5)
args_2$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_2 <- do.call("epim", args_2)
grid.arrange(plot_obs(fit_2, type="deaths"), plot_infections(fit_2), plot_rt(fit_2),nrow=3)

grid.arrange(plot_rt(fit_2),rt_bar)
grid.arrange(plot_obs(fit_2, type="deaths"), plot_infections(fit_2))

#####Análisis Atlántico

atlantico<- cases%>%filter(departamento_o_distrito=="atlantico")
atl_cases <- fCleanData(datos = cases,  place = "atlantico", type = "depto")

new_cases_atl <-
    atl_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_atl <- 
    atl_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_atl, new_deaths_atl, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_atl <- deaths[1:(NROW(deaths)-14),]

################ Rt_atlántico
names(atlantico) <- epitrix::clean_labels(names(atlantico))

atlantico$fecha_diagnostico <- as.Date(as.numeric(atlantico$fecha_diagnostico),
                                          origin = "1900-01-01")
datos_inc_atl <- atlantico %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_atl$fis_clean <- clean_labels(datos_inc_atl$fis)
datos_inc_atl$asymptomatic <- 0
datos_inc_atl$asymptomatic[datos_inc_atl$fis_clean ==  "asintomatico"] <- 1

atlantico <- datos_inc_atl %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(atlantico$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_atl  <- incidence(atlantico$date, groups = atlantico$type)[1:(max(atlantico$date)-min(atlantico$date)- rezago)] 
days <- seq_along(inc_multiple_atl$dates)[2:(length(inc_multiple_atl$dates)-7)]
RTo_atl  <- estimate_R(inc_multiple_atl, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))

intervalo_serial_atl <- as.vector(RTo_atl$si_distr)

data_rt <- RTo_atl$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_atl$dates) + RTo_atl$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_atl$dates) + RTo_atl$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_atl,
            inc_multiple = inc_multiple_atl,
            fechaConfiable = fechaConfiable,
            datos = atlantico,
            datos_inc = datos_inc_atl)
rt_atl <-fPlotRt_1(rt)

############################completar Serie
incidencia_atl <- deaths%>% select(date,deaths)
inicio_atl <-data.frame(date=c("2020-03-04","2020-03-05","2020-03-06","2020-03-07","2020-03-08",
                               "2020-03-09","2020-03-10","2020-03-11","2020-03-12","2020-03-14",
                               "2020-03-18","2020-03-23","2020-03-25","2020-03-26","2020-03-29",
                               "2020-03-30","2020-04-09","2020-04-11"),
                        deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
incidencia_atl <- rbind(incidencia_atl,inicio_atl)
incidencia_atl <-incidencia_atl %>% arrange(date)

########################################
# eventos_publicos_bar <- c(rep(0,6),rep(1,122))
# colegios_publicos_bar <- c(rep(0,10),rep(1,118))
# cuarentena_bar <- c(rep(0,19),rep(1,109))
# total <- nrow(incidencia_bar)
# country <-c(rep("Barranquilla",total))
# intervenciones_bar <- data.frame(country,date=incidencia_bar$date,eventos_publicos_bar,colegios_publicos_bar,cuarentena_bar)
obs_3 <- !is.na(incidencia_atl$deaths)
si_atl <- intervalo_serial_atl

############# Análisis para atlantico
args_3 <- list(
    data = data.frame(country="atlantico",date=incidencia_atl$date,
                      week=format(incidencia_atl$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="atlantico",incidencia_atl),
            rates=list(means=data.frame(country="atlantico",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
            
        )
    ),
    pops=data.frame(country="atlantico",pop=2272128),
    si=si_atl 
)

args_3$algorithm <- "sampling"
args_3$sampling_args <- list(iter=500,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_3$group_subset <- c("atlantico")
args_3$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_3$prior <- rstanarm::normal(location=0,scale=.5)
args_3$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_3 <- do.call("epim", args_3)
grid.arrange(plot_obs(fit_3, type="deaths"), plot_infections(fit_3), plot_rt(fit_3),nrow=3)

grid.arrange(plot_rt(fit_3),rt_atl)
grid.arrange(plot_obs(fit_3, type="deaths"), plot_infections(fit_3))

########Análisis Bolivar

bolivar<- cases%>%filter(departamento_o_distrito=="bolivar")
boli_cases <- fCleanData(datos = cases,  place = "bolivar", type = "depto")

new_cases_boli <-
    boli_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_boli <- 
    boli_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_boli, new_deaths_boli, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_boli <- deaths[1:(NROW(deaths)-14),]

################ Rt_atlántico
names(bolivar) <- epitrix::clean_labels(names(bolivar))

bolivar$fecha_diagnostico <- as.Date(as.numeric(bolivar$fecha_diagnostico),
                                       origin = "1900-01-01")
datos_inc_boli <- bolivar %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_boli$fis_clean <- clean_labels(datos_inc_boli$fis)
datos_inc_boli$asymptomatic <- 0
datos_inc_boli$asymptomatic[datos_inc_boli$fis_clean ==  "asintomatico"] <- 1

bolivar <- datos_inc_boli %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(bolivar$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_boli  <- incidence(bolivar$date, groups = bolivar$type)[1:(max(bolivar$date)-min(bolivar$date)- rezago)] 
days <- seq_along(inc_multiple_boli$dates)[2:(length(inc_multiple_boli$dates)-7)]
RTo_boli  <- estimate_R(inc_multiple_boli, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))

intervalo_serial_boli <- as.vector(RTo_boli$si_distr)

data_rt <- RTo_boli$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_boli$dates) + RTo_boli$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_boli$dates) + RTo_boli$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_boli,
            inc_multiple = inc_multiple_boli,
            fechaConfiable = fechaConfiable,
            datos = bolivar,
            datos_inc = datos_inc_boli)
rt_boli <-fPlotRt_1(rt)

############################completar Serie
incidencia_boli <- deaths%>% select(date,deaths)
inicio_boli <-data.frame(date=c("2020-03-13","2020-03-14","2020-03-16","2020-03-18","2020-03-20",
                               "2020-03-22","2020-03-23","2020-03-24","2020-03-25","2020-03-27",
                               "2020-03-28","2020-03-29","2020-03-31","2020-04-01","2020-04-02",
                               "2020-04-04","2020-04-06","2020-04-11","2020-04-12",
                               "2020-04-14","2020-04-15","2020-04-18","2020-04-21",
                               "2020-05-01"),
                        deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
incidencia_boli <- rbind(incidencia_boli,inicio_boli)
incidencia_boli <-incidencia_boli %>% arrange(date)

########################################
# eventos_publicos_bar <- c(rep(0,6),rep(1,122))
# colegios_publicos_bar <- c(rep(0,10),rep(1,118))
# cuarentena_bar <- c(rep(0,19),rep(1,109))
# total <- nrow(incidencia_bar)
# country <-c(rep("Barranquilla",total))
# intervenciones_bar <- data.frame(country,date=incidencia_bar$date,eventos_publicos_bar,colegios_publicos_bar,cuarentena_bar)
obs_4 <- !is.na(incidencia_boli$deaths)
si_boli <- intervalo_serial_boli

############# Análisis para Bolivar
args_4 <- list(
    data = data.frame(country="bolivar",date=incidencia_boli$date,
                      week=format(incidencia_boli$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="bolivar",incidencia_boli),
            rates=list(means=data.frame(country="bolivar",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
            
        )
    ),
    pops=data.frame(country="bolivar",pop=2213061),
    si=si_boli
)

args_4$algorithm <- "sampling"
args_4$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_4$group_subset <- c("bolivar")
args_4$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_4$prior <- rstanarm::normal(location=0,scale=.5)
args_4$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_4 <- do.call("epim", args_4)
grid.arrange(plot_obs(fit_4, type="deaths"), plot_infections(fit_4), plot_rt(fit_4),nrow=3)

grid.arrange(plot_rt(fit_4),rt_boli)
grid.arrange(plot_obs(fit_4, type="deaths"), plot_infections(fit_4))

#### Análisis Cartagena
cartagena<- cases%>%filter(departamento_o_distrito=="cartagena_d_t_y_c")

cart_cases <- fCleanData(datos = cases,  place = "cartagena_d_t_y_c", type = "depto")
#cali_cases <- fCleanData(datos = cases,  place = "Cali", type = "ciudad")

new_cases_cart <-
    cart_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_cart <- 
    cart_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_cart, new_deaths_cart, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_cart <- deaths[1:(NROW(deaths)-14),]

################ Rt_Cartagena
names(cartagena) <- epitrix::clean_labels(names(cartagena))

cartagena$fecha_diagnostico <- as.Date(as.numeric(cartagena$fecha_diagnostico),
                                  origin = "1900-01-01")
datos_inc_cart <- cartagena %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_cart$fis_clean <- clean_labels(datos_inc_cart$fis)
datos_inc_cart$asymptomatic <- 0
datos_inc_cart$asymptomatic[datos_inc_cart$fis_clean ==  "asintomatico"] <- 1

datos_cartagena <- datos_inc_cart %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(datos_cartagena$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_cart  <- incidence(datos_cartagena$date, groups = datos_cartagena$type)[1:(max(datos_cartagena$date)-min(datos_cartagena$date)- rezago)] 
days <- seq_along(inc_multiple_cart$dates)[2:(length(inc_multiple_cart$dates)-7)]
RTo_cart  <- estimate_R(inc_multiple_cart, method = "parametric_si",
                        config = make_config(list(
                            mean_si = 6.48, std_si = 3.83,
                            t_start=days, t_end=days+7)))

intervalo_serial_cart <- as.vector(RTo_cart$si_distr)

data_rt <- RTo_cart$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_cart$dates) + RTo_cart$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_cart$dates) + RTo_cart$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_cart,
            inc_multiple = inc_multiple_cart,
            fechaConfiable = fechaConfiable,
            datos = datos_cartagena,
            datos_inc = datos_inc_cart)
rt_cart <-fPlotRt_1(rt)
############################completar Serie
incidencia_cart <- deaths%>% select(date,deaths)
inicio_cart <-data.frame(date=c("2020-03-03","2020-03-04","2020-03-05","2020-03-07",
                                "2020-03-08","2020-03-09","2020-03-10"),
                         deaths=c(0,0,0,0,0,0,0))
incidencia_cart <- rbind(incidencia_cart,inicio_cart)
incidencia_cart <-incidencia_cart %>% arrange(date)

########################################
# eventos_publicos_cali <- c(rep(0,10),rep(1,122))
# colegios_publicos_cali <- c(rep(0,14),rep(1,118))
# cuarentena_cali <- c(rep(0,23),rep(1,109))
# total <- nrow(incidencia_cali)
# country <-c(rep("Cali",total))
# intervenciones_cali <- data.frame(country,date=incidencia_cali$date,eventos_publicos_cali,colegios_publicos_cali,cuarentena_cali)
obs_5 <- !is.na(incidencia_cart$deaths)
si_cart <- intervalo_serial_cart

############# Análisis para Cali
args_5 <- list(
    data = data.frame(country="cartagena",date=incidencia_cart$date,
                      week=format(incidencia_cart$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="cartagena",incidencia_cart),
            rates=list(means=data.frame(country="cartagena",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="cartagena",pop=950000),
    si=si_cart 
)
#rates=list(means=data.frame(f("bogota"),0.01244963)
args_5$algorithm <- "sampling"
args_5$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_5$group_subset <- c("cartagena")
args_5$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_5$prior <- rstanarm::normal(location=0,scale=.5)
args_5$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_5 <- do.call("epim", args_5)
grid.arrange(plot_obs(fit_5, type="deaths"), plot_infections(fit_5), plot_rt(fit_5),nrow=3)

grid.arrange(plot_rt(fit_5),rt_cart)
grid.arrange(plot_obs(fit_5, type="deaths"), plot_infections(fit_5))


### Análisis Cali
cali<- cases%>%filter(ciudad_de_ubicacion=="Cali")

cali_cases <- fCleanData(datos = cases,  place = "cali", type = "ciudad")
#cali_cases <- fCleanData(datos = cases,  place = "Cali", type = "ciudad")

new_cases_cali <-
    cali_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_cali <- 
    cali_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_cali, new_deaths_cali, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
#incidence$X <- 1:NROW(incidence)
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_cali <- deaths[1:(NROW(deaths)-14),]

################ Rt_Cali
names(cali) <- epitrix::clean_labels(names(cali))

cali$fecha_diagnostico <- as.Date(as.numeric(cali$fecha_diagnostico),
                                   origin = "1900-01-01")
datos_inc_cali <- cali %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_cali$fis_clean <- clean_labels(datos_inc_cali$fis)
datos_inc_cali$asymptomatic <- 0
datos_inc_cali$asymptomatic[datos_inc_cali$fis_clean ==  "asintomatico"] <- 1

datos_cali <- datos_inc_cali %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(datos_cali$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_cali  <- incidence(datos_cali$date, groups = datos_cali$type)[1:(max(datos_cali$date)-min(datos_cali$date)- rezago)] 
days <- seq_along(inc_multiple_cali$dates)[2:(length(inc_multiple_cali$dates)-7)]
RTo_cali  <- estimate_R(inc_multiple_cali, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))

intervalo_serial_cali <- as.vector(RTo_cali$si_distr)

data_rt <- RTo_cali$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_cali$dates) + RTo_cali$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_cali$dates) + RTo_cali$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_cali,
            inc_multiple = inc_multiple_cali,
            fechaConfiable = fechaConfiable,
            datos = datos_cali,
            datos_inc = datos_inc_cali)
rt_cali <-fPlotRt_1(rt)
############################completar Serie
incidencia_cali <- deaths%>% select(date,deaths)
inicio_cali <-data.frame(date=c("2020-03-03","2020-03-04","2020-03-06","2020-03-08"),
                        deaths=c(0, 0,0,0))
incidencia_cali <- rbind(incidencia_cali,inicio_cali)
incidencia_cali <-incidencia_cali %>% arrange(date)

########################################
eventos_publicos_cali <- c(rep(0,10),rep(1,122))
colegios_publicos_cali <- c(rep(0,14),rep(1,118))
cuarentena_cali <- c(rep(0,23),rep(1,109))
total <- nrow(incidencia_cali)
country <-c(rep("Cali",total))
intervenciones_cali <- data.frame(country,date=incidencia_cali$date,eventos_publicos_cali,colegios_publicos_cali,cuarentena_cali)
obs_6 <- !is.na(incidencia_cali$deaths)
si_cali <- intervalo_serial_cali

############# Análisis para Cali
args_6 <- list(
    data = data.frame(country=intervenciones_cali$country,date=intervenciones_cali$date,
                      week=format(intervenciones_cali$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country,incidencia_cali),
            rates=list(means=data.frame(country="Cali",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="Cali",pop=2252616),
    si=si_cali 
)
#rates=list(means=data.frame(f("bogota"),0.01244963)
args_6$algorithm <- "sampling"
args_6$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_6$group_subset <- c("Cali")
args_6$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_6$prior <- rstanarm::normal(location=0,scale=.5)
args_6$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_6 <- do.call("epim", args_6)
grid.arrange(plot_obs(fit_6, type="deaths"), plot_infections(fit_6), plot_rt(fit_6),nrow=3)

grid.arrange(plot_rt(fit_6),rt_cali)
grid.arrange(plot_obs(fit_6, type="deaths"), plot_infections(fit_6))

#### Análisis Chocó

choco<- cases%>%filter(departamento_o_distrito=="choco")

choco_cases <- fCleanData(datos = cases,  place = "choco", type = "depto")

new_cases_choco <-
    choco_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_choco <- 
    choco_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_choco, new_deaths_choco, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
#incidence$X <- 1:NROW(incidence)
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_choco <- deaths[1:(NROW(deaths)-14),]

################ Rt_choco
names(choco) <- epitrix::clean_labels(names(choco))

choco$fecha_diagnostico <- as.Date(as.numeric(choco$fecha_diagnostico),
                                  origin = "1900-01-01")
datos_inc_choco <- choco %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_choco$fis_clean <- clean_labels(datos_inc_choco$fis)
datos_inc_choco$asymptomatic <- 0
datos_inc_choco$asymptomatic[datos_inc_choco$fis_clean ==  "asintomatico"] <- 1

datos_choco <- datos_inc_choco %>% filter(asymptomatic == 0)
rezago <- 14
fechaConfiable <- as.Date(max(datos_choco$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_choco  <- incidence(datos_choco$date, groups = datos_choco$type)[1:(max(datos_choco$date)-min(datos_choco$date)- rezago)] 
days <- seq_along(inc_multiple_choco$dates)[2:(length(inc_multiple_choco$dates)-7)]
RTo_choco  <- estimate_R(inc_multiple_choco, method = "parametric_si",
                        config = make_config(list(
                            mean_si = 6.48, std_si = 3.83,
                            t_start=days, t_end=days+7)))

intervalo_serial_choco <- as.vector(RTo_choco$si_distr)

data_rt <- RTo_choco$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_choco$dates) + RTo_choco$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_choco$dates) + RTo_choco$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_choco,
            inc_multiple = inc_multiple_choco,
            fechaConfiable = fechaConfiable,
            datos = datos_choco,
            datos_inc = datos_inc_choco)
rt_choco <-fPlotRt_1(rt)

############################completar Serie
incidencia_choco <- deaths%>% select(date,deaths)
inicio_choco <-data.frame(date=c("2020-03-24","2020-03-25","2020-03-26","2020-03-27",
                                "2020-03-28","2020-03-29","2020-03-30","2020-03-31",
                                "2020-04-03","2020-04-07","2020-04-08","2020-04-10",
                                "2020-04-11","2020-04-21","2020-04-22"),
                         deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
incidencia_choco <- rbind(incidencia_choco,inicio_choco)
incidencia_choco <-incidencia_choco %>% arrange(date)

########################################
#eventos_publicos_choco <- c(rep(0,10),rep(1,122))
# colegios_publicos_choco <- c(rep(0,14),rep(1,118))
# cuarentena_choco <- c(rep(0,23),rep(1,109))
# total <- nrow(incidencia_choco)
# country <-c(rep("Choco",total))
# intervenciones_choco <- data.frame(country,date=incidencia_choco$date,eventos_publicos_choco,colegios_publicos_choco,cuarentena_choco)
obs_7 <- !is.na(incidencia_choco$deaths)
si_choco <- intervalo_serial_choco

############# Análisis para choco
args_7 <- list(
    data = data.frame(country="choco",date=incidencia_choco$date,
                      week=format(incidencia_choco$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="choco",incidencia_choco),
            rates=list(means=data.frame(country="choco",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="choco",pop=549225),
    si=si_choco
)
#rates=list(means=data.frame(f("bogota"),0.01244963)
args_7$algorithm <- "sampling"
args_7$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_7$group_subset <- c("choco")
args_7$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_7$prior <- rstanarm::normal(location=0,scale=.5)
args_7$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_7 <- do.call("epim", args_7)
grid.arrange(plot_obs(fit_7, type="deaths"), plot_infections(fit_7), plot_rt(fit_7),nrow=3)

grid.arrange(plot_rt(fit_7),rt_choco)
grid.arrange(plot_obs(fit_7, type="deaths"), plot_infections(fit_7))


####Análisis Amazonas
amazonas<- cases%>%filter(departamento_o_distrito=="amazonas")

amaz_cases <- fCleanData(datos = cases,  place = "amazonas", type = "depto")

new_cases_amaz <-
    amaz_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_amaz <- 
    amaz_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_amaz, new_deaths_amaz, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_amaz <- deaths[1:(NROW(deaths)-14),]

################ Rt_amazonas
names(amazonas) <- epitrix::clean_labels(names(amazonas))

amazonas$fecha_diagnostico <- as.Date(as.numeric(amazonas$fecha_diagnostico),
                                  origin = "1900-01-01")
datos_inc_amaz <- amazonas %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_amaz$fis_clean <- clean_labels(datos_inc_amaz$fis)
datos_inc_amaz$asymptomatic <- 0
datos_inc_amaz$asymptomatic[datos_inc_amaz$fis_clean ==  "asintomatico"] <- 1

datos_amazonas <- datos_inc_amaz %>% filter(asymptomatic == 0)
rezago <- 14
fechaConfiable <- as.Date(max(datos_amazonas$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_amaz  <- incidence(datos_amazonas$date, groups = datos_amazonas$type)[1:(max(datos_amazonas$date)-min(datos_amazonas$date)- rezago)] 
days <- seq_along(inc_multiple_amaz$dates)[2:(length(inc_multiple_amaz$dates)-7)]
RTo_amaz  <- estimate_R(inc_multiple_amaz, method = "parametric_si",
                        config = make_config(list(
                            mean_si = 6.48, std_si = 3.83,
                            t_start=days, t_end=days+7)))

intervalo_serial_amaz <- as.vector(RTo_amaz$si_distr)

data_rt <- RTo_amaz$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_amaz$dates) + RTo_amaz$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_amaz$dates) + RTo_amaz$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_amaz,
            inc_multiple = inc_multiple_amaz,
            fechaConfiable = fechaConfiable,
            datos = datos_amazonas,
            datos_inc = datos_inc_amaz)
rt_amaz <-fPlotRt_1(rt)
############################completar Serie
incidencia_amaz <- deaths%>% select(date,deaths)
inicio_amaz <-data.frame(date=c("2020-03-15","2020-03-16","2020-03-17","2020-03-18","2020-03-19",
                                "2020-03-20","2020-03-21","2020-03-22","2020-03-23","2020-03-24",
                                "2020-03-25","2020-03-26","2020-03-27","2020-03-28","2020-03-29",
                                "2020-03-30","2020-04-01","2020-04-02","2020-04-03","2020-04-04",
                                "2020-04-06","2020-04-08","2020-06-27"),
                         deaths=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
incidencia_amaz <- rbind(incidencia_amaz,inicio_amaz)
incidencia_amaz <-incidencia_amaz %>% arrange(date)

########################################
# eventos_publicos_cali <- c(rep(0,10),rep(1,122))
# colegios_publicos_cali <- c(rep(0,14),rep(1,118))
# cuarentena_cali <- c(rep(0,23),rep(1,109))
# total <- nrow(incidencia_cali)
# country <-c(rep("Cali",total))
# intervenciones_cali <- data.frame(country,date=incidencia_cali$date,eventos_publicos_cali,colegios_publicos_cali,cuarentena_cali)
obs_8 <- !is.na(incidencia_amaz$deaths)
si_amaz <- intervalo_serial_amaz

############# Análisis para amazonas
args_8 <- list(
    data = data.frame(country="amazonas",date=incidencia_amaz$date,
                      week=format(incidencia_amaz$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="amazonas",incidencia_amaz),
            rates=list(means=data.frame(country="amazonas",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="amazonas",pop=80464),
    si=si_amaz 
)
args_8$algorithm <- "sampling"
args_8$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_8$group_subset <- c("amazonas")
args_8$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_8$prior <- rstanarm::normal(location=0,scale=.5)
args_8$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_8 <- do.call("epim", args_8)
grid.arrange(plot_obs(fit_8, type="deaths"), plot_infections(fit_8), plot_rt(fit_8),nrow=3)

grid.arrange(plot_rt(fit_8),rt_amaz)
grid.arrange(plot_obs(fit_8, type="deaths"), plot_infections(fit_8))


###Analisis Medellín

medellin<- cases%>%filter(ciudad_de_ubicacion=="Medellín")

medellin_cases <- fCleanData(datos = cases,  place = "medellin", type = "ciudad")
#cali_cases <- fCleanData(datos = cases,  place = "Cali", type = "ciudad")

new_cases_medellin <-
    medellin_cases %>% 
    group_by(fis) %>% 
    summarise(cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)

new_deaths_medellin <- 
    medellin_cases %>% 
    group_by(fecha_de_muerte) %>% 
    summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
    rename(date = fecha_de_muerte) 

deaths <- merge(new_cases_medellin, new_deaths_medellin, by = "date", all = TRUE) %>% 
    filter(!is.na(date)) 
deaths$cases[is.na(deaths$cases)] <- 0
deaths$deaths[is.na(deaths$deaths)] <- 0
#incidence$X <- 1:NROW(incidence)
deaths <- deaths[1:(NROW(deaths)-14),]
infecciones_medellin <- deaths[1:(NROW(deaths)-14),]

################ Rt_medellin
names(medellin) <- epitrix::clean_labels(names(medellin))

medellin$fecha_diagnostico <- as.Date(as.numeric(medellin$fecha_diagnostico),
                                  origin = "1900-01-01")
datos_inc_medellin <- medellin %>%
    mutate(date = as.Date (fis),
           fecha_diagnostico=as.Date(fecha_diagnostico),
           type=case_when(tipo=="Importado"~"imported",
                          tipo=="Relacionado"~"local",
                          tipo=="En estudio"~"local"))

datos_inc_medellin$fis_clean <- clean_labels(datos_inc_medellin$fis)
datos_inc_medellin$asymptomatic <- 0
datos_inc_medellin$asymptomatic[datos_inc_medellin$fis_clean ==  "asintomatico"] <- 1

datos_medellin <- datos_inc_medellin %>% filter(asymptomatic == 0)

rezago <- 14
fechaConfiable <- as.Date(max(datos_medellin$fecha_de_notificacion),origin = "1900-01-01") - rezago

inc_multiple_medellin  <- incidence(datos_medellin$date, groups = datos_medellin$type)[1:(max(datos_medellin$date)-min(datos_medellin$date)- rezago)] 
days <- seq_along(inc_multiple_medellin$dates)[2:(length(inc_multiple_medellin$dates)-7)]
RTo_medellin  <- estimate_R(inc_multiple_medellin, method = "parametric_si",
                        config = make_config(list(
                            mean_si = 6.48, std_si = 3.83,
                            t_start=days, t_end=days+7)))

intervalo_serial_medellin <- as.vector(RTo_medellin$si_distr)

data_rt <- RTo_medellin$R
data_rt$Fecha_Inicio_Ventana <- min(inc_multiple_medellin$dates) + RTo_medellin$R$t_start 
data_rt$Fecha_Fin_Ventana    <- min(inc_multiple_medellin$dates) + RTo_medellin$R$t_end 

rt <- list (data_rt = data_rt,
            RTo = RTo_medellin,
            inc_multiple = inc_multiple_medellin,
            fechaConfiable = fechaConfiable,
            datos = datos_medellin,
            datos_inc = datos_inc_medellin)
rt_medellin <-fPlotRt_1(rt)
############################completar Serie
incidencia_medellin <- deaths%>% select(date,deaths)
inicio_medellin <-data.frame(date=c("2020-03-01","2020-03-03","2020-03-04","2020-03-07","2020-04-30"),
                         deaths=c(0,0,0,0,0))
incidencia_medellin <- rbind(incidencia_medellin,inicio_medellin)
incidencia_medellin <-incidencia_medellin %>% arrange(date)

# ########################################
# eventos_publicos_cali <- c(rep(0,10),rep(1,122))
# colegios_publicos_cali <- c(rep(0,14),rep(1,118))
# cuarentena_cali <- c(rep(0,23),rep(1,109))
# total <- nrow(incidencia_cali)
# country <-c(rep("Cali",total))
# intervenciones_cali <- data.frame(country,date=incidencia_cali$date,eventos_publicos_cali,colegios_publicos_cali,cuarentena_cali)
 obs_9 <- !is.na(incidencia_medellin$deaths)
 si_medellin <- intervalo_serial_medellin

############# Análisis para Cali
args_9 <- list(
    data = data.frame(country="medellin",date=incidencia_medellin$date,
                      week=format(incidencia_medellin$date, "%V")),
    obs=list(
        deaths=list(
            odata=data.frame(country="medellin",incidencia_medellin),
            rates=list(means=data.frame(country="medellin",ifr=0.004),
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="medellin",pop=2500000),
    si=si_medellin 
)
#rates=list(means=data.frame(f("bogota"),0.01244963)
args_9$algorithm <- "sampling"
args_9$sampling_args <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_9$group_subset <- c("medellin")
args_9$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_9$prior <- rstanarm::normal(location=0,scale=.5)
args_9$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_9 <- do.call("epim", args_9)
grid.arrange(plot_obs(fit_9, type="deaths"), plot_infections(fit_9), plot_rt(fit_),nrow=3)

grid.arrange(plot_rt(fit_9),rt_medellin)
grid.arrange(plot_obs(fit_9, type="deaths"), plot_infections(fit_9))

plot_pr <- plot_rt(fit_1)+ theme(legend.position = "none")
data_bogota <- plot_pr$data
############################################
grafica_final <- ggplot(data_epidemia, aes(x = date, y =low)) + 
    geom_ribbon(aes(ymin = low, 
                    ymax = up, 
                fill = 'grey', alpha = 0.5) +
    geom_line(colour = 'steelblue', size = 0.75) +
    theme_bw(25))+
    labs(title = 'Número Efectivo de Reproducción',
         subtitle = paste0('Bogotá D.C.  (n = ', nrow(positivos), ')'),
         x = 'Fecha Final de la Ventana',
         y = 'Número Efectivo de Reproducción') +
    scale_x_date(date_breaks = '1 week')
    










############################################
plot_rt <- rt_medellin
plot_grid(plot_pr, plot_rt, nrow = 2, 
          rel_heights = c(1.1, 1.3),
          align = "v")





###### Utilizando solo datos de incidencia
obs <- !is.na(incidencia_bog$cases)
args_1 <- list(formula=Rt(country,date)~ 0 + rw(time=week,prior_scale=0.1),
               data=data.frame(country="Bogota",
                               date=incidencia_bog$date,
                               week=format(incidencia_bog$date, "%V")),
               obs=list(
                   incidence=list(
                       odata=data.frame(country="Bogota",
                                        date=incidencia_bog$date[obs],incidence=incidencia_bog$cases[obs]),
                       rates=list(means=data.frame(factor("Bogota"),1),
                                  scale=.01),
                       pvec=c(.25,.25,.25,.25)
                   )
               ),
               seed_days=7,
               algorithm="sampling",
               r0=3,
               pops=data.frame(country="Bogota",pop=8e6),
               si=intervalo_serial,
               prior = rstanarm::normal(location=0,scale=.2),
               prior_intercept = rstanarm::normal(location=0,scale=.5),
               prior_tau = rstanarm::exponential(rate=4)
)
args_1$sampling_args_1 <- list(iter=150,control=list(adapt_delta=0.95,max_treedepth=15),seed=713)
fit <- do.call("epim",args_1)
grid.arrange(plot_rt(fit),plot_obs(fit,"incidence"),plot_infections(fit),
             nrow=3)















########### Analizando para Italia######################
args <- EuropeCovid
prueba_1 <-args$data
italia <- prueba_1 %>%filter(country=="Italy")
si_1 <-args$si
prueba_5 <-args$obs$deaths$pvec
prueba_6 <- args$obs$deaths$odata
italia_1 <- prueba_6 %>%filter(country=="Italy")
media_1 <- data.frame(country="Italy",ifr=0.012449626)

args_21 <- list(
    data = italia,
    obs=list(
        deaths=list(
            odata=italia_1,
            rates=list(means=media_1,
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="Italy",pop=60461828),
    si=si_1 
)
args_21$algorithm <- "sampling"
args_21$sampling_args <- list(iter=150,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_21$group_subset <- c("Italy")
args_21$formula <- R(country,date) ~  1 + lockdown
args_21$prior <- rstanarm::normal(location=0,scale=.5)
args_21$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_21 <- do.call("epim", args_21)
grid.arrange(plot_obs(fit_21, type="deaths"), plot_infections(fit_21), plot_rt(fit_21),nrow=3)












options(mc.cores=parallel::detectCores())# esta instrucción permite que se ejecuten cadenas en paralelo y no en secuencia

###### Bogotá muertes
incidence_bog <- deaths%>% select(date,deaths)
inicio<-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
                   deaths=c(0, 0,0))
incidence_bog <- rbind(incidence_bog,inicio)
incidence_bog <-incidence_bog %>% arrange(date) 
#######Bogotá casos####################
incidencia_bog <- deaths%>% select(date,cases)
inicio_2 <-data.frame(date=c("2020-02-29","2020-03-02","2020-03-03"),
                      cases=c(0, 0,0))
incidencia_bog <- rbind(incidencia_bog,inicio_2)
incidencia_bog <-incidencia_bog %>% arrange(date)







###########
args_3 <- list(
    data = datos,
    obs=list(
        deaths=list(
            odata=data.frame(country,incidence_bog),
            rates=list(means=data.frame(factor("bogota"),1),
                       scale=.01),
            pvec=c(.25,.25,.25,.25)
        )
    ),
    pops=data.frame(country="bogota",pop=8000000),
    si=si 
)
args_3$algorithm <- "sampling"
args_3$sampling_args <- list(iter=600,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
#args_3$group_subset <- c("bogota")
args_3$formula <- R(country,date) ~ (1 | country) + eventos_publicos + colegios_publicos + cuarentena
args_3$prior <- rstanarm::normal(location=0,scale=.5)
args_3$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_3 <- do.call("epim", args_3)
grid.arrange(plot_obs(fit_3, type="deaths"), plot_infections(fit_3), plot_rt(fit_3),nrow=3)





args$formula <- R(country,date) ~ (1 | country)







args_1 <- list(formula=Rt(country,date)~ 0 + rw(time=week,prior_scale=0.1),
             data=data.frame(country="bogota",
                             date=incidence_bog$date),
                             #week=format(incidence_bog$date, "%V")),
             obs=list(
                 deaths=list(
                     odata=data.frame(country="bogota",
                                      date=incidence_bog$date[obs_1],incidence=incidence_bog$deaths[obs_1]),
                     rates=list(means=data.frame(factor("bogota"),1),
                                scale=.01),
                     pvec=c(.25,.25,.25,.25)
                 )
             ),
             seed_days=7,
             algorithm="sampling",
             r0=3,
             pops=data.frame(country="bogota",pop=8000000),
             si=si,
             prior = rstanarm::normal(location=0,scale=.2),
             prior_intercept = rstanarm::normal(location=0,scale=.5),
             prior_tau = rstanarm::exponential(rate=4)             
)
args_1$sampling_args_1 <- list(iter=150,control=list(adapt_delta=0.95,max_treedepth=15),seed=713)

fit_1 <- do.call("epim",args_1)

grid.arrange(plot_obs(fit_1,type="deaths"), plot_infections(fit_1), plot_rt(fit_1),nrow=3)

################SEgunda forma
media <- data.frame(country="Bogota",ifr=0.010783873)

args_2 <- list(formula=Rt(country,date)~ 1 + cuarentena,
               data=data.frame(country="bogota",
                               date=incidence_bog$date,
                               week=format(incidence_bog$date, "%V")),
               obs=list(
                   incidence=list(
                       odata=data.frame(country="bogota",
                                        date=incidence_bog$date[obs_1],incidence=incidence_bog$deaths[obs_1]),
                       rates=list(means=media,
                                  scale=.01),
                       pvec=c(.25,.25,.25,.25)
                   )
               ),
               seed_days=7,
               algorithm="sampling",
               r0=3,
               pops=data.frame(country="bogota",pop=8000000),
               si=si,
               prior = rstanarm::normal(location=0,scale=.2),
               prior_intercept = rstanarm::normal(location=0,scale=.5),
               prior_tau = rstanarm::exponential(rate=4)             
)
args_2$sampling_args_2 <- list(iter=500,control=list(adapt_delta=0.95,max_treedepth=15),seed=713)

fit_2 <- do.call("epim",args_2)

grid.arrange(plot_obs(fit_2,type="incidence"), plot_infections(fit_2), plot_rt(fit_2),nrow=3)
#######################
args_3 <- list(formula=Rt(country,date)~ 0 + eventos_publicos+ colegios_publicos+
                   cuarentena,
               data=data.frame(country="bogota",
                               date=incidence_bog$date,
                               eventos_publicos = incidence_bog$eventos_publicos,
                               colegios_publicos = incidence_bog$colegios_publicos,
                               cuarentena = incidence_bog$cuerentena,
                               week=format(incidence_bog$date, "%V")),
               obs=list(
                   incidence=list(
                       odata=data.frame(country="bogota",
                                        date=incidence_bog$date[obs_1],incidence=incidence_bog$deaths[obs_1]),
                       rates=list(means=data.frame(factor("bogota"),1),
                                  scale=.01),
                       pvec=c(.25,.25,.25,.25)
                   )
               ),
               seed_days=7,
               algorithm="sampling",
               r0=3,
               pops=data.frame(country="bogota",pop=8000000),
               si=si,
               prior = rstanarm::normal(location=0,scale=.2),
               prior_intercept = rstanarm::normal(location=0,scale=.5),
               prior_tau = rstanarm::exponential(rate=4)             
)

args_3$sampling_args_3 <- list(iter=500,control=list(adapt_delta=0.95,max_treedepth=15),seed=713)

fit_3 <- do.call("epim",args_3)

grid.arrange(plot_obs(fit_3,type="incidence"), plot_infections(fit_3), plot_rt(fit_3),nrow=3)

###
# flu <- Flu1918
# flu$incidence <- c(rep(NA,1),flu$incidence) ## pad before initialisation
# flu$fludate <- as.Date("1918-01-01")+seq(0,along.with=flu$incidence)
# obs <- !is.na(flu$incidence)
# 
# args <- list(formula=Rt(country,date)~ 0 + rw(time=week,prior_scale=0.1),
#              data=data.frame(country="A",
#                              date=flu$fludate,
#                              week=format(flu$fludate, "%V")),
#              obs=list(
#                  incidence=list(
#                      odata=data.frame(country="A",
#                                       date=flu$fludate[obs],incidence=flu$incidence[obs]),
#                      rates=list(means=data.frame(factor("A"),1),
#                                 scale=.01),
#                      pvec=c(.25,.25,.25,.25)
#                  )
#              ),
#              seed_days=7,
#              algorithm="sampling",
#              r0=3,
#              pops=data.frame(country="A",pop=1e6),
#              si=flu$si,
#              prior = rstanarm::normal(location=0,scale=.2),
#              prior_intercept = rstanarm::normal(location=0,scale=.5),
#              prior_tau = rstanarm::exponential(rate=4)             
# )
# args_1$sampling_args_1 <- list(iter=1000,control=list(adapt_delta=0.95,max_treedepth=15),seed=713)
# 
# 
args <- EuropeCovid
args$algorithm <- "sampling"
args$sampling_args <- list(iter=1e3,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args$group_subset <- c("Italy")
args$formula <- R(country,date) ~  1 + lockdown
args$prior <- rstanarm::normal(location=0,scale=.5)
args$prior_intercept <- rstanarm::normal(location=0,scale=2)



args_2 <- args_1
args_2$algorithm <- "sampling"
args_2$sampling_args_2 <- list(iter=100,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_2$group_subset <- c("bogota")
args_2$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
args_2$prior <- rstanarm::normal(location=0,scale=.5)
args_2$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_3 <- do.call("epim", args_2)
grid.arrange(plot_obs(fit_3, type="incidence"), plot_infections(fit_3), plot_rt(fit_3),nrow=3)


args <- EuropeCovid
args$algorithm <- "sampling"
args$sampling_args <- list(iter=150,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args$group_subset <- c("Germany")
args$formula <- R(country,date) ~  1 + lockdown
args$prior <- rstanarm::normal(location=0,scale=.5)
args$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit <- do.call("epim", args)
grid.arrange(plot_obs(fit, type="deaths"), plot_infections(fit), plot_rt(fit),nrow=3)


list(data=as.data.frame(country="Bogota",incidence_bog),
     obs=list(
         deaths=list(
             odata=data.frame(country="bogota",
                              date=incidence_bog$date[obs_1],incidence=incidence_bog$deaths[obs_1]),
             rates=list(means=data.frame(factor("bogota"),1),
                        scale=.01),
             pvec=c(.25,.25,.25,.25)
         )
     ),
     seed_days=7,
     algorithm="sampling",
     r0=3,
     pops=data.frame(country="bogota",pop=8000000),
     si=si,
     prior = rstanarm::normal(location=0,scale=.2),
     prior_intercept = rstanarm::normal(location=0,scale=.5),
     prior_tau = rstanarm::exponential(rate=4)             
)

data=data.frame(country="bogota",
                date=incidence_bog$date,
                deaths =incidence_bog$deaths)

                week=format(incidence_bog$date, "%V"))

#####################Pruebas
args <- EuropeCovid
prueba_1 <-args$data
italia <- prueba_1 %>%filter(country=="Italy")
si <-args$si
prueba_5 <-args$obs$deaths$pvec

args_2 <- list(
    data = prueba_1,
    obs=list(
        deaths=list(
            odata=italia,
            rates=list(means=media,
                       scale=.1),
            pvec=prueba_5
        )
    ),
    pops=data.frame(country="Italy",pop=60461828),
    si=si 
)
args_2$algorithm <- "sampling"
args_2$sampling_args <- list(iter=150,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
args_2$group_subset <- c("Italy")
args_2$formula <- R(country,date) ~  1 + lockdown
args_2$prior <- rstanarm::normal(location=0,scale=.5)
args_2$prior_intercept <- rstanarm::normal(location=0,scale=2)
fit_2 <- do.call("epim", args_2)
grid.arrange(plot_obs(fit_2, type="deaths"), plot_infections(fit_2), plot_rt(fit_2),nrow=3)


