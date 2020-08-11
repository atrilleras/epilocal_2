##Función para unificar bases de R(t)
get_rt_unified <- function(rt_epiestim, rt_epidemia)
    
{
    rt_epidemia_50 <- rt_epidemia %>% filter(tag == "50%") %>% 
        rename(epidemia_low_50 = low, epidemia_up_50 = up, place = group) %>% dplyr::select(-tag)
    rt_epidemia_95 <- rt_epidemia %>% filter(tag == "95%") %>% 
        rename(epidemia_low_95 = low, epidemia_up_95 = up, place = group) %>% dplyr::select(-tag)
    
    
    rt_epidemia <- merge(rt_epidemia_50, rt_epidemia_95, by = c("place", "date"))
    place <- unique(rt_epidemia$place)
    
    
    rt_epiestim <- rt_epiestim %>% rename(date = Fecha_Fin_Ventana, 
                                          epiestim_mean  =`Mean(R)`, 
                                          epiestim_q0025  = `Quantile.0.025(R)`,
                                          epiestim_q0975 = `Quantile.0.975(R)`) %>% 
        mutate(place = place) %>% dplyr::select(place, date, epiestim_mean, epiestim_q0025, epiestim_q0975)
    
    rt <- merge(rt_epiestim, rt_epidemia, by = c("date", "place"),all=TRUE)
    
    rt$method_epiestim <- "R(t) EpiEstim_1_week"
    rt$method_epidemia <- "R(t) Epidemia"

    
    return(rt)
    
}

get_rt_unified_2 <- function(rt_epiestim, rt_epiestim_2, rt_epidemia)
    
{
    rt_epidemia_50 <- rt_epidemia %>% filter(tag == "50%") %>% 
        rename(epidemia_low_50 = low, epidemia_up_50 = up, place = group) %>% dplyr::select(-tag)
    rt_epidemia_95 <- rt_epidemia %>% filter(tag == "95%") %>% 
        rename(epidemia_low_95 = low, epidemia_up_95 = up, place = group) %>% dplyr::select(-tag)
    
    
    rt_epidemia <- merge(rt_epidemia_50, rt_epidemia_95, by = c("place", "date"))
    place <- unique(rt_epidemia$place)
    
    
    rt_epiestim <- rt_epiestim %>% rename(date = Fecha_Fin_Ventana, 
                                          epiestim_mean  =`Mean(R)`, 
                                          epiestim_q0025  = `Quantile.0.025(R)`,
                                          epiestim_q0975 = `Quantile.0.975(R)`) %>% 
        mutate(place = place) %>% dplyr::select(place, date, epiestim_mean, epiestim_q0025, epiestim_q0975)
    
    rt_epiestim_2 <- rt_epiestim_2 %>% rename(date = Fecha_Fin_Ventana, 
                                          epiestim_mean_2  =`Mean(R)`, 
                                          epiestim_q0025_2  = `Quantile.0.025(R)`,
                                          epiestim_q0975_2 = `Quantile.0.975(R)`) %>% 
        mutate(place = place) %>% dplyr::select(place, date, epiestim_mean_2, epiestim_q0025_2, epiestim_q0975_2)
    
    rt_1 <- merge(rt_epiestim,rt_epiestim_2, by = c("date", "place"))
    rt <- merge(rt_1 ,rt_epidemia, by = c("date", "place"), all=TRUE)
    
    rt$method_epiestim <- "R(t) EpiEstim_1_week"
    rt$method_epiestim_2 <- "R(t) EpiEstim con asintomáticos"
    rt$method_epidemia <- "R(t) Epidemia"
   
    
    return(rt)
    
}



##función para obtener base de muertes registradas
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
    #deaths <- deaths[1:(NROW(deaths)-14),]
    death_place <- deaths %>% select(date,deaths)
    return(death_place)
}
##### Función para obtener casos y muertes

get_case_death <- function(place_cases)
{
    new_cases <-
        place_cases %>% 
        group_by(fis) %>% 
        summarise(Cases = sum(cases, na.rm = TRUE)) %>% rename(date = fis)
    
    new_deaths <- 
        place_cases %>% 
        group_by(fecha_de_muerte) %>% 
        summarise(Deaths = sum(deaths, na.rm = TRUE)) %>% 
        rename(date = fecha_de_muerte) 
    
    deaths <- merge(new_cases, new_deaths, by = "date", all = TRUE) %>% 
        filter(!is.na(date)) 
    deaths$Cases[is.na(deaths$Cases)] <- 0
    deaths$Deaths[is.na(deaths$Deaths)] <- 0
    deaths <- deaths[1:(NROW(deaths)-14),]
    cases_death_place <- deaths %>% select(date,Cases,Deaths)
    return(cases_death_place)
}



###Función para obtener R(t) por Epiestim
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
    datos$date <- datos$date-5
    
fechaConfiable <- as.Date(max(datos$fecha_de_notificacion),origin = "1900-01-01") - rezago
    inc_multiple  <- incidence(datos$date, groups = datos$type)[1:(max(datos$date)-min(datos$date)- rezago)] 
    days <- seq_along(inc_multiple$dates)[2:(length(inc_multiple$dates)-7)]
    RTo  <- estimate_R(inc_multiple, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))
    data_rt <- RTo$R
    data_rt$Fecha_Inicio_Ventana <- min(inc_multiple$dates) + RTo$R$t_start 
    data_rt$Fecha_Fin_Ventana    <- (min(inc_multiple$dates) + RTo$R$t_end)
    rt <- list (data_rt = data_rt,
                RTo = RTo)
    return(rt)
}


get_rt_epiestim_2 <- function(datos)
{
    #datos$fecha_diagnostico <- as.Date(as.numeric(datos$fecha_diagnostico),
                                       #origin = "1900-01-01")
    datos_inc <- datos %>%
        mutate(date = as.Date (fis_1),
               #fecha_diagnostico=as.Date(fecha_diagnostico),
               type=case_when(tipo=="Importado"~"imported",
                              tipo=="Relacionado"~"local",
                              tipo=="En estudio"~"local"))
    datos_inc$fis_clean <- clean_labels(datos_inc$fis_1)
    #datos_inc$asymptomatic <- 0
    #datos_inc$asymptomatic[datos_inc$fis_clean ==  "asintomatico"] <- 1
   # datos <- datos_inc %>% filter(asymptomatic == 0)
    rezago <- 14
    datos_inc$date <- datos_inc$date-5
    fechaConfiable <- as.Date(max(datos$fecha_de_notificacion),origin = "1900-01-01") - rezago
    inc_multiple  <- incidence(datos_inc$date, groups = datos_inc$type)[1:(max(datos_inc$date)-min(datos_inc$date)- rezago)] 
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

###Función para aplicar epidemia
fit_epidemia <- function (datos_epidemia,country=" ",pop=345678){
    
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
    args$sampling_args <- list(iter=600,control=list(adapt_delta=0.95,max_treedepth=15),seed=12345)
    args$group_subset <- c(country)
    args$formula <- R(country,date) ~  0 + rw(time=week,prior_scale=0.1)
    args$prior <- rstanarm::normal(location=0,scale=.5)
    args$prior_intercept <- rstanarm::normal(location=0,scale=2)
    fit <- do.call("epim", args)
    
    return(fit)
}

###Plot R(t)
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
        theme(legend.position = c(0.7, 0.8), legend.title = element_blank())
    
    return(plot_rt_comp)
}

fCleanData_2 <- function(datos){
    names(datos) <- epitrix::clean_labels(names(datos))
    datos$departamento_o_distrito <- epitrix::clean_labels(datos$departamento_o_distrito )
    datos$ciudad <- epitrix::clean_labels(datos$ciudad_de_ubicacion )
    
    datos_inc <- datos %>%
        mutate(date = as.Date (fis),
               fecha_diagnostico=as.Date(fecha_diagnostico),
               type=case_when(tipo=="Importado"~"imported",
                              tipo=="Relacionado"~"local",
                              tipo=="En estudio"~"local"))
    datos_inc$date[is.na(datos_inc$date) ] <- datos_inc$fecha_diagnostico
    
    datos_inc$fis_clean <- clean_labels(datos_inc$fis)
    datos_inc$asymptomatic <- 0
    datos_inc$asymptomatic[datos_inc$fis_clean ==  "asintomatico"] <- 1
    datos_inc$fecha_de_muerte <- as.Date(datos_inc$fecha_de_muerte)
    datos_inc$fis <- as.Date(datos_inc$fis)
    datos_inc$cases  <- 1
    datos_inc$deaths <- NA
    datos_inc$deaths[datos_inc$estado == "Fallecido"] <- 1
    
    return(datos_inc)
    
}

###Datos especiales
##Distribución de tiempo de inicio de síntomas hasta muerte
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
