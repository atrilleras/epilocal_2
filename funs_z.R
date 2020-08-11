
up_accents <- function(x){
    #Pone en mayusculas
    x <- toupper(x) 
    #Remueve todos los acentos
    x <- stri_trans_general(x,"Latin-ASCII")
    #Remueve espacios en blanco al incio y al final
    x <- trimws(x)
    return(x)
}


get_places_names <- function (datos) {
    names(datos) <- epitrix::clean_labels(names(datos))
    datos$departamento_o_distrito <- up_accents(datos$departamento_o_distrito)
    datos$ciudad  <- up_accents(datos$ciudad_de_ubicacion)
    
    sort(c(unique(datos$departamento_o_distrito),
      unique(datos$ciudad)))
}


fCleanData <- function(datos, place, type = "country"){
  

    names(datos) <- epitrix::clean_labels(names(datos))
    datos$departamento_o_distrito <- epitrix::clean_labels(datos$departamento_o_distrito )
    datos$ciudad <- epitrix::clean_labels(datos$ciudad_de_ubicacion )
    
    if (type == "depto") {
        datos <- datos %>% filter(departamento_o_distrito == place) } 
    
    if (type == "ciudad") {
        datos <- datos %>% filter(ciudad == place)
    }
    
    datos$place <- place
    
    
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

fCleanData_col <- function(datos){
  
  
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




fEpiEstim <- function(datos_inc, rezago = 14) {
    
    datos <- datos_inc %>% filter(asymptomatic == 0)
    fechaConfiable <- as.Date(max(datos$fecha_de_notificacion)) - rezago
    
    inc_multiple  <- incidence(datos$date, groups = datos$type)[1:(max(datos$date)-min(datos$date)- rezago)] 
    inc_simple  <- incidence(datos$date)[1:(max(datos$date)-min(datos$date)- rezago)] 
    
    days <- seq_along(inc_multiple$dates)[2:(length(inc_multiple$dates)-7)]
    RTo  <- estimate_R(inc_multiple, method = "parametric_si",
                       config = make_config(list(
                           mean_si = 6.48, std_si = 3.83,
                           t_start=days, t_end=days+7)))
    
    data_rt <- RTo$R
    data_rt$Fecha_Inicio_Ventana <- min(inc_multiple$dates) + RTo$R$t_start 
    data_rt$Fecha_Fin_Ventana    <- min(inc_multiple$dates) + RTo$R$t_end 
    
    rt <- list (data_rt = data_rt,
                RTo = RTo,
                inc_multiple = inc_multiple,
                inc_simple   = inc_simple,
                fechaConfiable = fechaConfiable,
                datos = datos,
                datos_inc = datos_inc)
    
    return(rt)
}



fPlotIncProj <- function(rt)
{
    RT <- rt$RTo
    inc_simple <- rt$inc_simple
    tmp <- inc_simple %>% as.data.frame() %>% melt(id = c('dates'))
    datos_inc <- rt$datos_inc
    place <-  rt$datos$place[1]
    
    
    window_ultima_semana <- (NROW(RT$R)-10):NROW(RT$R)
    assumed_R <- EpiEstim::sample_posterior_R(RT, 
                                              n = 1000, window =window_ultima_semana )
    
    mu <- 6.48
    sigma <- 3.83
    cv <- sigma/mu
    params <- gamma_mucv2shapescale(mu, cv)
    si <- distcrete("gamma", shape = params$shape,
                    scale = params$scale,
                    interval = 1, w = 0)

    
    pred0 <- project(inc_simple,
                     R = assumed_R,
                     si = si,
                     n_days = 30,
                     n_sim = 1000,model=c("negbin"))
    
    tmp1 <- as.data.frame(apply(pred0, 1, mean))
    tmp2 <- as.data.frame(apply(pred0, 1, quantile, probs = c(0.025)))
    tmp3 <- as.data.frame(apply(pred0, 1, quantile, probs = c(0.975)))
    
    proyecciones <- cbind(row.names(tmp1), tmp1, tmp2, tmp3)
    names(proyecciones) <- c('Fecha', 'Media', 
                             'Limite_Inferior', 'Limite_Superior')
    proyecciones$Fecha <- as.character(proyecciones$Fecha) %>% as.Date()
    
    fechaReporte <- Sys.Date()
    fechaCuarentena <- as.Date("2020-03-25")
    fechaApertura   <- as.Date("2020-04-27")
    size <- 15
    
    fechaend <- Sys.Date() + 5
    date_start  <- as.Date("2020-02-25", origin = "1899-12-30")
    date_end   <-  as.Date(fechaend, origin = "1899-12-30")
    
    plot_proj <- ggplot(tmp, 
                        aes(x = dates, y = value, 
                            fill = variable)) +
        geom_bar(stat = 'identity',alpha=0.8,aes(fill = variable)) +
        geom_ribbon(data = proyecciones, 
                    aes(x = Fecha, ymin = Limite_Inferior, ymax = Limite_Superior), 
                    fill = "grey70", alpha = 0.5, inherit.aes = F) +
        geom_line(data = proyecciones, aes(x = Fecha, y = Media), 
                  colour = 'steelblue', 
                  inherit.aes = F, size = 1)+
        theme_bw(size) +
        scale_fill_brewer(palette = "Set1",name = 'Tipo de Contagio') +
        cowplot::theme_minimal_grid() +
        geom_vline(xintercept = fechaCuarentena, linetype="dashed", lwd = 0.8,
                   color = "orange") +
        geom_vline(xintercept = fechaApertura, linetype="dashed", lwd = 0.8,
                   color = "darkgreen") +
        cowplot::theme_minimal_grid() +
        scale_x_date(date_breaks = '2 weeks')+
        coord_cartesian(xlim=c(date_start,date_end)) +
        xlab("") + ylab("Incidencia") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
        ggtitle(place) +
        labs (subtitle = paste0("( n=", NROW(datos_inc), ")")) +
        theme(legend.position = "none")
        
    
    return(plot_proj)
}

fPlotRt_1 <- function(rt){
  
  numeroEfectivo <- rt$data_rt
  datos <- rt$datos
  fechaReporte <- Sys.Date()
  fechaCuarentena <- as.Date("2020-03-23")
  fechaApertura   <- as.Date("2020-04-27")
  fechaConfiable  <- rt$fechaConfiable
  fechaend <- Sys.Date() + 5
  date_start  <- as.Date("2020-03-09", origin = "1899-12-30")
  date_end   <-  as.Date(fechaend, origin = "1899-12-30")
  
  primer_caso_reportado <- min(datos$fecha_diagnostico, na.rm = T)
  
  size <- 15
  
  plot_rt <- ggplot(numeroEfectivo, aes(x = Fecha_Fin_Ventana, 
                                        y = `Mean(R)`)) + 
    geom_ribbon(aes(ymin = `Quantile.0.025(R)`, 
                    ymax = `Quantile.0.975(R)`), 
                fill = 'darkgreen', alpha = 0.4) +
    geom_line(colour = 'steelblue', size = 0.75) +
    theme_bw(size) +
    cowplot::theme_minimal_grid() +
    scale_x_date(date_breaks = '1 week') +
    geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
               color = "red") +
    geom_vline(xintercept = fechaCuarentena, linetype="dashed", lwd = 0.8,
               color = "orange") +
    geom_vline(xintercept = fechaApertura, linetype="dashed", lwd = 0.8,
               color = "darkgreen") +
    geom_vline(xintercept = fechaConfiable, linetype="dashed", lwd = 0.8,
               color = "darkblue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_point(aes(x = primer_caso_reportado, y = 2), 
               size = 6, 
               colour = '#DC143C') +
    xlab("") + ylab("R(t)") +
    coord_cartesian(ylim=c(0,5), xlim=c(date_start,date_end))
  
  
  return(plot_rt)
  
}


fPlotRt <- function(rt){
    
    numeroEfectivo <- rt$data_rt
    datos <- rt$datos
    
    place <-  rt$datos$place[1]
    fechaReporte <- Sys.Date()
    fechaCuarentena <- as.Date("2020-03-25")
    fechaApertura   <- as.Date("2020-04-27")
    fechaConfiable  <- rt$fechaConfiable
    fechaend <- Sys.Date() + 5
    date_start  <- as.Date("2020-02-25", origin = "1899-12-30")
    date_end   <-  as.Date(fechaend, origin = "1899-12-30")
    
    primer_caso_reportado <- min(datos$fecha_diagnostico, na.rm = T)
    
    size <- 15
    
    plot_rt <- ggplot(numeroEfectivo, aes(x = Fecha_Fin_Ventana, 
                                          y = `Mean(R)`)) + 
        geom_ribbon(aes(ymin = `Quantile.0.025(R)`, 
                        ymax = `Quantile.0.975(R)`), 
                    fill = 'darkgreen', alpha = 0.4) +
        geom_line(colour = 'steelblue', size = 0.75) +
        theme_bw(size) +
        cowplot::theme_minimal_grid() +
        scale_x_date(date_breaks = '1 week') +
        geom_hline(yintercept = 1, linetype="dashed", lwd = 0.8,
                   color = "red") +
        geom_vline(xintercept = fechaCuarentena, linetype="dashed", lwd = 0.8,
                   color = "orange") +
        geom_vline(xintercept = fechaApertura, linetype="dashed", lwd = 0.8,
                   color = "darkgreen") +
        geom_vline(xintercept = fechaConfiable, linetype="dashed", lwd = 0.8,
                   color = "darkblue") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_point(aes(x = primer_caso_reportado, y = 2), 
                   size = 6, 
                   colour = '#DC143C') +
        xlab("") + ylab("R(t)") +
        coord_cartesian(ylim=c(0,5), xlim=c(date_start,date_end))
        
    
    return(plot_rt)
    
}


get_estimates_depto <- function(place) {
    datos_inc <- fCleanData(dat0, place = place, type = "depto")
    rt <- fEpiEstim(datos_inc)
    plot_pr <- fPlotIncProj(rt)
    plot_rt <- fPlotRt(rt)
    plot_grid(plot_pr, plot_rt, nrow = 2, 
              rel_heights = c(1.1, 1.3),
              align = "v") 
}


get_estimates_city <- function(place) {
    datos_inc <- fCleanData(dat0, place = place, type = "ciudad")
    rt <- fEpiEstim(datos_inc)
    plot_pr <- fPlotIncProj(rt)
    plot_rt <- fPlotRt(rt)
    plot_grid(plot_pr, plot_rt, nrow = 2, 
              rel_heights = c(1.1, 1.3),
              align = "v") 
}



re_scale_intervention <-function(int,region="Choco",categorias= "transit_stations_percent_change_from_baseline"){
  
  movilidad  <- int %>% filter(sub_region_1 == region)
  movilidad  <- movilidad %>% select(date,categorias)
  movilidad  <- movilidad %>% rename(modalidad=categorias)
  movilidad  <- movilidad %>% mutate(CATEGORIA=rank(modalidad,ties.method = "average"))
  movilidad  <- movilidad %>% mutate(C=(CATEGORIA/nrow(movilidad)))
  movilidad  <- movilidad %>% mutate(x.1 = seq_along(movilidad$date))
  movilidad  <- movilidad %>% mutate(x= seq_along(movilidad$date))
  intervencion <- movilidad %>% select(x.1,x,date,C)
  intervencion$date <- as.Date(as.character(intervencion$date))
  
  return(intervencion)
}
