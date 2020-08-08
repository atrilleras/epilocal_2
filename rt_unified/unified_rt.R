

###plot rt_Epiestim

# test <- list(numeroEfectivo,data_bogota)
# saveRDS(test,"test.RDS")))

rm(list=ls())

test <- readRDS("test.RDS")
rt_epiestim <- test [[1]]# numeroEfectivo
rt_epidemia <- test [[2]] #data_bogota
rm(test)

bogota <- readRDS("bogota.RDS")
rm(bogota)

####Extarer bases
base_bogota <- readRDS("base_bogota.RDS")
rt_epiestim <- base_bogota[[1]]# numeroEfectivo
rt_epidemia <- base_bogota[[2]] #data_bogota
rm(base_bogota)

source("rt_unified/functions.R")

rtu <- get_rt_unified(rt_epiestim, rt_epidemia)

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


