library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(dplyr)
library(tidyr)
library(EnvStats)
library(optparse)
library(stringr)
library(bayesplot)
library(matrixStats)
library(scales)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(ggplot2)
library(abind)
library(openxlsx)
library(incidence)
rm(list=ls())
source("replicar_nature/utils/process_covariates_3.r")
source("funs_z.R")

# Commandline options and parsing
parser <- OptionParser()
parser <- add_option(parser, c("-D", "--debug"), action="store_true",
                     help="Perform a debug run of the model")
parser <- add_option(parser, c("-F", "--full"), action="store_true",
                     help="Perform a full run of the model")
cmdoptions <- parse_args(parser, args = commandArgs(trailingOnly = TRUE), positional_arguments = TRUE)

# Default run parameters for the model
if(is.null(cmdoptions$options$debug)) {
  DEBUG = Sys.getenv("DEBUG") == "TRUE"
} else {
  DEBUG = cmdoptions$options$debug
}

# Sys.setenv(FULL = "TRUE")
if(is.null(cmdoptions$options$full)) {
  FULL = Sys.getenv("FULL") == "TRUE"
} else {
  FULL = cmdoptions$options$full
}

if(DEBUG && FULL) {
  stop("Setting both debug and full run modes at once is invalid")
}

if(length(cmdoptions$args) == 0) {
  StanModel = 'base-nature'
} else {
  StanModel = cmdoptions$args[1]
}

print(sprintf("Running %s",StanModel))
if(DEBUG) {
  print("Running in DEBUG mode")
} else if (FULL) {
  print("Running in FULL mode")
}

cat(sprintf("Running:\nStanModel = %s\nDebug: %s\n",
            StanModel,DEBUG))

###Subir datos
datos <-loadWorkbook("replicar_nature/data/datos_colombia.xlsx")
#d     <- readRDS("d.R")
#d$DateRep <- as.Date(as.numeric(d$DateRep), origin="1899-12-30")
d <- read.xlsx(datos,"casos") %>% as.data.frame()
d$DateRep <- as.Date(as.numeric(d$DateRep), origin="1899-12-30")
ifr.by.country <- read.xlsx(datos,"ifr_by_country") %>% as.data.frame()
ifr.by.country$ifr <- as.numeric(ifr.by.country$ifr)
interventions  <- read.xlsx(datos,"intervenciones_2") %>% as.data.frame()
interventions$schools_universities <- as.Date(as.numeric(interventions$schools_universities), origin="1899-12-30")
interventions$self_isolating_if_ill <- as.Date(as.numeric(interventions$self_isolating_if_ill), origin="1899-12-30")
interventions$public_events <- as.Date(as.numeric(interventions$public_events), origin="1899-12-30")
interventions$lockdown <- as.Date(as.numeric(interventions$lockdown), origin="1899-12-30")
interventions$social_distancing_encouraged <- as.Date(as.numeric(interventions$social_distancing_encouraged), origin="1899-12-30")


serial.interval <-read.xlsx(datos,"si_europa") %>% as.data.frame()
#countries <- data.frame(Regions = ifr.by.country$country)
countries <- data.frame(Regions=c("Bogota","Cali","Medellin","Barranquilla","Cartagena",
                                  "Cundinamarca","Valle","Antioquia","Bolivar","Atlantico","Narino"))
# Read which countires to use
#countries <- readRDS('nature/data/regions.rds')
# Read deaths data for regions
#d <- readRDS('nature/data/COVID-19-up-to-date.rds')
# Read IFR and pop by country
#ifr.by.country <- readRDS('nature/data/popt-ifr.rds')

# Read interventions
#interventions <- readRDS('nature/data/interventions.rds')

forecast <- 0 # increase to get correct number of days to simulate
# Maximum number of days to simulate
N2 <- (max(d$DateRep) - min(d$DateRep) + 1 + forecast)[[1]]

processed_data <- process_covariates_3(countries = countries, interventions = interventions, 
                                     d = d , ifr.by.country = ifr.by.country, N2 = N2,serial.interval)

stan_data = processed_data$stan_data
dates = processed_data$dates
deaths_by_country = processed_data$deaths_by_country
reported_cases = processed_data$reported_cases
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('replicar_nature/stan-models/',StanModel,'.stan'))

if(DEBUG) {
  fit = sampling(m,data=stan_data,iter=50,warmup=20,chains=10)
} else if (FULL) {
  fit = sampling(m,data=stan_data,iter=1800,warmup=1000,chains=5,thin=1,control = list(adapt_delta = 0.99, max_treedepth = 20))
} else { 
  fit = sampling(m,data=stan_data,iter=600,warmup=300,chains=4,thin=1,control = list(adapt_delta = 0.95, max_treedepth = 10))
}   

out = rstan::extract(fit)
prediction = out$prediction
estimated.deaths = out$E_deaths
estimated.deaths.cf = out$E_deaths0

JOBID = Sys.getenv("PBS_JOBID")
if(JOBID == "")
  JOBID = as.character(abs(round(rnorm(1) * 1000000)))
print(sprintf("Jobid = %s",JOBID))

countries <- countries$Regions
save(fit,prediction,dates,reported_cases,deaths_by_country,countries,estimated.deaths,estimated.deaths.cf,stan_data, file=paste0('replicar_nature/results/',StanModel,'-',JOBID,'-stanfit.Rdata'))

#definir el objeto
filename <- paste0(StanModel,'-',JOBID)

print('Generating mu, rt plots')
mu = (as.matrix(out$mu))
colnames(mu) = countries
g = (mcmc_intervals(mu,prob = .9))
ggsave(sprintf("replicar_nature/figures/%s-mu.png",filename),g,width=4,height=6)
tmp = lapply(1:length(countries), function(i) (out$Rt_adj[,stan_data$N[i],i]))
Rt_adj = do.call(cbind,tmp)
colnames(Rt_adj) = countries
g = (mcmc_intervals(Rt_adj,prob = .9))
ggsave(sprintf("replicar_nature/figures/%s-final-rt.png",filename),g,width=4,height=6)

print("Generate 3-panel plots")
source('replicar_nature/utils/plot_3_panel.r')
make_three_panel_plot(filename)

print('Covars plots')
source('replicar_nature/utils/covariate_size_effects.r')
plot_covars(filename)

print('Making table')
source('replicar_nature/utils/make_table.r')
make_table(filename)

#####crear tablas en Escel
wb <- createWorkbook(
  creator = "Me",
  title = "title here",
  subject = "this & that",
  category = "something"
)
addWorksheet(wb, "casos")
writeData(wb,sheet="casos",d)
addWorksheet(wb, "intervenciones")
writeData(wb,sheet="intervenciones",interventions)
addWorksheet(wb, "casos")
writeData(wb,sheet="casos",incidencia)
addWorksheet(wb,"si")
writeData(wb,sheet="si",serial.interval)
saveWorkbook(wb, "casos_y_muertes.xlsx", overwrite = TRUE)
