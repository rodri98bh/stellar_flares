library(rstan)
library(dplyr)
library(ggpubr)
library(ggplot2)
options(mc.cores = parallel::detectCores()/2)
rstan_options(auto_write = TRUE)
source("./R/misc.R") # Helper functions


# Star brightness data
rawdata <- read.csv("./Data/Data_brightness.csv")[,c("time","pdcsap_flux")]

# Center flux and mark missing observations
rawdata[,2] <- rawdata[,2] - mean(rawdata[,2], na.rm = TRUE)
observed <- (!is.na(rawdata[,2])) * 1
rawdata[is.na(rawdata[,2]),2] <- 0
N <- nrow(rawdata)
tt <- rawdata[,1]

# Data list for Stan
QFD_data <- list(N=N, t = rawdata[,1],
                 y = rawdata[,2],
                 observed = observed,
                 sigma_prior = c(-8,8),
                 Q0_prior = c(0,8), # This values seem to work for most stars
                 dQ_prior = c(-8,8),
                 period_prior = c(-8,8),
                 f_prior = c(1e-6,1-1e-6),
                 alpha_quiet = c(1,.1), 
                 alpha_firing = c(1,1),
                 alpha_decay = c(1,.1,1),
                 mu0_quiet = 0,
                 lambda_quiet = .01,
                 gamma_noise = c(0.01,0.01),
                 mu0_rate_firing = 0,
                 sigma_rate_firing = 1e3,
                 mu0_rate_decay = 0,
                 sigma_rate_decay = 1e3,
                 diag = rep(1e-6,N)
)

# Fit celerite alone

start.time <- Sys.time()

modelcelerite <- stan_model(file = './Stan/celerite-missing-handling.stan', 
                            model_name = "celerit2", 
                            allow_undefined = TRUE,
                            includes = paste0('\n#include "', 
                                              normalizePath('./C/celerite2.hpp', mustWork = TRUE), '"\n'))

celeritedata <- QFD_data
celeritedata$err_prior <- c(0.01, 0.01)



fitcelerite <- sampling(modelcelerite, 
                        data = celeritedata,control = list(adapt_delta = 0.99, max_treedepth=15), 
                        iter = 2000,
                        init_r = 2, 
                        chains = 2)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken



