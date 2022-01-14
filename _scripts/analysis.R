#### Prepare workspace #### 

# clear workspace & load libraries
rm(list=ls(all=TRUE))


library(abind)
library(bayestestR)
library(car)
library(dplyr)
library(posterior)
library(readxl)
library(rstan)
library(tidyr)
library(writexl)


# Stan options
rstan_options(auto_write = TRUE)
options(mc.cores = 4) #number of cores used for sampling - ADAPT TO LOCAL HARDWARE CAPACITY!


#### Functions ####

# characteristics summary function
subject.summary <- function(v1, digits){
  var.mean <- round(mean(v1), digits = digits) #mean
  var.sd <- round(sd(v1), digits = digits) #sd
  var.min <- round(min(v1), digits = digits) #min
  var.max <- round(max(v1), digits = digits) #max
  paste0(var.mean, " +/- ", var.sd, " [", var.min, " - ", var.max, "]") #output
}


#pop.sd function
pop.sd <- function(df1, df2){
  sqrt(((length(df1)-1)*sd(df1)^2 + (length(df2)-1)*sd(df2)^2) / 
         (length(df1)+length(df2)-2))
}


#### load data #### 

# read raw data
data_directory <- ("./_data/S2 Raw Data.xlsx")
rawdata <- as.data.frame(read_xlsx(data_directory, col_names = T))

# adapt data types
rawdata$gender <- as.factor(rawdata$gender)

# view raw data structure
str(rawdata)


#### Subject characteristics ####

men.subject.summary <- c(
  sum(rawdata$gender == "m"),
  subject.summary(rawdata$`age(yrs)`[rawdata$gender == "m"], 1),
  subject.summary(rawdata$BP_experience[rawdata$gender == "m"], 1),
  subject.summary(rawdata$`height(cm)`[rawdata$gender == "m"], 1),
  subject.summary(rawdata$`body_mass(kg)`[rawdata$gender == "m"], 1)
)
women.subject.summary <- c(
  sum(rawdata$gender == "f"),
  subject.summary(rawdata$`age(yrs)`[rawdata$gender == "f"], 1),
  subject.summary(rawdata$BP_experience[rawdata$gender == "f"], 1),
  subject.summary(rawdata$`height(cm)`[rawdata$gender == "f"], 1),
  subject.summary(rawdata$`body_mass(kg)`[rawdata$gender == "f"], 1)
)

men.t1.summary <- c(
  subject.summary(rawdata$t1_1RM_true[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t1_1RM_rel[rawdata$gender == "m"], 2),
  subject.summary(rawdata$t2_reps_90[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t2_reps_80[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t2_reps_70[rawdata$gender == "m"], 1)
)
women.t1.summary <- c(
  subject.summary(rawdata$t1_1RM_true[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t1_1RM_rel[rawdata$gender == "f"], 2),
  subject.summary(rawdata$t2_reps_90[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t2_reps_80[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t2_reps_70[rawdata$gender == "f"], 1)
)

men.t2.summary <- c(
  subject.summary(rawdata$t3_1RM_true[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t3_1RM_rel[rawdata$gender == "m"], 2),
  subject.summary(rawdata$t4_reps_90[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t4_reps_80[rawdata$gender == "m"], 1),
  subject.summary(rawdata$t4_reps_70[rawdata$gender == "m"], 1)
)
women.t2.summary <- c(
  subject.summary(rawdata$t3_1RM_true[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t3_1RM_rel[rawdata$gender == "f"], 2),
  subject.summary(rawdata$t4_reps_90[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t4_reps_80[rawdata$gender == "f"], 1),
  subject.summary(rawdata$t4_reps_70[rawdata$gender == "f"], 1)
)

subject.descriptives <- data.frame(Variable = c("N", "Age (yrs)", "Experience in bench press (yrs)", 
                                                "Height (cm)", "Body mass (kg)"), 
                                   Men = men.subject.summary,
                                   Women = women.subject.summary
)

performance.descriptives <- data.frame(Variable = c("1-RM (kg)", "Relative 1-RM (kg.kg-1)", "Repetitions at 90% 1-RM",
                                            "Repetitions at 80% 1-RM", "Repetitions at 70% 1-RM"),
                               men.T1 = men.t1.summary, men.T2 = men.t2.summary, 
                               women.T1 = women.t1.summary, women.T2 = women.t2.summary
)

print(subject.descriptives)
print(performance.descriptives)


#### Prepare data ####

nSubjects = length(rawdata$subject_ID)

data <- data.frame(subject = 1:length(rawdata$subject_ID), 
                   Sex = as.numeric(rawdata$gender)-1,
                   t1_1RM = rawdata$t1_1RM_true, 
                   t1_r90 = rawdata$t2_reps_90,
                   t1_r80 = rawdata$t2_reps_80,
                   t1_r70 = rawdata$t2_reps_70,
                   t2_1RM = rawdata$t3_1RM_true, 
                   t2_r90 = rawdata$t4_reps_90,
                   t2_r80 = rawdata$t4_reps_80,
                   t2_r70 = rawdata$t4_reps_70)

datalong <- gather(data, key = "variable", value = "value",
                   t1_1RM:t2_r70, factor_key = TRUE)

datalong <- cbind(datalong, time = c(rep(1, 4*nSubjects), rep(2, 4*nSubjects)))

datalong$variable <- recode(datalong$variable, "c('t1_1RM', 't2_1RM') = '1RM'")
datalong$variable <- recode(datalong$variable, "c('t1_r90', 't2_r90') = 'r90'")
datalong$variable <- recode(datalong$variable, "c('t1_r80', 't2_r80') = 'r80'")
datalong$variable <- recode(datalong$variable, "c('t1_r70', 't2_r70') = 'r70'")


str(datalong)

data_1RM <- datalong[datalong$variable == "1RM",]
data_reps <- datalong[datalong$variable != "1RM",]

data_1RM$time <- as.factor(data_1RM$time)
data_reps$time <- as.factor(data_reps$time)

str(data_1RM)
str(data_reps)


#### Calculate reproducibility of performance measures ####

# prepare data
PM <- c("1RM", "r90", "r80", "r70")
P.list<- list(cbind(data$t1_1RM, data$t2_1RM), 
              cbind(data$t1_r90, data$t2_r90), 
              cbind(data$t1_r80, data$t2_r80), 
              cbind(data$t1_r70, data$t2_r70))


# non-default priors (specific to the performance measure)
prior.mu.list <- list(c(50, 50), c(4, 2), c(8, 4), c(11, 6))


# sampling specifications
nIter     = 6000 #iterations per chain
nChains   = 4 #number of chains
nWarmup   = floor(nIter/3) #warm-up (not used in the posterior)
nThin     = 1 #period of saving samples
ar        = c(0.9, 0.8, 0.8, 0.8) #acceptance rate for performance measures


for (i in 1:length(PM)){
  
  # create performance datalist for Stan with default priors (identical for all performance measures)
  dataList <- list(
    nSubjects = nSubjects,
    prior_mu = prior.mu.list[[i]],
    prior_t = c(0, 10),
    prior_sigma_e = c(0, 10),
    prior_sigma_s = c(0, 10),
    P = P.list[[i]]
  )
  
  
  # run model
  fit.tmp <- stan("_scripts/RandomEffectsModel.stan", 
       data    = dataList, 
       chains  = nChains,
       iter    = nIter,
       warmup  = nWarmup,
       thin    = nThin,
       control = list(adapt_delta = ar[i]),
       init    = "random"
  )
  
  assign(paste0("fit_", PM[i]), fit.tmp) #save model with new label
  rm(fit.tmp) #delete temporary model
  
}


#### ICC ####

digits <- 2
CrInt <- 0.9

ICC_fit_1RM <- unlist(rstan::extract(object = fit_1RM, pars = "ICC", inc_warmup = FALSE))
ICC_fit_r90 <- unlist(rstan::extract(object = fit_r90, pars = "ICC", inc_warmup = FALSE))
ICC_fit_r80 <- unlist(rstan::extract(object = fit_r80, pars = "ICC", inc_warmup = FALSE))
ICC_fit_r70 <- unlist(rstan::extract(object = fit_r70, pars = "ICC", inc_warmup = FALSE))

ICC <- cbind(
  paste0(round(map_estimate(ICC_fit_1RM)[1], digits), " [", 
         round(hdi(ICC_fit_1RM, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(ICC_fit_1RM, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(ICC_fit_r90)[1], digits), " [", 
         round(hdi(ICC_fit_r90, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(ICC_fit_r90, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(ICC_fit_r80)[1], digits), " [", 
         round(hdi(ICC_fit_r80, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(ICC_fit_r70, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(ICC_fit_r70)[1], digits), " [", 
         round(hdi(ICC_fit_r70, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(ICC_fit_r70, ci = CrInt)[1,3], digits), "]")
)

#### SEM ####

digits <- 1

SEM_fit_1RM <- unlist(rstan::extract(object = fit_1RM, pars = "sigma", inc_warmup = FALSE))
SEM_fit_r90 <- unlist(rstan::extract(object = fit_r90, pars = "sigma", inc_warmup = FALSE))
SEM_fit_r80 <- unlist(rstan::extract(object = fit_r80, pars = "sigma", inc_warmup = FALSE))
SEM_fit_r70 <- unlist(rstan::extract(object = fit_r70, pars = "sigma", inc_warmup = FALSE))

SEM <- cbind(
  paste0(round(map_estimate(SEM_fit_1RM)[1], digits), " [", 
         round(hdi(SEM_fit_1RM, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEM_fit_1RM, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEM_fit_r90)[1], digits), " [", 
         round(hdi(SEM_fit_r90, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEM_fit_r90, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEM_fit_r80)[1], digits), " [", 
         round(hdi(SEM_fit_r80, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEM_fit_r70, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEM_fit_r70)[1], digits), " [", 
         round(hdi(SEM_fit_r70, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEM_fit_r70, ci = CrInt)[1,3], digits), "]")
)

# calculate 90% quantile
round(quantile(SEM_fit_1RM, CrInt), digits)
round(quantile(SEM_fit_r90, CrInt), digits)
round(quantile(SEM_fit_r80, CrInt), digits)
round(quantile(SEM_fit_r70, CrInt), digits)


#### SEP ####

SEP_fit_1RM <- pop.sd(data_1RM$value[data_1RM$time == 1], data_1RM$value[data_1RM$time == 2]) * sqrt(1 - ICC_fit_1RM ^ 2)
SEP_fit_r90 <- pop.sd(data_reps$value[data_reps$variable == "r90" & data_reps$time == 1], data_reps$value[data_reps$variable == "r90" & data_reps$time == 2]) * sqrt(1 - ICC_fit_r90 ^ 2)
SEP_fit_r80 <- pop.sd(data_reps$value[data_reps$variable == "r80" & data_reps$time == 1], data_reps$value[data_reps$variable == "r80" & data_reps$time == 2]) * sqrt(1 - ICC_fit_r80 ^ 2)
SEP_fit_r70 <- pop.sd(data_reps$value[data_reps$variable == "r70" & data_reps$time == 1], data_reps$value[data_reps$variable == "r70" & data_reps$time == 2]) * sqrt(1 - ICC_fit_r70 ^ 2)

SEP <- cbind(
  paste0(round(map_estimate(SEP_fit_1RM)[1], digits), " [", 
         round(hdi(SEP_fit_1RM, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEP_fit_1RM, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEP_fit_r90)[1], digits), " [", 
         round(hdi(SEP_fit_r90, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEP_fit_r90, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEP_fit_r80)[1], digits), " [", 
         round(hdi(SEP_fit_r80, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEP_fit_r70, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(SEP_fit_r70)[1], digits), " [", 
         round(hdi(SEP_fit_r70, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(SEP_fit_r70, ci = CrInt)[1,3], digits), "]")
)


#### WSCV ####

WSCV_fit_1RM <- unlist(rstan::extract(object = fit_1RM, pars = "sigma", inc_warmup = FALSE)) / 
  unlist(rstan::extract(object = fit_1RM, pars = "mu", inc_warmup = FALSE)) * 100
WSCV_fit_r90 <- unlist(rstan::extract(object = fit_r90, pars = "sigma", inc_warmup = FALSE)) / 
  unlist(rstan::extract(object = fit_r90, pars = "mu", inc_warmup = FALSE)) * 100
WSCV_fit_r80 <- unlist(rstan::extract(object = fit_r80, pars = "sigma", inc_warmup = FALSE)) / 
  unlist(rstan::extract(object = fit_r80, pars = "mu", inc_warmup = FALSE)) * 100
WSCV_fit_r70 <- unlist(rstan::extract(object = fit_r70, pars = "sigma", inc_warmup = FALSE)) / 
  unlist(rstan::extract(object = fit_r70, pars = "mu", inc_warmup = FALSE)) * 100

WSCV <- cbind(
  paste0(round(map_estimate(WSCV_fit_1RM)[1], digits), " [", 
         round(hdi(WSCV_fit_1RM, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(WSCV_fit_1RM, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(WSCV_fit_r90)[1], digits), " [", 
         round(hdi(WSCV_fit_r90, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(WSCV_fit_r90, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(WSCV_fit_r80)[1], digits), " [", 
         round(hdi(WSCV_fit_r80, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(WSCV_fit_r80, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(WSCV_fit_r70)[1], digits), " [", 
         round(hdi(WSCV_fit_r70, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(WSCV_fit_r70, ci = CrInt)[1,3], digits), "]")
)



#### Time effect ####

digits = 1

time_fit_1RM <- unlist(rstan::extract(object = fit_1RM, pars = "time", inc_warmup = FALSE))
time_fit_r90 <- unlist(rstan::extract(object = fit_r90, pars = "time", inc_warmup = FALSE))
time_fit_r80 <- unlist(rstan::extract(object = fit_r80, pars = "time", inc_warmup = FALSE))
time_fit_r70 <- unlist(rstan::extract(object = fit_r70, pars = "time", inc_warmup = FALSE))

time <- cbind(
  paste0(round(map_estimate(time_fit_1RM)[1], digits), " [", 
         round(hdi(time_fit_1RM, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(time_fit_1RM, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(time_fit_r90)[1], digits), " [", 
         round(hdi(time_fit_r90, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(time_fit_r90, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(time_fit_r80)[1], digits), " [", 
         round(hdi(time_fit_r80, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(time_fit_r80, ci = CrInt)[1,3], digits), "]"),
  paste0(round(map_estimate(time_fit_r70)[1], digits), " [", 
         round(hdi(time_fit_r70, ci = CrInt)[1,2], digits), ", ", 
         round(hdi(time_fit_r70, ci = CrInt)[1,3], digits), "]")
)


# calculate 90% quantile
round(quantile(time_fit_1RM, CrInt), digits)
round(quantile(time_fit_r90, CrInt), digits)
round(quantile(time_fit_r80, CrInt), digits)
round(quantile(time_fit_r70, CrInt), digits)


#### Reliability table ####

rel.table <- rbind(time, SEM, WSCV, SEP, ICC)
colnames(rel.table) <- c("1-RM (kg)", "RTF at 90% 1-RM", "RTF at 80% 1-RM", "RTF at 70% 1-RM")
rownames(rel.table) <- c("time", "SEM", "WSCV", "SEP", "ICC")

print(rel.table)


#### Calculate reproducibility of strength-endurance models ####

models <-  c("lin", "ex2", "ex3", "crt")

# model directories
model.agree.lin = '_scripts/linear_model_agreement.stan'
model.agree.ex2 = '_scripts/exponential_2P_model_agreement.stan'
model.agree.ex3 = '_scripts/exponential_3P_model_agreement.stan'
model.agree.crt = '_scripts/critical_load_model_agreement.stan'


# non-default priors (model-specific)
prior.alpha.list <- list(c(100.8, 8.5), c(102.1, 7.7), c(55.2, 102.1), c(21.4, 72.4))
prior.beta.list <- list(c(-2.65, 1.8), c(-0.032, 0.023), c(-0.085, 0.222), c(-22.1, 48.0))
prior.gamma.list <- list(c(NA, NA), c(NA, NA), c(48.7, 108.4), c(9.2, 127.1))
prior.d_alpha.list <- list(c(0, 10), c(0, 10), c(0, 50), c(0, 50))
prior.d_beta.list <- list(c(0, 10), c(0, 10), c(0, 05), c(0, 50))
prior.d_gamma.list <- list(c(NA, NA), c(NA, NA), c(0, 50), c(0, 50))


# sampling specifications
nIter     = 6000 #iterations per chain
nChains   = 4 #number of chains
nWarmup   = floor(nIter/3) #warm-up (not used in the posterior)
nThin     = 1 #period of saving samples
ar        = c(0.9, 0.9, 0.85, 0.9) #acceptance rate for each model


for (i in 1:length(models)){
  
  # create performance datalist for Stan with default priors (identical across all models)
  dataList <- list(
    nSubjects = nSubjects,
    prior_sigma = c(0, 10),
    prior_tau_u1 = c(0, 10),
    prior_tau_u2 = c(0, 10),
    prior_tau_u3 = c(0, 10),
    prior_tau_v1 = c(0, 10),
    prior_tau_v2 = c(0, 10),
    prior_tau_v3 = c(0, 10),
    prior_alpha = prior.alpha.list[[i]],
    prior_beta = prior.beta.list[[i]], 
    prior_gamma = prior.gamma.list[[i]], 
    prior_d_alpha = prior.d_alpha.list[[i]],
    prior_d_beta = prior.d_beta.list[[i]],
    prior_d_gamma = prior.d_gamma.list[[i]],
    Load = abind(
      cbind(rep(100, nSubjects), 
            rawdata$t2_load_90 / rawdata$t1_1RM_true * 100, 
            rawdata$t2_load_80 / rawdata$t1_1RM_true * 100, 
            rawdata$t2_load_70 / rawdata$t1_1RM_true * 100),
      cbind(rep(100, nSubjects), 
            rawdata$t4_load_90 / rawdata$t3_1RM_true * 100, 
            rawdata$t4_load_80 / rawdata$t3_1RM_true * 100, 
            rawdata$t4_load_70 / rawdata$t3_1RM_true * 100),
      along = 3
    ),
    Repetitions = abind(
      cbind(rep(1, nSubjects), rawdata$t2_reps_90, rawdata$t2_reps_80, rawdata$t2_reps_70),
      cbind(rep(1, nSubjects), rawdata$t4_reps_90, rawdata$t4_reps_80, rawdata$t4_reps_70),
      along = 3
    )
  )
  
  model.tmp <-eval(parse(text = paste0("model.agree.", models[i])))

  # run model
  fit.tmp <- stan(model.tmp,
                  data    = dataList,
                  chains  = nChains,
                  iter    = nIter,
                  warmup  = nWarmup,
                  thin    = nThin,
                  control = list(adapt_delta = ar[i]),
                  init    = "random",
                  seed    = seed[i]
  )

  assign(paste0("fit_", models[i]), fit.tmp) #save model with new label
  rm(fit.tmp) #delete temporary model
  
}


#### Model diagnostics ####

for (i in 1: length(models)){
  
  fit <- eval(parse(text = paste0("fit_", models[i])))
  
  # Rhat and ESS
  check_hmc_diagnostics(fit)
  chain_diagnostics <- summarise_draws(fit, "rhat", "ess_bulk", "ess_tail")
  chain_diagnostics[which.max(chain_diagnostics$rhat),] # maximum rhat
  chain_diagnostics[which.min(chain_diagnostics$ess_bulk),] # minimum ess_bulk
  chain_diagnostics[which.min(chain_diagnostics$ess_tail),] # minimum ess_tail
  
  chain_diagnostics <- na.omit(chain_diagnostics)
  chain_diagnostics <- chain_diagnostics[order(-chain_diagnostics$ess_bulk),]
  write_xlsx(chain_diagnostics, path = paste0("_outputs/diagnostics_", models[i], ".xlsx"), col_names = TRUE)
  
  # traceplots
  # for (j in 1:length(names(fit))){
  #   assign(paste0("traceplot.", models[i]), rstan::traceplot(fit, par = names(fit)[j], ncol = 1, inc_warmup = F, alpha = 0.5))
  #   ggsave(plot = eval(parse(text = paste0("traceplot.", models[i]))), paste0("_plots/traceplot_", models[i], "_", j, ".tiff"), 
  #          width = 6, height = 3, units = "in", device="tiff", dpi = 100)
  # }
}


#### Extract delta PPDs ####

samples <- 0

for (i in 1:length(models)){
  fit <- eval(parse(text = paste0("fit_", models[i])))
  n.tmp <- sum(fit@sim$n_save) - sum(fit@sim$warmup2)
  if (n.tmp > samples){
    samples <- n.tmp
  }
}

PPD.df <- as.data.frame(matrix(data = 0, nrow = samples, ncol = 10))
colnames(PPD.df) <- c("lin.a", "lin.b", "ex2.a", "ex2.b", "ex3.a", "ex3.b", "ex3.c", "crt.Lprime", "crt.k", "crt.CL")

j <- 1

for (i in 1:4){
  
  ChooseModel <- eval(parse(text = paste0("fit_", models[i])))
  
  PPD.delta_a <- unlist(rstan::extract(object = ChooseModel, pars = "sim[1]", inc_warmup = FALSE))
  PPD.tau_a <- unlist(rstan::extract(object = ChooseModel, pars = "tau_u[1]", inc_warmup = FALSE))
  PPD.delta_b <- unlist(rstan::extract(object = ChooseModel, pars = "sim[2]", inc_warmup = FALSE))
  PPD.tau_b <- unlist(rstan::extract(object = ChooseModel, pars = "tau_u[2]", inc_warmup = FALSE))
  
  PPD.df[,j] <- PPD.delta_a / PPD.tau_a
  j <- j + 1
  PPD.df[,j] <- PPD.delta_b / PPD.tau_b
  j <- j + 1
  
  
  if (i > 2){
    
    PPD.delta_c <- unlist(rstan::extract(object = ChooseModel, pars = "sim[3]", inc_warmup = FALSE))
    PPD.tau_c <- unlist(rstan::extract(object = ChooseModel, pars = "tau_u[3]", inc_warmup = FALSE))
    
    PPD.df[,j] <- PPD.delta_c / PPD.tau_c
    j <- j + 1
    
  }
  
}


# Summarize PPDs
PPD_summary.df <- as.data.frame(matrix(0, nrow = 10, ncol = 3))
colnames(PPD_summary.df) <- c("MAP", "HDI_LL", "HDI_UL")
rownames(PPD_summary.df) <- c("lin.a", "lin.b", "ex2.a", "ex2.b", "ex3.a", "ex3.b", "ex3.c", "crt.Lprime", "crt.k", "crt.CL")

digits = 2
CrInt = 0.9

for (i in 1:10){
  
  PPD_summary.df[i, 1] <- round(map_estimate(PPD.df[, i], precision = 4^12), digits)
  PPD_summary.df[i, 2] <- round(hdi(PPD.df[, i], ci = CrInt)[1,2], digits)
  PPD_summary.df[i, 3] <- round(hdi(PPD.df[, i], ci = CrInt)[1,3], digits)
  
}

output <-as.data.frame(matrix(0, 10, 1))
for (i in 1:10){
  
  output[i, 1] <- paste0(PPD_summary.df[i, 1], " [", PPD_summary.df[i, 2], ", ", PPD_summary.df[i, 3], "]")
  
}
output$model.parameter <- rownames(PPD_summary.df)
colnames(output) <- paste0("MAP [", CrInt*100, "% HDI]")
output <- output[, c(2, 1)]

write_xlsx(output, path ="./_outputs/PPD_summary.xlsx")


# Evaluate PPDs against ROPE
ROPE = 0.6
digits = 3

ROPE.df <- as.data.frame(matrix(0, nrow = 10, ncol = 5))
colnames(ROPE.df) <- c("p_trivial", "p_negative", "p_positive", "p<0", "p>0")
rownames(ROPE.df) <- c("lin.a", "lin.b", "ex2.a", "ex2.b", "ex3.a", "ex3.b", "ex3.c", "crt.Lprime", "crt.k", "crt.CL")

for (i in 1:10){
  
  ROPE.df[i, 1] <- round(rope(PPD.df[, i], range = c(-ROPE, ROPE), ci = 1)$ROPE_Percentage, digits)
  ROPE.df[i, 2] <- round(rope(PPD.df[, i], range = c(-Inf, -ROPE), ci = 1)$ROPE_Percentage, digits)
  ROPE.df[i, 3] <- round(rope(PPD.df[, i], range = c(ROPE, Inf), ci = 1)$ROPE_Percentage, digits)
  ROPE.df[i, 4] <- round(rope(PPD.df[, i], range = c(-Inf, 0), ci = 1)$ROPE_Percentage, digits)
  ROPE.df[i, 5] <- round(rope(PPD.df[, i], range = c(0, Inf), ci = 1)$ROPE_Percentage, digits)
  
}

ROPE.df <- ROPE.df * 100

write_xlsx(ROPE.df, path ='./_outputs/PPD_ROPE.xlsx')


#### Calculate relative changes ####

delta_std.df <- as.data.frame(matrix(data = 0, nrow = samples, ncol = 10))
colnames(delta_std.df) <- c("lin.a", "lin.b", "ex2.a", "ex2.b", "ex3.a", "ex3.b", "ex3.c", "crt.Lprime", "crt.k", "crt.CL")

j <- 1

for (i in 1:4){
  
  ChooseModel <- eval(parse(text = paste0("fit_", models[i])))
  
  d_a_rn <- unlist(rstan::extract(object = ChooseModel, pars = "sim[1]", inc_warmup = FALSE))
  alpha <- unlist(rstan::extract(object = ChooseModel, pars = "alpha", inc_warmup = FALSE))
  
  d_b_rn <- unlist(rstan::extract(object = ChooseModel, pars = "sim[2]", inc_warmup = FALSE))
  beta <- unlist(rstan::extract(object = ChooseModel, pars = "beta", inc_warmup = FALSE))
  
  
  delta_a <- d_a_rn / abs(alpha)
  delta_b <- d_b_rn / abs(beta)
  
  delta_std.df[,j] <- delta_a
  j <- j + 1
  delta_std.df[,j] <- delta_b
  j <- j + 1
  
  
  if (i > 2){
    
    d_c_rn <- unlist(rstan::extract(object = ChooseModel, pars = "sim[3]", inc_warmup = FALSE))
    gamma <- unlist(rstan::extract(object = ChooseModel, pars = "gamma", inc_warmup = FALSE))
    
    
    delta_c <- d_c_rn / abs(gamma)
    
    delta_std.df[,j] <- delta_c
    j <- j + 1
    
  }
  
}


# Summarize relative changes
delta_std_summary.df <- as.data.frame(matrix(0, nrow = 10, ncol = 3))
colnames(delta_std_summary.df) <- c("MAP", "HDI_LL", "HDI_UL")
rownames(delta_std_summary.df) <- c("lin.a", "lin.b", "ex2.a", "ex2.b", "ex3.a", "ex3.b", "ex3.c", "crt.Lprime", "crt.k", "crt.CL")

digits = 3

for (i in 1:10){
  
  delta_std_summary.df[i, 1] <- round(map_estimate(delta_std.df[, i], precision = 4^12), digits)*100
  delta_std_summary.df[i, 2] <- round(hdi(delta_std.df[, i], ci = CrInt)[1,2], digits)*100
  delta_std_summary.df[i, 3] <- round(hdi(delta_std.df[, i], ci = CrInt)[1,3], digits)*100
  
}

output <-as.data.frame(matrix(0, 10, 1))
for (i in 1:10){
  
  output[i, 1] <- paste0(delta_std_summary.df[i, 1], " [", delta_std_summary.df[i, 2], ", ", delta_std_summary.df[i, 3], "]")
  
}
output$model.parameter <- rownames(PPD_summary.df)
colnames(output) <- paste0("MAP [", CrInt*100, "% HDI]")
output <- output[, c(2, 1)]

write_xlsx(output, path ='./_outputs/delta_pct_summary.xlsx')
