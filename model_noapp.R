####### LIBRARIES & DATA INPUts #######
library(data.table)


###### defined parameters across the sectors ######
n.t <- 10 ## time horizon
dr <- 0.035 ## discount rate
wtp <- 20000 ## willingness to pay per QALY gained

############# model functions
inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)

model <- function(inputs){

inputs[ , value := as.numeric(as.character(value))]

human <- inputs[scenario=="human_0" | scenario=="human_1"]
animal <- inputs[scenario=="animal_0" | scenario=="animal_1"]
intervention <- inputs[scenario=="intervention"]


##### functions used across the sectors##########
f_expvalue <- function(x,y,z){
  ## x is the epi matrix
  ## y is the cost matrix
  ## z is the reward matrix
  results <- matrix(c(sum(x*y),sum(x*z)),1,2)
  return(results)
  
}

f_di <- function(x,y){
  # function to apply a discount rate
  # x is cost
  # y is discount rate 
  x2 <- x - (x*y)
  return(x2)
}


###################*****HUMAN MODEL*****###########################

state_names <- c("well", "res","sus","dead") ## the compartments
transition_names  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s")  ## the rates
parameter_names <- c(state_names, transition_names)

state_i <- c(intervention[parameter=="n_population",value], rep(0,length=length(state_names)-1))


m_param <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
colnames(m_param) <- parameter_names
rownames(m_param) <- paste("cycle", 0:(n.t-1), sep  =  "")


m_param[ , "r"] <- rep(human[parameter=="well_r",value], n.t)
m_param[ , "s"] <- rep(human[parameter=="well_s",value], n.t)
m_param[ , "mort_r"] <- rep(human[parameter=="r_dead",value], n.t)
m_param[ , "mort_s"] <- rep(human[parameter=="s_dead",value], n.t)
m_param[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]), n.t)
m_param[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]), n.t)
m_param[ , "birth"] <- rep(human[parameter=="birth_well",value], n.t)
m_param[ , "mort_w"] <- rep(human[parameter=="well_dead",value], n.t)

m_param[1, 1:length(state_names)] <- state_i

f_human_epi <- function(m_param, n.t){
  for (i in 2:(n.t)){
    m_param[i,"well"] <- m_param[i-1,"well"] -(m_param[i-1,"r"]*m_param[i-1,"well"]) -
      (m_param[i-1,"s"]*m_param[i-1,"well"]) + (m_param[i-1,"birth"]*m_param[i-1,"well"])-
      (m_param[i-1,"mort_w"]*m_param[i-1,"well"])+(m_param[i-1,"rec_r"]*m_param[i-1,"res"])+
      (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
    
    m_param[i,"res"] <- m_param[i-1,"res"] + (m_param[i-1,"r"]*m_param[i-1,"well"]) - 
      (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) - (m_param[i-1,"rec_r"]*m_param[i-1,"res"])
    
    m_param[i,"sus"] <- m_param[i-1,"sus"] + (m_param[i-1,"s"]*m_param[i-1,"well"])
    - (m_param[i-1,"mort_s"]*m_param[i-1,"sus"]) - (m_param[i-1,"rec_s"]*m_param[i-1,"sus"])
    
    m_param[i,"dead"] <- (m_param[i-1,"mort_r"]*m_param[i-1,"res"]) + (m_param[i-1,"mort_s"]*m_param[i-1,"sus"])+
      (m_param[i-1,"mort_w"]*m_param[i-1,"well"])
    ## note that this is the incidence of death due to how we then multiply with QALY loss but 
    # if change that should also add in + m_param[i-1,"dead"] 
  }
  return(m_param)
}

m_param <- f_human_epi(m_param,n.t)

#### HC system cost ###########
m_cost <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
colnames(m_cost) <- parameter_names
rownames(m_cost) <- paste("cycle", 0:(n.t-1), sep  =  "")


c_r <- human[parameter=="r_cost",value]
c_s <- human[parameter=="s_cost",value]
  
cost_i <- c(0,c_r,c_s,0)
  
## start at cycle 1 so you do not multiply initial state vector 
m_cost[2, 1:length(state_names)] <- cost_i

for (j in 1:length(state_names)) {
  for (i in 3:(n.t)){
    m_cost[i,j] <- f_di(m_cost[i-1,j],dr)
  }  
}

#### HC system rewards ##################
m_rwd <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names))
colnames(m_rwd) <- parameter_names
rownames(m_rwd) <- paste("cycle", 0:(n.t-1), sep  =  "")


r_r <- human[parameter=="hrqol_ill",value]
r_s <- human[parameter=="hrqol_ill",value]
r_d <- human[parameter=="hrqol_death",value]

rwd_i <- c(1,r_r,r_s,r_d)

## start at cycle 1 so you do not multiply initial state vector 
m_rwd[2, 1:length(state_names)] <- rwd_i

for (j in 1:length(state_names)) {
  for (i in 3:(n.t)){
    m_rwd[i,j] <- f_di(m_rwd[i-1,j],dr)
  }  
}


###################*****ANIMAL MODEL*****###########################


state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
parameter_names_a <- c(state_names_a, transition_names_a)

state_i_a <- c(intervention[parameter=="n_animals",value], rep(0,length=length(state_names_a)-1))

m_param_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
colnames(m_param_a) <- parameter_names_a
rownames(m_param_a) <- paste("cycle", 0:(n.t-1), sep  =  "")

m_param_a[ , "r"] <- rep(animal[parameter=="well_r",value], n.t)
m_param_a[ , "s"] <- rep(animal[parameter=="well_s",value], n.t)
m_param_a[ , "mort_s"] <- rep(animal[parameter=="s_dead",value], n.t)
m_param_a[ , "mort_r"] <- rep(m_param_a[1, "mort_s"]+((m_param_a[1, "mort_s"])*(animal[parameter=="r_dead_impact",value])), n.t)
m_param_a[ , "rec_r"] <- rep(1-(m_param[1,"mort_r"]), n.t)
m_param_a[ , "rec_s"] <- rep(1-(m_param[1,"mort_s"]), n.t)
m_param_a[ , "birth"] <- rep(animal[parameter=="birth_well",value], n.t)
m_param_a[ , "mort_w"] <- rep(animal[parameter=="well_dead",value], n.t)
m_param_a[ , "w_sold"] <- rep(animal[parameter=="well_sold",value], n.t)

m_param_a[1, 1:length(state_names_a)] <- state_i_a

f_animal_epi <- function(m_param_a, n.t){
for (i in 2:(n.t)){
  m_param_a[i,"well"] <- m_param_a[i-1,"well"] -(m_param_a[i-1,"r"]*m_param_a[i-1,"well"]) -
    (m_param_a[i-1,"s"]*m_param_a[i-1,"well"]) + (m_param_a[i-1,"birth"]*m_param_a[i-1,"well"])-
    (m_param_a[i-1,"mort_w"]*m_param_a[i-1,"well"])+(m_param_a[i-1,"rec_r"]*m_param_a[i-1,"res"])+
    (m_param_a[i-1,"rec_s"]*m_param_a[i-1,"sus"])-(m_param_a[i-1,"w_sold"]*m_param_a[i-1,"well"])
  
  m_param_a[i,"res"] <- m_param_a[i-1,"res"] + (m_param_a[i-1,"r"]*m_param_a[i-1,"well"]) - 
    (m_param_a[i-1,"mort_r"]*m_param_a[i-1,"res"]) - (m_param_a[i-1,"rec_r"]*m_param_a[i-1,"res"])
  
  m_param_a[i,"sus"] <- m_param_a[i-1,"sus"] + (m_param_a[i-1,"s"]*m_param_a[i-1,"well"])
  - (m_param_a[i-1,"mort_s"]*m_param_a[i-1,"sus"]) - (m_param_a[i-1,"rec_s"]*m_param_a[i-1,"sus"])
  
  m_param_a[i,"fallen"] <- m_param_a[i-1,"fallen"]+(m_param_a[i-1,"mort_r"]*m_param_a[i-1,"res"]) + (m_param_a[i-1,"mort_s"]*m_param_a[i-1,"sus"])+
    (m_param_a[i-1,"mort_w"]*m_param_a[i-1,"well"])
  
  m_param_a[i,"sold"] <- (m_param_a[i-1,"w_sold"]*m_param_a[i-1,"well"])
  ## note that theis is the incidence of death due to how we then multiply with income etc.
  # if change that should also add in + m_param[i-1,"sold"] 
}
return(m_param_a)
}

m_param_a <- f_animal_epi(m_param_a,n.t)

#### farm cost ###########
m_cost_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
colnames(m_cost_a) <- parameter_names_a
rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")

c_s <- animal[parameter=="s_cost",value]
c_r <- c_s+(c_s*animal[parameter=="r_cost_impact",value])

cost_i_a <- c(0,c_r,c_s,0,0)

## start at cycle 1 so you do not multiply initial state vector 
m_cost_a[2, 1:length(state_names_a)] <- cost_i_a

for (j in 1:length(state_names_a)) {
  for (i in 3:(n.t)){
    m_cost_a[i,j] <- f_di(m_cost_a[i-1,j],dr)
  }  
}

#### farm rewards ##################
m_rwd_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
colnames(m_rwd_a) <- parameter_names_a
rownames(m_rwd_a) <- paste("cycle", 0:(n.t-1), sep  =  "")


r_w <- animal[parameter=="i_animal",value]
r_sold <- animal[parameter=="i_animal",value]

rwd_i_a <- c(r_w,0,0,0,r_sold)

## start at cycle 1 so you do not multiply initial state vector 
m_rwd_a[2, 1:length(state_names_a)] <- rwd_i_a

for (j in 1:length(state_names_a)) {
  for (i in 3:(n.t)){
    m_rwd_a[i,j] <- f_di(m_rwd_a[i-1,j],dr)
  }  
}


##########*****INTERVENTION****** ############################

### reduction in incidence of drug resistant infections
### humans
m_param2 <- m_param

m_param2[ , "r"] <- rep(human[parameter=="well_r",value]-(human[parameter=="well_r",value]*intervention[parameter=="u_RH",value]), 
                          n.t)

## clear state values
m_param2[ , 1:length(state_names)] <- 0
m_param2[1, 1:length(state_names)] <- state_i

m_param2 <- f_human_epi(m_param2, n.t)

## animals
m_param_a2 <- m_param_a
m_param_a2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*intervention[parameter=="u_RA",value]), 
                       n.t)
m_param_a2[ , 1:length(state_names_a)] <- 0
m_param_a2[1, 1:length(state_names_a)] <- state_i_a

m_param_a2 <- f_animal_epi(m_param_a2, n.t)

### costs and rewards are the same for healthcare system
## rewards are the same for farms
# costs change for each well add a cost of intervention

m_cost_a2 <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
colnames(m_cost_a) <- parameter_names_a
rownames(m_cost_a) <- paste("cycle", 0:(n.t-1), sep  =  "")

c_s <- animal[parameter=="s_cost",value]
c_r <- c_s+(c_s*animal[parameter=="r_cost_impact",value])
c_interv <- intervention[parameter=="int_cost_per",value]

cost_i_a2 <- c(c_interv,c_r,c_s,0,0)

## start at cycle 1 so you do not multiply initial state vector 
m_cost_a2[2, 1:length(state_names_a)] <- cost_i_a2

for (j in 1:length(state_names_a)) {
  for (i in 3:(n.t)){
    m_cost_a2[i,j] <- f_di(m_cost_a2[i-1,j],dr)
  }  
}



############ RESULTS #######################
results_base_h <- f_expvalue(m_param,m_cost,m_rwd)
results_base_a <- f_expvalue(m_param_a,m_cost_a,m_rwd_a)
results_interv_h <- f_expvalue(m_param2,m_cost,m_rwd)
results_interv_a <- f_expvalue(m_param_a2,m_cost_a2,m_rwd_a)

total_results_HC<- matrix(rep(0), nrow=2, ncol=2)
colnames(total_results_HC) <- c("Costs (£)", "QALYs")
rownames(total_results_HC) <- c("Base Case", "Intervention")

total_results_HC[1,] <- results_base_h[1,]
total_results_HC[2,] <- results_interv_h[1,]

#### HC 
incr_cost <- (results_interv_h[1,1] - results_base_h[1,1])
incr_benefit <-  (results_interv_h[1,2]-results_base_h[1,2])
icer <- incr_cost/incr_benefit
NMB_H <- (incr_benefit*wtp)-(incr_cost)  # per person in the population

## Farm level
incr_cost_a <- (results_interv_a[1,1] - results_base_a[1,1])
incr_benefit_a <-  (results_interv_a[1,2]-results_base_a[1,2])

total_results_Ag<- matrix(rep(0), nrow=2, ncol=2)
colnames(total_results_Ag) <- c("Costs (£)", "Benefits (£)")
rownames(total_results_Ag) <- c("Base Case", "Intervention")

total_results_Ag[1,] <- results_base_a[1,]
total_results_Ag[2,] <- results_interv_a[1,]

CBR <- incr_benefit_a/incr_cost_a
NMB_A <- incr_benefit_a-incr_cost_a # per farm in the population

NMB_A_all <- NMB_A*intervention[parameter=="n_farms",value]

outputs <- data.table(incr_cost=incr_cost, incr_benefit=incr_benefit,
                      incr_cost_a=incr_cost_a, incr_benefit_a=incr_benefit_a,
                      icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A)

return(outputs)

}

######### sensitivity analysis ######################

## number of parameters
nr <- nrow(inputs)
## number of psa runs
npsa <- 1000

######## running different sampling distributions based on the distribution types
####**** beta moments****####

mbeta <- inputs[distribution=="beta"]
mbeta[ , alpha := ((value^2)*(1-value)/(low^2))-(value)]
mbeta[ , beta := alpha*((1-value)/value)]

sampling.beta <- function(x, n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(x[n.t, "alpha"])
  be <- as.numeric(x[n.t, "beta"])
  vec <- rbeta(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid=runid, scenario, parameter,value=vec)
  return(dt)
}

l.mbeta <- list()
# for each subgroup + resistance type
for (i in 1:nrow(mbeta)) {
  n.t <- i
  l.mbeta[[i]] <- sampling.beta(mbeta,n.t)
}


#####**** beta *****####
# with count - not currently used

#####**** lognormal ****######
# haven't done a gamma one in this current iteration 
ln.d <- inputs[distribution=="lognormal"]
ln.d[ , se := as.numeric(as.character(low))]
ln.d[ ,location := log(value^2 / sqrt(se^2 + value^2))]
ln.d[ , shape := sqrt(log(1 + (se^2 / value^2)))]


sampling.ln <- function(x, n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(x[n.t, "location"])
  be <- as.numeric(x[n.t, "shape"])
  vec <- rlnorm(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid=runid, scenario, parameter,value=vec)
  return(dt)
}

l.lognorm <- list()


for (i in 1:nrow(ln.d)) {
  n.t <- i
  l.lognorm[[i]] <- sampling.ln(ln.d,n.t)
}



###**** normal****#####
norm.d <- inputs[distribution=="normal"]
norm.d[ , SD := low]   

sampling.n <- function(x,n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  al <- as.numeric(norm.d[n.t, "value"])
  be <- as.numeric(norm.d[n.t, "SD"])
  vec <- rnorm(1000, al, be)
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.norm <- list()

for (i in 1:nrow(norm.d)) {
  n.t <- i
  l.norm[[i]] <- sampling.n(norm.d,n.t)
}

### uniform distribution
###**** normal****#####
uni.d <- inputs[distribution=="uniform"]

sampling.u <- function(x,n.t) {
  set.seed(123)
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  mn <- as.numeric(uni.d[n.t, "low"])
  mx <- as.numeric(uni.d[n.t, "high"])
  vec <- runif(1000, mn, mx)
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.uni <- list()

for (i in 1:nrow(uni.d)) {
  n.t <- i
  l.uni[[i]] <- sampling.u(uni.d,n.t)
}

### just repeat for those not in PSA analysis currently the same values 1000 times

other.d <- inputs[is.na(distribution)]


sampling.other <- function(x, n.t) {
  scenario<- x[n.t, "scenario"]
  parameter <- x[n.t, "parameter"]
  vec <- x[n.t, "value"]
  runid <- c(1:1000)
  dt <- data.table(runid, scenario, parameter,value=vec)
  return(dt)
}

l.other <- list()


for (i in 1:nrow(other.d)) {
  n.t <- i
  l.other[[i]] <- sampling.other(other.d,n.t)
}


###### combine to form 1000 data.tables to pull the parameters from 

### restack the individual lists before joining
mbeta <- rbindlist(l.mbeta)
mnorm <- rbindlist(l.norm)
lognorm <- rbindlist(l.lognorm)
uniform <- rbindlist(l.uni)
other <- rbindlist(l.other)

# Change colname of last data frame
colnames(other)[colnames(other) == "value.value"] <- "value"


## create data.table of all the current lists
psa.data <- list(mbeta, mnorm, lognorm, uniform,other)
bigdata <- rbindlist(psa.data)

## create array of each run to pull from for the model
# probably a faster way to do this 
# number of columns
nc <- ncol(psa.data[[1]])
cols <- colnames(bigdata)

#  Preallocate 
# psa.values <- array(0,
#                     dim = c(nr, nc, npsa),
#                     dimnames = list(1:nr, cols,
#                                     paste("run", 1:npsa, sep = "")))
# # fill
# for (i in 1:npsa) {
#   psa.values[, ,i] <- as.matrix(bigdata[runid==i])
# }

# ## save
# save(psa.values, file="data/psa.RData")


psa_output <- data.table(incr_cost=rep(0,1000),
                          incr_benefit=rep(0,1000),
                         incr_cost_a=rep(0,1000),
                         incr_benefit_a=rep(0,1000),
                         icer=rep(0,1000),
                         CBR=rep(0,1000),
                         NMB_H=rep(0,1000),
                         NMB_A=rep(0,1000))

for (i in 1:1000){
  
  inputs <- as.data.table(bigdata[runid==i])
  
  psa_output[i, ] <- model(inputs)
  
}

# save list
# save(psa_output, file="data/PSA-output.RData")

library(ggplot2)

ggplot(psa_output, aes(x=incr_benefit, y=incr_cost)) +
  geom_point() +labs(x = "Incremental QALYs", y="Incremental Cost (£)")+
  labs(title = "Probablistic Sensitivity Analysis - Healthcare")

ggplot(psa_output, aes(x=incr_benefit_a, y=incr_cost_a)) +
  geom_point() +labs(x = "Incremental Benefit (£)", y="Incremental Cost (£)")+
  labs(title = "Probablistic Sensitivity Analysis - Agriculture")



#################### univariate sensitivity analysis (two-way)


inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)

inputs.urh.low <- inputs
inputs.urh.low[parameter=="u_RH", value := 0.05]
model(inputs.urh.low)

inputs.urh.high <- inputs
inputs.urh.high[parameter=="u_RH", value := 0.13]
model(inputs.urh.high)

inputs.rcost.low <- inputs
inputs.rcost.low[parameter=="r_cost" & scenario=="human_0", value := 1400]
model(inputs.rcost.low)

inputs.rcost.high <- inputs
inputs.rcost.high[parameter=="r_cost" & scenario=="human_0", value := 1700]
model(inputs.rcost.high)

inputs.rmort.low <- inputs
inputs.rmort.low[parameter=="r_dead" & scenario=="human_0", value := 0.15]
model(inputs.rmort.low)

inputs.rmorthigh.high <- inputs
inputs.rmorthigh.high[parameter=="r_dead" & scenario=="human_0", value := 0.20]
model(inputs.rmorthigh.high)

#### hand input the results for univariate for now for plot

library(ggplot2)

tornado1 <- data.frame(variable = c("Reduction in Resistant Infection Risk in Humans from Intervention",
                            "Cost of Resistant Infection",
                            "Mortality from Resistant Infections"),
                  min = c(-512.8244, -448.1568, -482.11),
                  max = c(-512.8295, -544.2149, -591.29))

save(tornado1, file="data/tornado_HC.RData")

basecase1 <- -512.8295
  
ggplot(tornado1, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = basecase1, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))


### same for agriculture parameters

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.urh.low <- inputs
inputs.urh.low[parameter=="u_RA", value := 0.17]
model(inputs.urh.low)

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.urh.high <- inputs
inputs.urh.high[parameter=="u_RA", value := 0.32]
model(inputs.urh.high)

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.rcost.low <- inputs
inputs.rcost.low[parameter=="r_cost_impact" & scenario=="animal_0", value := 0.05]
model(inputs.rcost.low)

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.rcost.high <- inputs
inputs.rcost.high[parameter=="r_cost_impact" & scenario=="animal_0", value := 0.5]
model(inputs.rcost.high)

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.rmort.low <- inputs
inputs.rmort.low[parameter=="r_dead_impact" & scenario=="animal_0", value := 0.05]
model(inputs.rmort.low)

inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs.rmorthigh.high <- inputs
inputs.rmorthigh.high[parameter=="r_dead_impact" & scenario=="animal_0", value := 0.50]
model(inputs.rmorthigh.high)

tornado2 <- data.frame(variable = c("Reduction in Resistant Infection Risk in Livestock from Intervention",
                                    "Cost of Resistant Infection Impact",
                                    "Mortality from Resistant Infections Impact"),
                       min = c(1.65, 2.34, 2.35),
                       max = c(3.382, 2.53,2.49))

save(tornado2, file="data/tornado_Agr.RData")

basecase2 <- 2.418353

ggplot(tornado2, aes(variable, ymin = min, ymax = max)) +
  geom_linerange(size = 10) +
  coord_flip() +
  xlab("") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = basecase2, linetype = "dashed") +
  theme_bw() +
  theme(axis.text = element_text(size = 15))