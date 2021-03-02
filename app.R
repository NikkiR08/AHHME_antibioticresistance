
################## requried libraries
require(shiny)
require(shinythemes)
require(xlsx)
require(rsconnect)
require(dplyr)
require(tidyr)
require(data.table)

################# required data
inputs <- read.csv("data/input.csv")
inputs <- as.data.table(inputs)
inputs[ , value := as.numeric(as.character(value))]
human <- inputs[scenario=="human_0" | scenario=="human_1"]
animal <- inputs[scenario=="animal_0" | scenario=="animal_1"]
intervention <- inputs[scenario=="intervention"]

# The user interface (ui) object controls the layout and appearance of your app. 
# The server function contains the instructions that your computer needs to build your app. 
# Finally the shinyApp function creates Shiny app objects from an explicit UI/server pair.

###################### BACKGROUND CODE ##############################

#######################################################################
############## USER INTERFACE ########################################
ui <- fluidPage(  
    theme = shinythemes::shinytheme("flatly"),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    
    titlePanel("Agriculture-Human-Health-MicroEconomic (AHHME) Tool for Antimicrobial Usage Scenarios: A Pilot Study"),
    
    sidebarPanel(h3("Methodology Inputs"),
                 
                 ## Willingness to Pay
                 numericInput("wtp", em("Willingness-to-Pay per QALY Gained (GBP)"), 20000, min = 0, 
                              max = 1000000),
                 
                 ## Discount Rate
                 numericInput("dcr", em("Discount rate (0%-10%)"), 3.5, min = 0, 
                              max = 100),
                 
                 ## Time Horizon
                 numericInput("nt", em("Time Horizon (Model Cycle Length, 3-100 cycles)"), 10, min = 1, 
                              max = 100),
                 
    ),
    
    mainPanel(
        
        # Output: Tabset w/ plot, summary, and table
        tabsetPanel(type = "tabs", 
                    tabPanel("Inputs: Manual Variation of Selected Paramaters", 
                             h4("General Values"),
                             numericInput("r_cost", em("Cost of a Drug Resistant Infection to Healthcare System"), 1600, min = 0, 
                                          max = 1000000),
                             numericInput("s_cost", em("Cost of a Drug Susceptible Infection to Healthcare System"), 1200, min = 0, 
                                          max = 1000000),
                             numericInput("c_animal", em("Cost of Keeping an Animal to a Farm per Production cycle"), 100, min = 0, 
                                          max = 1000000),
                             numericInput("i_animal", em("Income per Animal to a Farm per Production cycle"), 200 , min = 0, 
                                          max = 1000000),
                             # Horizontal line ----
                             tags$hr(),
       
                             # Input: Intervention 
                             h4("Intervention Values"),
                             numericInput("int_cost_per", em("Direct Cost per Animal from Intervention Implementation"), 10, min = 0, 
                                          max = 1000),
                            
                             numericInput("u_RH", em("Risk Reduction in Resistant Infections in Humans"), 0.0934, min = 0, 
                                          max = 1),

                             numericInput("u_RA", em("Risk Reduction in Resistant Infections in Animals"), 0.24, min = 0, 
                                          max = 1),
                             
                             numericInput("n_pop", em("Population size for Healthcare Model"), 15000000, min = 0, 
                                          max = 10000000000000),
     
                             numericInput("n_animals", em("Population for Agriculture Model - Average Herd Size"), 250, min = 0, 
                                          max = 10000000000000),
                             
                             numericInput("n_farms", em("Population for Agriculture Model - Number of Farms"), 13000, min = 0, 
                                          max = 10000000000000)),
                    
                    tabPanel("Outputs: Base Case",
                             verbatimTextOutput("CEAresults"),
                             
                             textOutput("icer"),
                             
                             textOutput("CEAanswer"),
                             
                             verbatimTextOutput("CBAresults"),
                             
                             textOutput("CBA"),
                             
                             textOutput("NMB_H"),
                             
                             textOutput("NMB_A"),
                             
                             textOutput("NMB_A_all")),
                    
                    br(),
                    h5("Abbreviations: GBP - Great British Pound, QALY - Quality Adjusted Life Years"),
                    h5("The base year is 2018, with costs and rewards in 2018 Great British Pounds and Quality Adjusted Life Year values, at the national level"),
                    h5("Background data are currently based on dummy parameters (mainly retrieved from English data), these can be altered via the inputs.csv and the model code behind the app. Details of where to find these are given below."),
                    br(),
                    strong("This version of the AHHME App was to pilot the skeleton code for (a) a basic microeconomic evaluation model that incorporates impacts of antibiotic usage scenarios and (b) the app, to test functionality and feasibility of such a model in basic form. The results should NOT be taken as robust estimates of impact"),
                    br(),
                    code("App & R code by N.R Naylor. For descriptions of  model code and manual see: https://github.com/NikkiR08/AHHME_antibioticresistance"),
                    code("App last updated March 2021, Model last updated July 2020"),
                    br(),
                    strong("This research was funded by, and is a contribution to, the CGIAR Research Program on Agriculture for Nutrition and Health (A4NH). The opinions expressed here belong to the authors, and do not necessarily reflect those of A4NH or CGIAR. The funder was not involved in the study design, execution or write up processes."))           
    ))


######################################################
############# SERVER ###############################################

server <- function(input,output){
    
    
    # Reactive dependencies - if these change then MODEL will run again and update values
    xxchange <- reactive({
        paste(input$wtp, input$dcr, input$nt, input$int_cost_per,
              input$u_RH, input$u_RA, 
              input$n_pop, input$n_animals, input$n_farms,
              input$r_cost, input$s_cost, input$c_animal, input$i_animal) })
    
    
    model <- eventReactive(xxchange(), {
         
        wtp <- input$wtp
        dcr <- input$dcr/100 ##convert from 10% input to 0.1
        n.t <- input$nt
        
        u_RH <- input$u_RH 
        u_RA <- input$u_RA
        c_interv <- input$int_cost_per
        
        n_pop <- input$n_pop
        n_animals <- input$n_animals
        n_farms <- input$n_farms
        
        r_cost <- input$r_cost
        s_cost <- input$s_cost
        c_animal <- input$c_animal
        i_animal <- input$i_animal
        
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
        
        
        state_i <- c(n_pop, rep(0,length=length(state_names)-1))
        
        
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
        
    
        cost_i <- c(0,r_cost,s_cost,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost[2, 1:length(state_names)] <- cost_i
        
        for (j in 1:length(state_names)) {
            for (i in 3:(n.t)){
                m_cost[i,j] <- f_di(m_cost[i-1,j],dcr)
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
                m_rwd[i,j] <- f_di(m_rwd[i-1,j],dcr)
            }  
        }
        
        
        ###################*****ANIMAL MODEL*****###########################
        
        
        state_names_a <- c("well", "res","sus","fallen","sold") ## the compartments
        transition_names_a  <- c("birth","r","s","mort_r", "mort_s","mort_w", "rec_r","rec_s","w_sold")  ## the rates
        parameter_names_a <- c(state_names_a, transition_names_a)
    
        
        state_i_a <- c(n_animals, rep(0,length=length(state_names_a)-1))
        
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
                ## note that this is the incidence of death due to how we then multiply with income etc.
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
        c_fallen <- animal[parameter=="c_fallen", value]
        cost_i_a <- c(c_animal,c_r,c_s,c_fallen,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost_a[2, 1:length(state_names_a)] <- cost_i_a
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_cost_a[i,j] <- f_di(m_cost_a[i-1,j],dcr)
            }  
        }
        
        #### farm rewards ##################
        m_rwd_a <- matrix(rep(0), nrow=n.t, ncol =length(parameter_names_a))
        colnames(m_rwd_a) <- parameter_names_a
        rownames(m_rwd_a) <- paste("cycle", 0:(n.t-1), sep  =  "")
        
        rwd_i_a <- c(i_animal,0,0,0, i_animal)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_rwd_a[2, 1:length(state_names_a)] <- rwd_i_a
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_rwd_a[i,j] <- f_di(m_rwd_a[i-1,j],dcr)
            }  
        }
        
        
        ##########*****INTERVENTION****** ############################
        
        ### reduction in incidence of drug resistant infections
        ### humans
        m_param2 <- m_param
        
        
        m_param2[ , "r"] <- rep(human[parameter=="well_r",value]-(human[parameter=="well_r",value]*u_RH), 
                                n.t)
        
        ## clear state values
        m_param2[ , 1:length(state_names)] <- 0
        m_param2[1, 1:length(state_names)] <- state_i
        
        m_param2 <- f_human_epi(m_param2, n.t)
        
        ## animals
        
        m_param_a2 <- m_param_a
        m_param_a2[ , "r"] <- rep(animal[parameter=="well_r",value]-(animal[parameter=="well_r",value]*u_RA), 
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
        c_intervW <- c_interv + c_animal
        cost_i_a2 <- c(c_intervW,c_r,c_s,c_fallen,0)
        
        ## start at cycle 1 so you do not multiply initial state vector 
        m_cost_a2[2, 1:length(state_names_a)] <- cost_i_a2
        
        for (j in 1:length(state_names_a)) {
            for (i in 3:(n.t)){
                m_cost_a2[i,j] <- f_di(m_cost_a2[i-1,j],dcr)
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
    
        
        NMB_A_all <- NMB_A*n_farms
        
        
        
        list(total_results_HC=total_results_HC, total_results_Ag= total_results_Ag,
            icer=icer, CBR = CBR, NMB_H=NMB_H, NMB_A=NMB_A, NMB_A_all=NMB_A_all)
    })
    
    output$CEAresults <- renderPrint({model()$total_results_HC})
    
    output$icer <- renderText({paste0("Cost per QALY gained from Healthcare Sector Perspective (GBP) = ", round(model()$icer,2))})
    
    output$CEAanswer <- renderText({
        if (model()$icer<input$wtp) {paste0("The Intervention is Cost-Effective from the Healthcare System Perspective")
    }else {paste0("The Intervention is not Cost-Effective from the Healthcare System Perspective")}
        })
    
    output$CBAresults <- renderPrint({model()$total_results_Ag})
    
    output$CBA <- renderText({paste0("Cost:Benefit Ratio from the Farm-Level Perspective (GBP) = ", "1:",round(model()$CBR,2))})
    
    output$NMB_H <- renderText({paste0("Net Monetary Benefit to Healthcare Sector (GBP) = ",round(model()$NMB_H,0))})
    
    output$NMB_A <- renderText({paste0("Net Monetary Benefit to Individual Farms (GBP) = ", round(model()$NMB_A,0))})
    
    output$NMB_A_all <- renderText({paste0("Net Monetary Benefit across all Farms (GBP) = ", round(model()$NMB_A_all,0))})
    
}

##################################################################
############ SHINYAPP ###########################################
shinyApp(ui = ui, server = server)