# ----- Run life table -----

### Function to create a life table for a age and sex cohort

RunLifeTable <- function(in_data, in_sex, in_mid_age, death_rates=NA, scenario = 0) {
  # in_data= mslt_general
  # in_sex='male'
  # in_mid_age=17
  # death_rates=death_projections
  
  # Create a life table starting data frame from input data. 
  lf_df <- in_data %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>%
    dplyr::select('sex', 'age', 'pyld_rate', 'mx') %>%
    mutate(mx=mx*(1-scenario))
  
  # If mortality trends available
  if(is.data.frame(death_rates)) {
    # filter to only this cohort's death rates
    cohort_death_rates <- death_rates %>%
      dplyr::filter(age_cohort == in_mid_age & sex == in_sex) %>%
      dplyr::select(age,sex,rate)
    # join to lf_df and replace mx with the new mortality data
    lf_df <- lf_df %>%
      dplyr::inner_join(cohort_death_rates, by=c('age','sex')) %>%
      dplyr::select(sex, age, pyld_rate, mx=rate) %>%
        mutate(mx=mx*(1-scenario))
    }
  
  
  
  # Create list life table variables. First as vector and then added to the data frame at the end.
  ## We model up to 100, that is the reason for the age limit in the function
  
  # probability of dying
  
  qx <-  ifelse(lf_df$age < 100, 1 - exp(-1 * lf_df$mx), 1)
  
  # number of survivors year 1 simulation
  
  num_row <- nrow(lf_df)
  lx <- rep(0,num_row)
  lx[1] <- as.numeric(in_data$population[in_data$age == in_mid_age & in_data$sex == in_sex]) 
  
  # number died in year 1 simulation
  
  dx <- rep(0,num_row)
  dx[1] <- lx[1] * qx[1]
  
  # number of survivors and who die from year 2 onwards. 
  
  for (i in 2:num_row){
    lx[i] <- lx[i - 1] - dx[i - 1]
    dx[i] <- lx[i] * qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  
  Lx <- rep(0,num_row)
  
  # for years up to 99
  
  for (i in 1:(num_row-1))
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  
  # for year 100, cohort dies at 100 if anyone left
  
  Lx[num_row] <- lx[num_row] / lf_df$mx[num_row]
  
  
  # create life expectancy variable
  ex <- rep(0,num_row)
  for (i in 1:num_row){
    ex[i] <- sum(Lx[i:num_row]) / lx[i]
  }
  
  # create health adjusted life years variable 
  
  Lwx <- Lx * (1 - lf_df$pyld_rate)
  
  # create health adjusted life expectancy variable
  ewx <- rep(0,num_row)
  for (i in 1:num_row){
    ewx[i] <- sum(Lwx[i:num_row]) / lx[i]
  }
  
  lf_df$qx <- qx
  lf_df$lx <- lx
  lf_df$dx <- dx
  lf_df$Lx <- Lx
  lf_df$ex <- ex
  lf_df$Lwx <- Lwx
  lf_df$ewx <- ewx
  lf_df
}

# ----- Run disease process -----
## Function to generate disease process for an age and sex cohort
## Based on formulas in Barendregt JJ, Oortmarssen GJ, van, Vos T, Murray CJL. A generic model for the assessment of
## disease epidemiology: the computational basis of DisMod II. Population Health Metrics.2003;1(1):4.

RunDisease <- function(in_data, in_sex, in_mid_age, in_disease, incidence_trends=NA, 
                       mortality_trends=NA, scenario_inc=0, scenario_cf=0) {
  # in_data=mslt_general
  # in_sex='male'
  # in_mid_age=17
  # in_disease='dmt2'
  
  # create disease variable for the disease life table function 
  dw_disease <- paste("dw_adj", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## add generic variable names to the source data frame (in_data)
  in_data$dw_disease <- in_data[[dw_disease]]
  in_data$incidence_disease <- in_data[[incidence_disease]]
  in_data$case_fatality_disease <- in_data[[case_fatality_disease]]
  
  # Select columns for lifetable calculations
  

  dlt_df <- in_data %>%
    dplyr::filter(age >= in_mid_age & sex == in_sex) %>% 
    dplyr::select('sex', 'age', dw_disease, incidence_disease, case_fatality_disease)
  

  dlt_df$disease <- in_disease
  
  ## Add trends
  if(is.data.frame(mortality_trends)) {
    # filter to only this cohort's incidence trends
    cohort_mortality_trends <- mortality_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',mortality_trend=in_disease) %>%
      dplyr::mutate(row_num=row_number())
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_mortality_trends, by=c('row_num')) %>%
      dplyr::mutate(case_fatality_disease=case_fatality_disease*mortality_trend* (1-scenario_cf)) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  

  if(is.data.frame(incidence_trends)) {
    # filter to only this cohort's incidence trends
    cohort_incidence_trends <- incidence_trends %>%
      dplyr::filter(sex == in_sex) %>%
      dplyr::select('year',incidence_trend=in_disease) %>%
      dplyr::mutate(row_num=row_number())
    dlt_df <- dlt_df %>%
      dplyr::mutate(row_num=row_number()) %>%
      dplyr::inner_join(cohort_incidence_trends, by=c('row_num')) %>%
      dplyr::mutate(incidence_disease=incidence_disease*incidence_trend* (1-scenario_inc)) %>%
      dplyr::select('sex', 'age', 'dw_disease', 'incidence_disease', 'case_fatality_disease', 'disease')
  }
  
  # create list of life table variables. Created as vectors and then added to dataframe. 
  # See see methods in: 1) Concept and original calculations: Barendregt, J. J., et al. (1998). "Coping with multiple morbidity in a life table." Math Popul Stud 7(1): 29-49. 
  # and 2) Latest version, variables below calculated from it: Barendregt, J. J., et al. (2003). "A generic model for the assessment of disease epidemiology: the computational basis of DisMod II." Popul Health Metr 1(1): 4-4.
  
  
  ### lx, qx, wx and vx are intermediate variables, 
  
  lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  wx <- exp(-1*(lx+qx)/2)
  vx <- exp(-1*(lx-qx)/2)
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ## persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ## Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  ### First create empty variables
  
  number_of_ages <- nrow(dlt_df)
  Sx <- Cx <- Dx <- Tx  <- Ax <- PYx <- px <- mx <- rep(0,number_of_ages)
  cfds <- dlt_df$case_fatality_disease
  ages <- dlt_df$age
  
  #### Starts with 1000 healthy (Sx) and total (Ax) people. 
  
  Sx[1] <- Ax[1] <- 1000
  
  ##### start with variables without calculation exceptions
  
  ##### variables without exceptions (else includes exception for year one of the simulation)  
  for (i in 2:(number_of_ages-1)){ 
    if(qx[i-1] > 0){
      
      ### The following five variables are created to simplify Sx, Cx and Dx calculations, and do not form part of the disease life tables.
      vxmwx <- vx[i-1] - wx[i-1]
      SxpCx <- Sx[i-1]+Cx[i-1]
      dqx <- 2 * qx[i-1]
      qxmlx <- qx[i-1] - lx[i-1]
      qxplx <- qx[i-1] + lx[i-1]
      
      ### Healthy (Sx), Diseases (Cx) and Death from the Disease (Dx)
      
      Sx[i] <- Sx[i-1] * (2*vxmwx * cfds[i-1]  + (vx[i-1] * qxmlx + wx[i-1] * qxplx)) / dqx
      Cx[i] <- -1*(vxmwx*(2*(cfds[i-1]  * SxpCx - lx[i-1] * Sx[i-1]) - Cx[i-1] * lx[i-1]) - Cx[i-1] * qx[i-1] * (vx[i-1]+wx[i-1])) / dqx
      Dx[i] <- (vxmwx * (2 * cfds[i-1] * Cx[i-1] - lx[i-1]*SxpCx)- qx[i-1] * SxpCx*(vx[i-1]+wx[i-1]) + dqx * (SxpCx+Dx[i-1]) ) / dqx
    }else{
      Sx[i] <- Sx[i - 1] 
      Cx[i] <- Cx[i - 1]
      Dx[i] <- Dx[i - 1]
    }
  }
  
  
  Tx   <- Sx + Cx + Dx 
  Ax <- Sx + Cx
  
  first_indices <- 1:(number_of_ages-1)
  last_indices <- 2:number_of_ages
  PYx <- (Ax[first_indices] + Ax[last_indices])/2
  mx[first_indices] <- (Dx[last_indices] - Dx[first_indices])/PYx[first_indices]
  mx[mx<0] <- 0
  px[first_indices] <- (Cx[last_indices] + Cx[first_indices])/2/PYx[first_indices]
  
  dlt_df$Sx <- Sx
  dlt_df$Cx <- Cx
  dlt_df$Dx <- Dx
  dlt_df$Tx <- Tx
  dlt_df$mx <- mx
  dlt_df$px <- px

  dlt_df
}


# ----- Run Model -----
CalculationModel <- function(in_data, 
                             in_sex, 
                             in_mid_age, 
                             in_death_trends=NA,
                             in_disease, 
                             incidence_trends=NA, 
                             mortality_trends=NA, 
                             scenario_inc=0, 
                             scenario_cf=0
){
  
# 
 
  # in_data=mslt_general
  # in_sex="female"
  # in_mid_age=17
  # in_disease=c("ishd", "tbalc")
  # scenario_inc=0.05
  # scenario_cf=0
  # in_death_trends=death_projections


  ### Steps 
  # 1) Run general life table baseline
  # 2) Run disease life tables baseline
  # 3) Run scenario life tables (where incidence of diseases changes)
  # 4) Collect changes in mx and pylds from differences between baseline and scenario disease life tables
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
  # 1) Run general life table baseline

      general_life_table_bl <- RunLifeTable(
        in_data    = in_data,
        in_sex      = in_sex,
        in_mid_age  = in_mid_age,
        death_rates = in_death_trends
       )
      
  # View(general_life_table_bl)
          
  
  cat(paste0("have run baseline life table"))
  
  # 2) Run disease life tables baseline
  
  # Change order in disease short_names to start with diabetes. 
  # This is important when calculating the scenario disease life tables as diabetes is calculated first to then 
  # impact on cardiovascular disease calculations. 
  # In the disease trends "Year" means simulation year, not age. 


  disease_cohorts <- tibble(read_csv("./data/disease_names.csv")) %>%
    # Exclude disease not included in input
    dplyr::filter(sname %in% in_disease) %>%
    dplyr::select(sname,acronym)
  
  disease_life_table_list_bl <- list()
  
  for (i in 1:nrow(disease_cohorts)){
    disease_life_table_list_bl[[i]] <- RunDisease(
      in_data         = in_data,
      in_mid_age       = in_mid_age,
      in_sex           = in_sex,
      in_disease       = disease_cohorts$sname[i],
      incidence_trends = incidence_trends,
      mortality_trends = mortality_trends, 
      scenario_inc = 0, 
      scenario_cf = 0
    )
    names(disease_life_table_list_bl)[i] <- disease_cohorts$sname[i]
    
  }
  
  # View(disease_life_table_list_bl[[2]])
  
  cat(paste0("have run baseline disease life table"))
  
  # 3) Run scenario disease life tables (where incidence is modified by scenario_inc or case fatality by scenario_cf)
  
 
  disease_life_table_list_sc <- list()
  
  for (i in 1:nrow(disease_cohorts)){
    
    disease_life_table_list_sc[[i]] <- RunDisease(
      in_data         = in_data,
      in_mid_age       = in_mid_age,
      in_sex           = in_sex,
      in_disease       = disease_cohorts$sname[i],
      incidence_trends = incidence_trends,
      mortality_trends = mortality_trends, 
      scenario_inc = scenario_inc, 
      scenario_cf = scenario_cf
    )  %>%
      mutate(
      diff_inc_disease = incidence_disease - disease_life_table_list_bl[[i]]$incidence_disease,
      diff_prev_disease = px - disease_life_table_list_bl[[i]]$px,
      diff_mort_disease = mx - disease_life_table_list_bl[[i]]$mx,
      diff_pylds_disease= (px - disease_life_table_list_bl[[i]]$px) *
               dw_disease)

    names(disease_life_table_list_sc)[i] <- disease_cohorts$sname[i]

  }
  cat(paste0("have run scenario disease life table"))
  

  
  # View(disease_life_table_list_sc[[1]])
  

  
  # convert the list of dataframes to single dataframes
  disease_life_table_bl <- bind_rows(disease_life_table_list_bl)
  disease_life_table_sc <- bind_rows(disease_life_table_list_sc) 
  
  
  # 4) Collect changes in mx and pylds from differences between baseline and sceanrio disease life tables
  
  ### Sum mortality rate and pylds change scenarios
  mx_pylds_sc_total_disease_df <- disease_life_table_sc %>%
    dplyr::group_by(age) %>%
    dplyr::summarise(mortality_sum = sum(diff_mort_disease,na.rm=T),
                     pylds_sum=sum(diff_pylds_disease,na.rm=T)) %>%
    ungroup()
  
  
  
  # 5) Recalculate general life table with mx and totalpylds modified by 4
  
  ## Calculate general life tables with modified mortality and pylds total
  ## Original mortality rate is modified by the mx_sc_total (total change in mortality from diseases)
  ## Original pyld rate is modified by the change in each disease pylds
  
 ### Modify rates in static MSLT  (pylds are always static, mx can include future trends)
    
    ### removing the impact from the accumulated mortality and pyld from the diseases
  
    td1 <- in_data %>%
      dplyr::select(sex, age, pyld_rate, mx, population) %>%
      left_join(mx_pylds_sc_total_disease_df,by="age") %>%
      dplyr::mutate(mortality_sum = replace_na(mortality_sum, 0)) %>%
      mutate(mx=mx+mortality_sum,
             pyld_rate=pyld_rate+pylds_sum) %>%
      dplyr::mutate(pyld_rate = replace_na(pyld_rate, 0)) %>%
      dplyr::select(-pylds_sum)
  
    
    
    td2 <- in_death_trends %>% # variable to match change in mortality rates df
      filter(age_cohort==in_mid_age, sex == in_sex) %>%
      left_join(mx_pylds_sc_total_disease_df) %>%
        mutate(rate=rate+replace_na(mortality_sum,0))%>%
        dplyr::select(-mortality_sum,-pylds_sum)
    
   
   ### ADD an if condition to run with td2 if death_trends available
    
      general_life_table_sc <- RunLifeTable(
        in_data    = td1,
        in_sex      = in_sex,
        in_mid_age  = in_mid_age,
        death_rates = td2
      )


  
  # 6) Generate outputs dataframe
  
  disease_sc <- inner_join(disease_life_table_sc %>%
                             dplyr::select(sex,age,disease,incidence_disease,mx,px),
                           general_life_table_sc %>%
                             dplyr::select(sex,age,Lx,ex,Lwx,ewx),
                           by=c("age","sex")) %>%
    mutate(intervention="sc")
  
  disease_bl <- inner_join(disease_life_table_bl %>%
                             dplyr::select(sex,age,disease,incidence_disease,mx,px),
                           general_life_table_bl %>%
                             dplyr::select(sex,age,Lx,ex,Lwx,ewx),
                           by=c("age","sex")) %>%
    mutate(intervention="bl")
  
  disease_combined <- bind_rows(disease_sc,disease_bl) %>%
    pivot_wider(names_from  = intervention,
                values_from = c(incidence_disease,mx,px,Lx,ex,Lwx,ewx)) %>%
    mutate(inc_num_bl   = incidence_disease_bl*(1-px_bl)*Lx_bl,
           inc_num_sc   = incidence_disease_sc*(1-px_sc)*Lx_sc,
           inc_num_diff = inc_num_sc-inc_num_bl,
           mx_num_bl    = mx_bl*Lx_bl,
           mx_num_sc    = mx_sc*Lx_sc,
           mx_num_diff  = mx_num_sc-mx_num_bl) %>%
    pivot_wider(names_from  = disease,
                values_from = incidence_disease_sc:mx_num_diff)
  
  general_lf <- bind_rows(
    general_life_table_sc %>%
      dplyr::select(sex,age,Lx,ex,Lwx,ewx) %>%
      mutate(intervention="sc"),
    general_life_table_bl %>%
      dplyr::select(sex,age,Lx,ex,Lwx,ewx) %>%
      mutate(intervention="bl")) %>%
    pivot_wider(names_from  = intervention,
                values_from = c(Lx,ex,Lwx,ewx)) %>%
    mutate(Lx_diff  = Lx_sc-Lx_bl,
           Lwx_diff = Lwx_sc-Lwx_bl,
           ex_diff  = ex_sc-ex_bl,
           ewx_diff = ewx_sc-ewx_bl)
  
  
  ## Dataframe with all outputs by age and sex cohort over the simulation years (years of the cohort)
  output_df <- inner_join(disease_combined,
                          general_lf,
                          by=c("age","sex"))
  
output_df
}


