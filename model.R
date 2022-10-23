rm(list = ls())

## Teaching code for proportional multi-state life table modelling
## Examples: 
## 1) Life table modelling for one cohort for base case (business as usual)
## and scenarios where mortality for all causes changes for every year.
## 1.1) Sensitivity modelling: inclusion of mortality trends. 
## 2) Inclusion of two diseases, in addition to the life table
## for base case and: 1) reduction of incidence of ischemic heart disease every
## year; 2) reduction of incidence of lung cancer every year; 3) reduction
## of ischemic heart disease and diabetes every years. 


# Libraries

library(ggplot2)
# library(dbplyr)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)

# Functions

source("./functions.R")

# ----- Create directories to save results ----

finalLocation     <- "outputs/" 
# Create directories, in case not created yet
dir.create(finalLocation , recursive=TRUE, showWarnings=FALSE)

# ----- Get data -----

# Diseases (two diseases: ischemic heart disease and type 2 diabetes)

mslt_general <- read_csv("./data/mslt_general.csv")


# Mortality projections

death_projections <- read_csv("data/deaths_projections.csv") %>% 
  dplyr::filter(location == "Victoria", assumption == "medium")

# Disease trends

incidence_trends <- read_csv("./data/incidence_trends.csv")
mortality_trends <- read_csv("./data/mortality_trends.csv")

# -----  Life table modelling ----

# Variable names: 
## mx=average mortality rate at age x
## qx=probability of dying between age x and age x + 1 (mortality risk)
## lx= number of survivors
## dx= number who die between the age x an x + 1
## Lx= number of person-years lived by cohort to age x + 1/2
## ex: life expectancy
## pyld_rate= prevalent years live with disability rate from all causes
## Lwx: health-adjusted life years
## ewx: health-adjusted life expectancy

## Function inputs that can be change

# in_mid_age: 17 or 22 or 27 or 32 or 37 or 42 or 47 or 52 or 57 or
# 62 or 67 or 72 or 77 or 82 or 87 or 92 or 97
# in_sex: "male"or "female"
# death_rates: NA or deaths projections
# scenario: 0 to 1 (represents reduction in all-cause mortality for all years)

## Use function to generate life tables for: 

# Base case Without mortality trends

life_table_nt <- RunLifeTable(
  in_data    = mslt_general,
  in_sex      = "female",
  in_mid_age  = 17,
  death_rates = NA, 
  scenario = 0
) %>%
  dplyr::mutate(Data="No trends")  # Add variable to compare in graph

# View life table for base case without trends in mortality
View(life_table_nt)
# Save
write_csv(life_table_nt, "./outputs/life_table_nt.csv")

# Graph mortality

ggplot(life_table_nt, aes(x=age, y=mx)) +
  geom_line() +
         theme_classic() +
  
  labs(x="Age cohort", y="Mortality rate") +
  theme_classic() +
  scale_x_continuous(limits=c(17,100),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(title="Mortality rate cohort no trends") +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12))


# Base case With mortality trends

life_table_wt <-  RunLifeTable(
  in_data    = mslt_general,
  in_sex      = "female",
  in_mid_age  = 17,
  death_rates = death_projections, 
  scenario = 0.00
) %>%
  dplyr::mutate(Data="With trends")  # Add variable to compare in graph

# View life table for base case with trends in mortality
View(life_table_wt)
# Save
write_csv(life_table_wt, "./outputs/life_table_wt.csv")

# Graph mortality

ggplot(life_table_wt, aes(x=age, y=mx)) +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits=c(17,100),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Mortality rate") +
  theme_classic() +
  labs(title="Mortality rate cohort with trends") +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12))

# Difference life years with and without trends in mortality

sum(life_table_wt$Lx) - sum(life_table_nt$Lx)

# Difference health-adjusted life years with and without trends in mortality

sum(life_table_wt$Lwx) - sum(life_table_nt$Lwx)

# Graph mortality rates difference with and without trends

## Data frame with both data sets

death_rate_compare <- life_table_nt %>%
  bind_rows(life_table_wt)


ggplot() +
  geom_line(data=death_rate_compare, aes(x=age, y=mx, color=Data)) +
  theme_classic() +
  scale_x_continuous(limits=c(17,100),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Mortality rate") +
  theme_classic() +
  labs(title="Mortality rate comparison for a cohort") +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12))


# Scenario testing

## Scenario 1: Decrease in all cause mortality at every year of simulation
### Choose value or zero for no change (default is zero)

# Scenario
life_table_sc <- RunLifeTable(
  in_data    = mslt_general,
  in_sex      = "female",
  in_mid_age  = 17,
  death_rates = death_projections, 
  scenario = 0.10
) %>%
  dplyr::mutate(Data="Scenario")  # Add variable to compare in graph

View(life_table_sc)
write_csv(life_table_sc, "./outputs/life_table_wt.csv")


# Difference life years scenario and base case with trends

sum(life_table_sc$Lx)- sum(life_table_wt$Lx)

# Difference health-adjusted life years scenario and base case with trends

sum(life_table_sc$Lwx)- sum(life_table_wt$Lwx)

# Difference life expectancy

(life_table_sc$ex[1] - life_table_wt$ex[1])*365

# Graph

## Data frame with both data sets

death_rate_scenario <- life_table_wt %>%
  bind_rows(life_table_sc)



ggplot() +
  geom_line(data=death_rate_scenario, aes(x=age, y=mx, color=Data)) +
  theme_classic() +
  scale_x_continuous(limits=c(17,100),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Mortality rate") +
  theme_classic() +
  labs(title="Mortality rate comparison for a cohort") +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12))

# -----  Disease modelling ----

# Variable names

## dw=disability weight for disease
## incidence_disease=risk of becoming diseased
## case_fatality_disease=risk of dying from the disease for people with the disease
## Sx=number of healthy simulated people
## Cx=number of simulated people with the disease
## Dx=number of simulated people who die from the disease
## Tx=Sx + Cx + Dx (has to be 1000)
## PYx=Person years at risk
## mx=proportion of peole who die from the disease (mortality)
## px=proportion of people who have the disease (prevalence)

## Function inputs that can change

# in_mid_age: 17 or 22 or 27 or 32 or 37 or 42 or 47 or 52 or 57 or
# 62 or 67 or 72 or 77 or 82 or 87 or 92 or 97
# in_sex: "male"or "female"
# in_disease" "ishd" or "tbalc"
# incidence_trends: NA or incidence_trends
# mortality_trends: NA or mortality_trends
# scenario_inc: 0 to 1 (represents reduction in incidence of disease)
# scenario_cf: 0 to 1 (represents reduction in case fatality of disease)

## Use function to generate disease tables for: 

# Base case Without trends

disease_ihd_nt <- RunDisease(
  in_data         = mslt_general,
  in_mid_age       = 17,
  in_sex           = "female",
  in_disease       = "ishd",
  incidence_trends = NA,
  mortality_trends = NA, 
  scenario_inc = 0, 
  scenario_cf = 0
) %>% mutate(Data="No trends")

View(disease_ihd_nt)

# Base case with trends

disease_ihd_wt <- RunDisease(
  in_data         = mslt_general,
  in_mid_age       = 17,
  in_sex           = "female",
  in_disease       = "ishd",
  incidence_trends = incidence_trends,
  mortality_trends = mortality_trends, 
  scenario_inc = 0, 
  scenario_cf = 0
) %>% mutate (Data="With trends")

View(disease_ihd_wt)

# Difference base case incidence numbers and death numbers for inclusion and non inclusion of trends

# Some calculations first: incidence/deaths numbers are a function of 
# incidence rate, the proportion of people who have the disease (cannot get it again) 
# and the number of persons-year lived by the cohort
# Note: in a complete model, incidence impacts on prevalence and mortality, which in turn
# impact of persons-years lived by the cohort. The example calculations for changes
# incidence and deaths are not capturing this. The complete model of life table with 
# two diseases captures it. 

# Incidence
sum(disease_ihd_wt$incidence_disease*(1-disease_ihd_wt$px)*
      life_table_wt$Lx) - sum(disease_ihd_nt$incidence_disease*(1-disease_ihd_nt$px)
                             *life_table_wt$Lx)
# Deaths
sum(disease_ihd_wt$mx*life_table_wt$Lx) - sum(disease_ihd_nt$mx*life_table_wt$Lx)

# Scenario incidence

disease_sc <- RunDisease(
  in_data         = mslt_general,
  in_mid_age       = 17,
  in_sex           = "female",
  in_disease       = "ishd",
  incidence_trends = incidence_trends,
  mortality_trends = mortality_trends, 
  scenario_inc = 0.10, 
  scenario_cf = 0
) %>% mutate(Data="Scenario")

View(disease_sc)

# Incidence
sum(disease_sc$incidence_disease*(1-disease_sc$px)*
      life_table_wt$Lx) - sum(disease_sc$incidence_disease*(1-disease_sc$px)
                              *life_table_wt$Lx)
# Deaths
sum(disease_ihd_sc_inc$mx*life_table_wt$Lx) - sum(disease_ihd_wt$mx*life_table_wt$Lx)


# Graph with all three data sets (no trends, trends and scenario)

disease_example <- disease_ihd_nt %>%
  bind_rows(disease_ihd_wt) %>%
  bind_rows(disease_sc)

ggplot() +
  geom_line(data=disease_example, aes(x=age, y=mx, color=Data)) +
  theme_classic() +
  scale_x_continuous(limits=c(17,97),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Incidence rate") +
  theme_classic() +
  labs(title="Incidence rate comparison for a cohort") +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12))


# -----  Life table and disease modelling ----

# Integration of life table and disease modelling for scenario analysis
# The model calculates base case and scenario life tables and disease life 
# tables and generates a dataframe with difference for disease incidence, 
# deaths, life years and health-adjusted life years. 

# Test that the function is doing the correct job


### NT Filtering by age and sex
output_test <- CalculationModel(in_data=mslt_general,
                 in_sex="female",
                 in_mid_age=17,
                 in_death_trends=death_projections,
                 in_disease=c("ishd", "tbalc"), 
                 incidence_trends=incidence_trends, 
                 mortality_trends=mortality_trends, 
                 scenario_inc=0, 
                 scenario_cf=0
)

# Some checks: all differences should be zero
## Life years
sum(output_test$Lx_diff)
## Health-adjusted life years
sum(output_test$Lwx_diff)
## Incidence of disease (cases)
sum(output_test$inc_num_diff_ishd)
sum(output_test$inc_num_diff_tbalc)
sum(output_test$mx_num_diff_ishd)
sum(output_test$mx_num_diff_tbalc)


# Scenario modelling
output_df <- CalculationModel(in_data=mslt_general,
                                in_sex="female",
                                in_mid_age=17,
                                in_death_trends=death_projections,
                                in_disease=c("ishd", "tbalc"), 
                                incidence_trends=incidence_trends, 
                                mortality_trends=mortality_trends, 
                                scenario_inc=0.10, 
                                scenario_cf=0.00
)

# Check outputs

## Life years
sum(output_df$Lx_diff)
## Health-adjusted life years
sum(output_df$Lwx_diff)
## Incidence of disease (cases)
sum(output_df$inc_num_diff_ishd)
sum(output_df$inc_num_diff_tbalc)
sum(output_df$mx_num_diff_ishd)
sum(output_df$mx_num_diff_tbalc)


# Some graphs
## Reformat data for graphs

output_graphs <- output_df %>%
                select(age, sex, contains("diff")) %>%
                pivot_longer(cols=inc_num_diff_ishd:ewx_diff,
                names_to = "measure")

## Plot life years and health adjusted life years over time

life_years_data <- output_graphs %>% filter(measure %in% c("Lx_diff", "Lwx_diff"))

ggplot() +
  geom_line(data=life_years_data, aes(x=age, y=value)) +
  aes(color=measure) +
  theme_classic() +
  scale_x_continuous(limits=c(17,97),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Life years and HALYs gained") +
  theme_classic() +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12), 
        legend.title = element_blank()) +
   scale_color_discrete(name = "Measure", labels = c("Health-adjusted life years"
                                                     , "Life years"))

## Plot incidence and mortality over time

disease_data <- output_graphs %>% filter(measure %in% 
                                           c("inc_num_diff_ishd",  
                                             "inc_num_diff_tbalc", 
                                             "mx_num_diff_ishd",  
                                             "mx_num_diff_tbalc"))

ggplot() +
  geom_line(data=disease_data, aes(x=age, y=value)) +
  aes(color=measure) +
  theme_classic() +
  scale_x_continuous(limits=c(17,97),breaks=seq(17,100,10), expand=c(0,0)) +
  labs(x="Age cohort", y="Change in diseases") +
  theme_classic() +
  theme(plot.title=element_text(size=12, hjust = 0.5), 
        axis.text=element_text(size=12),
        axis.title = element_text(size=12), 
        legend.title = element_blank()) +
  scale_color_discrete(name = "Measure", labels = 
                         c("Incidence ischemic heart disease",
                           "Incidence lung cancer",
                          "Death ischemic heart disease",
                          "Death lung cancer"))




















### Complete model for all age and sex cohorts





# Compare death rates and life years with and without projections


# Decrease in incidence of ischemic heart disease

# Decrease in all cause mortality




# ----- Visualize results -----

