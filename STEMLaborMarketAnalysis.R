# January 2025 STEM labor market analysis 

##########################
# LIBRARIES
##########################
library(ipumsr)
library(tidyverse)
library(dplyr)
library(readxl)
library(Hmisc)
library(stargazer)
library(fixest)
library(marginaleffects)
library(modelsummary)

##########################
# IMPORT AND CLEAN DATA
##########################

### Importing data extract from the 2023 one-year ACS visa IPUMS
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00031.xml")
STEM_data_raw <- read_ipums_micro(ddi)

### Filter data for relevant population
STEM_data <- STEM_data_raw %>%
  filter(EDUCD > 100 & EDUCD <120,
         #Filtering for BA or higher
         AGE > 16 & AGE < 75,
         #Filtering for age 16 to 75
         WKSWORK1>49,
         #Filtering for year-round workers
         UHRSWORK >34,
         #Filtering for full-time workers
         OCC >0,
         #Filtering for non-missing occupations
         INCWAGE <999998 & INCWAGE>0,
         #Filter for NA/missing income 
         )

### Import STEM occupations (lightly edited) list from https://ncses.nsf.gov/pubs/nsb20245/glossary

Occupation_List <- read_excel("STEM_occupations_list.xlsx")

# Drop all columns except occupation name, group, and code. Take out broader groups with "NA" occupation codes.

Occupation_List <- Occupation_List %>% select(`Broad occupational group`,`2021 OCC`, `Detailed occupations`, Small_occ_groups ) %>% filter(`Broad occupational group` != "na")

# Convert occupation columns to numeric
STEM_data$OCC <- as.numeric(STEM_data$OCC)
Occupation_List$`2021 OCC`<- as.numeric(Occupation_List$`2021 OCC`)

# Join occupation groups into the filtered dataset
STEM_data <- left_join(STEM_data, Occupation_List, by = c("OCC" = "2021 OCC"))

# Military, police, and firefighters in the dataset are not given a STEM/non-STEM definition. Manually giving them non-STEM designations. 
STEM_data <- STEM_data %>%
  mutate(`Broad occupational group` = ifelse(is.na(`Broad occupational group`), 
                                             "Non-STEM occupations", 
                                             `Broad occupational group`),
         Small_occ_groups = ifelse(is.na(Small_occ_groups), 
                                    "Transportation and Material Moving Occupations", 
                                    Small_occ_groups))

### Import degree field codes, STEM/non-STEM groups, and more specific groups (heavily edited manually in Excel)

Degree_fields <- read_excel("Degree_fields_codes_groups.xlsx")
  
# Join STEM filtered data with degree fields 

STEM_data <- left_join(STEM_data, Degree_fields, by = c("DEGFIELDD" = "Degree_code"))

##########################
# AVERAGE & MEDIAN PAY ANALYSIS
##########################

# Calculate the average pay of the whole sample

Avg_pay_all <- STEM_data %>% summarise(avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT))

# Median

Med_pay_all <- STEM_data %>% summarise(med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and average pay of workers in STEM and non-STEM occupations

STEM_vs_non_occs <- STEM_data %>%
  group_by(`Broad occupational group`) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and average earnings by STEM vs non-STEM major

Earnings_by_STEM_major <- STEM_data %>%
  group_by(Degree_group_big) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and average earnings by major grouping

Earnings_by_major_group <- STEM_data %>%
  group_by(Degree_group_small) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and average earnings by college major 

Earnings_by_major <- STEM_data %>%
  group_by(Degree) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate number of workers and earnings by STEM occupation and STEM degree status 

STEM_occ_vs_major_binary <- STEM_data %>%
  group_by(`Broad occupational group`,Degree_group_big) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and earnings by STEM occupation and major grouping

STEM_occ_vs_major_group <- STEM_data %>%
  group_by(`Broad occupational group`,Degree_group_small) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and earnings by STEM occupation and exact major 

STEM_occ_vs_major_specific <- STEM_data %>%
  group_by(`Broad occupational group`,Degree) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and earnings for occupation groups by STEM/non-STEM major

STEM_occ_groups_vs_major_binary <- STEM_data %>%
  group_by(Small_occ_groups,Degree_group_big) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and earnings for specific occupations by STEM/non-STEM major

STEM_occs_vs_major_binary <- STEM_data %>%
  group_by(`Detailed occupations`,Degree_group_big) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))


# Calculate the number of workers and earnings for occupation groups by major grouping

STEM_occ_groups_vs_major_group <- STEM_data %>%
  group_by(Small_occ_groups,Degree_group_small) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))


# Calculate the number of workers and earnings for specific occupations by major grouping

STEM_occs_vs_major_group <- STEM_data %>%
  group_by(`Detailed occupations`,Degree_group_small) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate the number of workers and earnings for specific occupations by specific majors 

STEM_occs_vs_major_specific <- STEM_data %>%
  group_by(`Detailed occupations`,Degree) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Calculate pay for STEM grads in non-STEM occupation groups 

STEM_grad_nonstem_destinations <- STEM_data %>%
  filter(Degree_group_big=="STEM Degree",
         `Broad occupational group`!="S&E occupations")%>%
  group_by(Small_occ_groups) %>%
  summarise(total =sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Share of non-STEM destinations for STEM grads

STEM_grad_nonstem_destinations <- STEM_grad_nonstem_destinations %>%
  mutate(share = total/Total_STEM_nonSTEM)

##########################
# ECON MAJOR ANALYSIS
##########################

# Total econ majors
STEM_data %>% filter(Degree=="Economics") %>% summarise(total = sum(PERWT))

# Calculate the number of workers and earnings for economics majors working in STEM vs non-STEM occupations

Econ_majors_nonstem <- STEM_data %>% filter(Degree=="Economics", `Broad occupational group`!="S&E occupations") %>% group_by(`Detailed occupations`) %>% summarise(total =sum(PERWT),
                                                                                                            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))
# Calculate the number of workers and earnings for economics majors by detailed occupations 

Econ_majors <- STEM_data %>% filter(Degree=="Economics") %>% group_by(`Detailed occupations`) %>% summarise(total =sum(PERWT),
                                                                                           avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))


# Calculate the number of workers and earnings for economics majors by detailed, non-STEM occupations 
Top_nonSTEM_econ_occs <- STEM_data %>% filter(Degree=="Economics", `Broad occupational group`!= "S&E Occupations") %>% group_by(`Detailed occupations`) %>% summarise(total =sum(PERWT),
                                                                                             avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                             med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5)) %>%
  arrange(desc(total))


STEM_data %>% filter(`Detailed occupations`=="Economists") %>% 
  summarise(total = sum(PERWT),
    avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

STEM_data %>% filter(`Detailed occupations`=="Economists") %>% summarise(total = sum(PERWT))

# Median pay across occupation groups (no filter)
STEM_data %>% group_by(`Broad occupational group`) %>%
  summarise(total = sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))
# Total workers and median pay in the sample 
STEM_data %>% summarise(total = sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))


##########################
# STEM MAJOR PREMIUM BY OCCUPATION
##########################

# Calculate the STEM degree premium in STEM vs non-STEM occupation binary
STEM_Premium_sector <-
  STEM_occ_vs_major_binary %>%
  select(-Workers,-avg_INCWAGE_per_person) %>%
  pivot_wider(
    names_from = Degree_group_big,
    values_from = med_pay) %>%
  mutate(STEM_premium = `STEM Degree` - `Non-STEM`)

# Calculate the STEM degree premium in occupation groups
STEM_premium_occ_group <-
  STEM_occ_groups_vs_major_binary %>%
  select(-Workers, -avg_INCWAGE_per_person) %>%
  pivot_wider(
    names_from = Degree_group_big,
    values_from = med_pay) %>%
  mutate(STEM_premium = `STEM Degree` - `Non-STEM`)

# Calculate the STEM degree premium in specific occupations 
STEM_premium_specific_occ <-
  STEM_occs_vs_major_binary %>%
  select(-Workers, -avg_INCWAGE_per_person) %>%
  pivot_wider(
    names_from = Degree_group_big,
    values_from = med_pay) %>%
  mutate(STEM_premium = `STEM Degree` - `Non-STEM`)

# Calculate the STEM degree premium in non-STEM occupation groups
STEM_worker_premium_nonSTEM_occs <-
  STEM_data %>%
  filter(`Broad occupational group`=="Non-STEM occupations") %>%
  group_by(Degree_group_big, Small_occ_groups) %>%
  summarise(med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5)) %>%
  pivot_wider(
    names_from = Degree_group_big,
    values_from = med_pay) %>%
  mutate(STEM_premium = `STEM Degree` - `Non-STEM`)

# Calculate the STEM degree premium in non-STEM occupations (minimum 1,000 FT, YR workers)
STEM_worker_premium_nonSTEM_occs_specific <-
  STEM_data %>%
  filter(`Broad occupational group`=="Non-STEM occupations") %>%
  group_by(Degree_group_big, `Detailed occupations`) %>%
  summarise(total = sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5)) %>%
  filter(total>1000) %>%
  select(-total) %>%
  pivot_wider(
    names_from = Degree_group_big,
    values_from = med_pay) %>%
  mutate(STEM_premium = `STEM Degree` - `Non-STEM`)
  
  

##########################
# ENGINEERING, BUSINESS, AND ART ANALYSIS
##########################

# Engineering majors' pay by occupation group

Engineering_degree_occs_pay <- STEM_data %>%
  filter(Degree_group_small=="Engineering") %>%
  group_by(Small_occ_groups) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Engineering majors' pay by specific occupation

Engineering_degree_occs__specific_pay <- STEM_data %>%
  filter(Degree_group_small=="Engineering") %>%
  group_by(`Detailed occupations`) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

STEM_data %>% group_by(Small_occ_groups) %>%
  summarise(Workers = sum(PERWT),
            avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
            med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))



# Art majors in art occupations
STEM_data %>% filter(Degree_group_small=="Visual and Performing Arts", Small_occ_groups=="Arts, design, entertainment, sports, and media occupations") %>%   summarise(Workers = sum(PERWT),
                                                                                                                                                                       avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                                                                                       med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))
# Art majors in non-art occupations
STEM_data %>% filter(Degree_group_small=="Visual and Performing Arts", Small_occ_groups!="Arts, design, entertainment, sports, and media occupations") %>%   summarise(Workers = sum(PERWT),
                                                                                                                                                                       avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                                                                                       med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Business majors in management or business occupations
STEM_data %>% filter(Degree_group_small=="Business", Small_occ_groups=="Business" | Small_occ_groups=="Management") %>%   summarise(Workers = sum(PERWT),
                                                                                                                                                                       avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                                                                                       med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))

# Business majors in non-managememnt or business occupations
STEM_data %>% filter(Degree_group_small!="Business", Small_occ_groups=="Business" & Small_occ_groups!="Management") %>%   summarise(Workers = sum(PERWT),
                                                                                                                                    avg_INCWAGE_per_person = sum(INCWAGE * PERWT) / sum(PERWT),
                                                                                                                                    med_pay = wtd.quantile(INCWAGE, weights = PERWT, probs = 0.5))


##########################
# STEM grads in non-STEM occupations 
##########################

# How many STEM grads work in non-STEM jobs earn higher than the STEM average ($129,245)?
STEM_data %>% 
  mutate(Non_STEM_binary = ifelse(`Broad occupational group`=="S&E occupations",0,1),
    HT_STEM_avg = ifelse(INCWAGE>129245.58, PERWT*Non_STEM_binary,0)) %>%
  filter(Degree_group_big =="STEM Degree",
         `Broad occupational group`!="S&E occupations") %>%
  summarise(total=sum(HT_STEM_avg))
  
# How many STEM grads work in non-STEM jobs earn higher than the STEM MEDIAN ($110,000)?
STEM_data %>% 
  mutate(Non_STEM_binary = ifelse(`Broad occupational group`=="S&E occupations",0,1),
         HT_STEM_avg = ifelse(INCWAGE>110000, PERWT*Non_STEM_binary,0)) %>%
  filter(Degree_group_big =="STEM Degree",
         `Broad occupational group`!="S&E occupations") %>%
  summarise(total=sum(HT_STEM_avg))

# Total STEM grads in non-STEM jobs
STEM_data %>% filter(`Broad occupational group`!="S&E occupations", Degree_group_big=="STEM Degree") %>% summarise(total = sum(PERWT))

# By degree group: How many STEM grads work in non-STEM jobs earn higher than the STEM average ($129,245)?
STEM_majorsnonstem_exceed_average <- STEM_data %>%
  filter(Degree_group_big=="STEM Degree", `Broad occupational group`!= "S&E occupations") %>%
  mutate(higher_than_avg_stem = ifelse(INCWAGE>129246, PERWT,0)) %>%
  group_by(Degree_group_small) %>%
  summarise(total = sum(PERWT),
            higher_than_avg = sum(higher_than_avg_stem)) %>%
  mutate(share_higher = higher_than_avg/total)

# By degree group: How many STEM grads work in non-STEM jobs earn higher than the STEM median ($110,000)?
STEM_majorsnonstem_exceed_median <- STEM_data %>%
  filter(Degree_group_big=="STEM Degree", `Broad occupational group`!= "S&E occupations") %>%
  mutate(higher_than_avg_stem = ifelse(INCWAGE>110000, PERWT,0)) %>%
  group_by(Degree_group_small) %>%
  summarise(total = sum(PERWT),
            higher_than_median = sum(higher_than_avg_stem)) %>%
  mutate(share_higher = higher_than_median/total)



##########################
# Regression
##########################

# Prep dataset for regression. Same filters as above except we don't filter only for full-time, year-round workers. 
                             
STEM_data_reg <- STEM_data_raw %>%
  filter(EDUCD > 100 & EDUCD <120,
         #Filtering for BA or higher
         AGE > 16 & AGE < 75,
         #Filtering for age 16 to 75
         OCC >0,
         #Filtering for non-missing occupations
         INCWAGE <999998 & INCWAGE>0,
         #Filter for NA/missing income 
  )

STEM_data_reg$OCC <- as.numeric(STEM_data_reg$OCC)

# Join occupation groups into the filtered dataset
STEM_data_reg <- left_join(STEM_data_reg, Occupation_List, by = c("OCC" = "2021 OCC"))

# Military, police, and firefighters in the dataset are not given a STEM/non-STEM definition. Manually giving them non-STEM designations. 
STEM_data_reg <- STEM_data_reg %>%
  mutate(`Broad occupational group` = ifelse(is.na(`Broad occupational group`), 
                                             "Non-STEM occupations", 
                                             `Broad occupational group`),
         Small_occ_groups = ifelse(is.na(Small_occ_groups), 
                                   "Transportation and Material Moving Occupations", 
                                   Small_occ_groups))

STEM_data_reg <- left_join(STEM_data_reg, Degree_fields, by = c("DEGFIELDD" = "Degree_code"))

# STEM vs non-STEM binary variable, occupations
STEM_data_reg$binary_occ <- ifelse(STEM_data_reg$`Broad occupational group`=="S&E occupations","STEM job","Non-STEM job")

# STEM vs non-STEM binary variable, degree 	
STEM_data_reg$binary_deg <- ifelse(STEM_data_reg$Degree_group_big=="STEM Degree","STEM Degree", "Non-STEM degree")

# Create total hours worked variable
STEM_data_reg$hours_worked <- STEM_data_reg$WKSWORK1*STEM_data_reg$UHRSWORK

# Convert to factor variables and change levels
STEM_data_reg$EDUCD <- factor(STEM_data_reg$EDUCD)
STEM_data_reg$`Broad occupational group` <- as.factor(STEM_data_reg$`Broad occupational group`)
STEM_data_reg$Degree_group_big <- as.factor(STEM_data_reg$Degree_group_big)
STEM_data_reg$`Broad occupational group` <- relevel(STEM_data_reg$`Broad occupational group`, ref = "STEM middle-skill occupations")
STEM_data_reg$Degree_group_big <- relevel(STEM_data_reg$Degree_group_big, ref = "Non-STEM")


# Full regression - controlling for age, education. 4-code occupation variable crossed with binary STEM/nonSTEM

model1_fixest <- feols(
  log(INCWAGE) ~ `Broad occupational group`* Degree_group_big + log(hours_worked) + AGE + I(AGE^2) | EDUC , 
  data = STEM_data_reg, 
  weights = ~PERWT
)

# Create a named list of models for clarity in the summary table
models_list <- list(
  "Model 1" = model1_fixest
)

# Generate and display the summary table
modelsummary(models_list,
             coef_omit = "log\\(hours_worked\\)|^AGE$|I\\(AGE\\^2\\)|^EDUC$",
             estimate = "{estimate}{stars}")

# Compute marginal effects for Degree_group_big across each Broad occupational group
me_results <- slopes(
  model1_fixest,
  variables = "Degree_group_big",
  by = "Broad occupational group"
)

# View a summary of the marginal effects
summary(me_results)

# Print detailed marginal effects results
print(me_results)

me_results2 <- me_results %>% filter(contrast=="STEM Degree - Non-STEM")
me_results2


# Rename S&E occupation labels to STEM

me_results2 <- me_results2 %>%
  mutate(
    `Broad occupational group` = case_when(
      `Broad occupational group` == "S&E occupations" ~ "STEM occupations",
      `Broad occupational group` == "S&E occupations-related occupations" ~ "STEM related occupations",
      TRUE ~ `Broad occupational group`,
      effect = exp(estimate)-1,
      conf_lower = exp(conf.low)-1,
      conf_high = exp(conf.high)-1
    )
  )

levels(me_results2$`Broad occupational group`) <- c("STEM occupations", "STEM related occupations", "STEM middle-skill occupations","Non-STEM occupations")

me_results2$`Broad occupational group` <- factor(me_results2$`Broad occupational group`, levels = c("STEM occupations", "STEM related occupations", "STEM middle-skill occupations","Non-STEM occupations"))

# Plot effect sizes 

ggplot(me_results2, 
       aes(x = contrast, 
           y = estimate, 
           ymin = conf.low, 
           ymax = conf.high)) +
  geom_hline(yintercept = 0) + 
  geom_pointrange() +
  facet_grid(~ `Broad occupational group`) +
  labs(
    title = "Marginal Effects of Degree Type by Occupation Group",
    x = "Difference between STEM and non-STEM degree holders' wages",
    y = "Marginal Effect on log(wage income)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())







