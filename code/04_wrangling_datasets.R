#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Data Wrangling | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################

## Remove all data in memory and load previously cleaned datasets.
rm(list = ls())
load("data/output/sdy296.RData")      #Read cleaned SDY296 data    
load("data/output/sdy301.RData")      #Read cleaned SDY301 data 


#' Append both SDY296 and SDY301 datasets --row bind.
#' arm_subj_296: Linked arm and subjects data for SDY296
#' arm_subj_301: Linked arm and subjects data for SDY301
lnk_arm_sub <- rbind (arm_subj_296 %>% mutate (link = "SDY296"),
                      arm_subj_301 %>% mutate (link = "SDY301")) 

#' hai2neut_result_296: Linked HAI and virus neutralisation titer at days 0 & 28 for SDY296
#' hai2neut_result_301: Linked HAI and virus neutralisation titer at days 0 & 28 for SDY301
lnk_hi2neut <- rbind (hai2neut_result_296 %>% mutate (link = "SDY296"),
                      hai2neut_result_301 %>% mutate (link = "SDY301")) %>% 
  
                #' We are not including strain name in labels so that labels are consistent.
                mutate (VIRUS_STRAIN_REPORTED = str_remove(VIRUS_STRAIN_REPORTED,
                                                           " H1N1| H3N2"),
                        #' Fixing inconsistencies (7 instead of 07) in the strain name for A/California/7/2009.
                        VIRUS_STRAIN_REPORTED = replace (VIRUS_STRAIN_REPORTED,
                                                         which(VIRUS_STRAIN_REPORTED == "A/California/7/2009"),
                                                         "A/California/07/2009"))
#' Derived meausred of sero-conversion and sero-protection.
#' sero-conversion: Four-fold increase in antibody titers between days 0 & 28.
lnk_surrogate <-  lnk_hi2neut %>% 
                  mutate (hai_seroconversion = Day_28_hai/Day_0_hai,
                          neut_seroconversion = Day_28_neut_ab/Day_0_neut_ab) %>% 
                  filter (!is.na(hai_seroconversion) | !is.na(neut_seroconversion)) %>% 
                  mutate (hai_sero_stat = if_else(hai_seroconversion >= 4,
                                             "Seroconverted", "Seroreverted",
                                             missing = NA),
                          neut_sero_stat = if_else(neut_seroconversion >= 4,
                                                 "Seroconverted", "Seroreverted",
                                                 missing = NA)) %>% 
                  mutate (hai_sero_prot = ifelse((Day_28_hai >= 40),
                                                "Protected", "Unprotected"),
                          neut_sero_prot = ifelse((Day_28_neut_ab >= 40),
                                                 "Protected", "Unprotected")) %>% 
                  clean_names()


lnk_antiBdy_lvl <- lnk_surrogate %>% 
                   select(subject_accession, link,
                          virus_strain_reported,
                          day_28_hai, day_28_neut_ab,
                          hai_seroconversion,
                          neut_seroconversion) %>% 
                    #' Reshape data to long so that all antibody responses are in a 
                    #' single column "names" and their corresponding values in "values".
                    pivot_longer(cols = c(day_28_hai:neut_seroconversion),
                                 values_to = "values",
                                 names_to = "names") %>% 
                    mutate (antibody = str_extract(names, "hai|neut"),    ## Create new columns that uniquely identifies each antibody type [HAI/NEUT]
                            measure = ifelse(str_detect(names, "day_28"), ## Also create an indication of the measures of antibody responses [fold vs titer].
                                             "titer", "fold")) %>% 
                    select (-c(names)) %>% 
                    #' Reshape data to wide using a combination of virus_strain_reported,
                    #' measure and antibody in the column names.
                    pivot_wider(names_from = c(virus_strain_reported, measure, antibody),
                                values_from = values) %>% 
                    clean_names()

## Linked lab data.
lnk_lab <- rbind (updt_lab_296 %>% mutate (link = "SDY296"),
                      updt_lab_301 %>% mutate (link = "SDY301")) %>% 
            mutate (time_of_enrollment = replace(time_of_enrollment,  ## Fix minor inconsistencies in the time of enrollment. 4=3.
                                                 which (time_of_enrollment == 4),
                                                 3),
                    time_of_enrollment = replace(time_of_enrollment,  ## Fix minor inconsistencies in the time of enrollment. 8=7.
                                                 which (time_of_enrollment == 8),
                                                 7)) %>%
            ## Only keep lab data collected until 240 days.
            filter (time_of_enrollment > 0 & time_of_enrollment <= 240) %>% 
            filter (!is.na(infect_stat)) %>%  ## Keep only records in which there is a valid infection status [infected/uninfected]
            group_by (subject_accession, link) %>% arrange() %>% 
            #' Check whether this participant had been infected at any time point between 0 and 120 days.
            #' If they tested positive at any time, code them as Infected and unifected if otherwise.
            summarise (infect_status = ifelse(any(infect_stat == "Infected"),
                                           "Infected", "Uninfected")) %>% 
            janitor::clean_names()

  
lnk_corr_surr <- lnk_lab %>% 
                 left_join(., lnk_antiBdy_lvl,
                           c("subject_accession", "link"))
          
## Keep only the relevant datasets for subsequent data analysis.
gdata::keep(lnk_arm_sub, lnk_hi2neut, 
            lnk_corr_surr, lnk_antiBdy_lvl,
            lnk_surrogate, lnk_lab, sure = TRUE)
