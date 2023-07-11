#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Link Study Datasets | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################

source ("code/02_read_datasets.R") ## This is to source the read data script.

updt_neut_301 <- neut_ab_titer_result_301 %>% 
  
                 ## Order data by the following variables
                 arrange (SUBJECT_ACCESSION,    ## SUBJECT_ACCESSION - unique IDs for each healthy patient. 
                          BIOSAMPLE_ACCESSION,  ## BIOSAMPLE_ACCESSION - unique IDs for each biological sample.
                          EXPERIMENT_ACCESSION, ## EXPERIMENT_ACCESSION - unique IDs for the experiment [virus Neutralisation].
                          VIRUS_STRAIN_REPORTED) %>%    ## VIRUS_STRAIN_REPORTED - Virus strain reported.
          
                 ## Group dataset by the following variables. All described above
                 group_by(SUBJECT_ACCESSION, 
                          BIOSAMPLE_ACCESSION,
                          EXPERIMENT_ACCESSION,
                          VIRUS_STRAIN_REPORTED) %>% 
                 ## During data analytics, we discovered multiple counts of titers for each viral strain for each patient in this dataset.
                 ## As a result, we have decided to keep only the first record.
                 mutate (n = 1:n()) %>% 
                 filter (n == 1) %>% ungroup () %>% 
  
                 ## Keep only the patients unique IDs, study time, antibody titers for virus neutralisation and virus strain.
                 select(SUBJECT_ACCESSION,
                        STUDY_TIME_COLLECTED,
                        VALUE_REPORTED,
                        VIRUS_STRAIN_REPORTED) %>% 
                  mutate (STUDY_TIME_COLLECTED = paste0("Day_", STUDY_TIME_COLLECTED,
                                                       "_neut_ab")) %>% 
                  arrange (SUBJECT_ACCESSION, STUDY_TIME_COLLECTED) %>% 
  
                  ## Reshape data to wide format, so that the study time combined with the experiment
                  ## are column names containing the antibody titers for each study period.
                  pivot_wider(names_from = STUDY_TIME_COLLECTED,
                              values_from = VALUE_REPORTED) %>% 
  
                  #' Generate an additional identifier which combines unique 
                  #' patient ID with virus strain.
                  #' This will be used later.
                  mutate (id = paste0(VIRUS_STRAIN_REPORTED,
                                      SUBJECT_ACCESSION))


updt_hai_301 <- hai_result_301 %>% 
  
                ## Order data by the following variables
                arrange (SUBJECT_ACCESSION,                ## SUBJECT_ACCESSION - unique IDs for each healthy patient.
                         BIOSAMPLE_ACCESSION,              ## BIOSAMPLE_ACCESSION - unique IDs for each biological sample.
                         EXPERIMENT_ACCESSION,             ## EXPERIMENT_ACCESSION - unique IDs for each experiment [HAI].
                         VIRUS_STRAIN_REPORTED) %>%        ## VIRUS_STRAIN_REPORTED - Virus strain reported.
  
                ## Group dataset by the following variables. All described above
                group_by(SUBJECT_ACCESSION, 
                         BIOSAMPLE_ACCESSION,
                         EXPERIMENT_ACCESSION,
                         VIRUS_STRAIN_REPORTED) %>% 
  
                #' During data analytics, we discovered multiple counts of titers 
                #' for each viral strain for each patient in SDY301.
                #' As a result, we have decided to keep only the first record both here and in SDY301.
                mutate (n = 1:n()) %>% 
                filter (n == 1) %>% ungroup () %>% 
  
                ## Keep only the patients unique IDs, study time, antibody titers for HAI and virus strain.
                select(SUBJECT_ACCESSION,
                       STUDY_TIME_COLLECTED,
                       VALUE_REPORTED,
                       VIRUS_STRAIN_REPORTED) %>% 
                mutate (STUDY_TIME_COLLECTED = paste0("Day_", STUDY_TIME_COLLECTED,
                                                     "_hai")) %>% 
                arrange (SUBJECT_ACCESSION, STUDY_TIME_COLLECTED) %>% 
  
                ## Reshape data to wide format, so that the study time combined with the experiment
                ## are column names containing the antibody titers for each study period.
                pivot_wider(names_from = STUDY_TIME_COLLECTED,
                            values_from = VALUE_REPORTED) %>% 
  
                ## Generate an additional identifier which combines unique patient ID with virus strain.
                ## This will be used later.
                mutate (id = paste0(VIRUS_STRAIN_REPORTED,
                                    SUBJECT_ACCESSION))



hai2neut_result_301 <-  updt_hai_301 %>%            ## First load the previously generated HAI_301 dataset.
                        full_join(., updt_neut_301, ## Merge with antibody level (titers) data derived for virus 
                                  "id") %>% 
                        #' Generate a single column for patient unique IDs replaced 
                        #' with the IDs from HAI if IDs in updt_neut_301 are empty.
                        mutate (SUBJECT_ACCESSION = ifelse(!is.na(SUBJECT_ACCESSION.x),
                                                           SUBJECT_ACCESSION.x,
                                                           SUBJECT_ACCESSION.y),
                                ## Generate a single column for patient unique IDs replaced with the IDs from HAI if IDs in updt_neut_301 are empty.
                                VIRUS_STRAIN_REPORTED = ifelse(!is.na(VIRUS_STRAIN_REPORTED.x),
                                                               VIRUS_STRAIN_REPORTED.x,
                                                               VIRUS_STRAIN_REPORTED.y)) %>% 
                        #' Select only patient uniqueID, antibody titers at
                        #' days 0 & 28 for both HAI and nuetralising virus columns. 
                        select (SUBJECT_ACCESSION,
                                VIRUS_STRAIN_REPORTED,
                                Day_0_hai, Day_28_hai,
                                Day_0_neut_ab, Day_28_neut_ab)




updt_vac_hist_301 <- assessment_component_301 %>%    ## Baseline assesment of patients including vaccination history.
                     select (SUBJECT_ACCESSION,
                             NAME_REPORTED,
                             RESULT_VALUE_REPORTED,
                             PLANNED_VISIT_ACCESSION) %>% 
                     arrange (SUBJECT_ACCESSION) %>%   ## Order by unique ID.
                     #' Reshape data to wide so that each vaccination history 
                     #' takes a column containing the corresponding value (Y/N/UNK).
                     pivot_wider(names_from = NAME_REPORTED,
                                 values_from = RESULT_VALUE_REPORTED) %>% 
                     janitor::clean_names()
  

arm_subj_301 <- arm_2_subject_301 %>%         # Demographic characteristics of patients by arm.
                select (SUBJECT_ACCESSION,
                        MAX_SUBJECT_AGE,
                        SUBJECT_PHENOTYPE) %>% 
                left_join(., subject_301,     # Merge with additional demographic characteristics of patients (e.g. gender).
                          "SUBJECT_ACCESSION") %>% 
                select(-c(WORKSPACE_ID)) %>% 
                ## Use clean names (lowercase, no slash) in column names.
                janitor::clean_names()


# Link participant's demographic characteristics with their vaccination history.
subj_2_vacHist_301 <- arm_subj_301 %>% 
                      left_join(., updt_vac_hist_301,       # Merge with additional demographic characteristics of patients (e.g. gender).
                                "subject_accession")
  

# Link biosample data with visitation data.
updt_biosamp_301 <- biosample_301 %>% 
                    select (BIOSAMPLE_ACCESSION,
                            PLANNED_VISIT_ACCESSION,
                            STUDY_TIME_COLLECTED,
                            STUDY_TIME_T0_EVENT,
                            SUBJECT_ACCESSION,
                            TYPE) %>% 
                    left_join(., planned_visit_301 %>% 
                                  select (-c(ORDER_NUMBER,
                                             STUDY_ACCESSION,
                                             WORKSPACE_ID)),
                              "PLANNED_VISIT_ACCESSION") %>% 
                    janitor::clean_names()


# Link haematology (lab data) with biosamples (including date/period of sample collection).
updt_lab_301 <- lab_test_301 %>% 
            filter (NAME_REPORTED == "lymphocyte count" |    ## We focused only on counts of lymphocyte(L) and 
                      NAME_REPORTED == "monocyte count") %>%  ## as this has L:M shown to effectively distinguish influenza infected people from uninfected. 
            select (BIOSAMPLE_ACCESSION, NAME_REPORTED,
                    RESULT_VALUE_REPORTED) %>%
            arrange (BIOSAMPLE_ACCESSION) %>%
            ungroup () %>% 
  
            ## Force lab results to numeric.
            mutate (RESULT_VALUE_REPORTED = as.numeric(RESULT_VALUE_REPORTED)) %>% 
  
            ## Reshape data to wide so that counts of each labtests are under their specific columns.
            pivot_wider(names_from = NAME_REPORTED,
                        values_from = RESULT_VALUE_REPORTED) %>% 
            janitor::clean_names() %>% 
  
            ## Link lab data with previously updt_biosamp_301 data.
            left_join(.,
                      updt_biosamp_301 %>% select (biosample_accession,
                                                   study_time_collected,
                                                   subject_accession),
                      "biosample_accession") %>% 
  
            #' Move participant ids (subject_accession) and 
            #' study collection period forward.
            relocate(c(subject_accession,
                       study_time_collected),
                     .before = biosample_accession) %>% 
            rename (time_of_enrollment = study_time_collected) %>% 
            #' Derive a L:M rounded to 2d.p.
            mutate (lmr = round(lymphocyte_count/monocyte_count, 2)) %>% 
            #' Classify participants are infected if LMR < 2.
            #' See for reference: https://doi.org/10.1371/currents.rrn1154
            mutate (infect_stat = ifelse(lmr < 2, "Infected", "Uninfected"))

## Link the newly created updt_lab_301 dataset with the cleaned biosample datasets.
lab_biosam_301 <- updt_lab_301 %>% 
                  left_join(., updt_biosamp_301,
                            "biosample_accession") %>% 
                  select (-c(type, max_start_day, min_start_day))

## Keep only the relevant study variables.
gdata::keep(hai2neut_result_301, subj_2_vacHist_301,
            arm_subj_301, updt_vac_hist_301,
            updt_lab_301, updt_biosamp_301, sure = TRUE)

## Save data image to memory. To be called later.
save.image("data/output/sdy301.RData")

