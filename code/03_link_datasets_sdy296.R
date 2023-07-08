#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Link Study Datasets | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################

updt_neut <- neut_ab_titer_result %>% 
             select(SUBJECT_ACCESSION,
                    STUDY_TIME_COLLECTED,
                    VALUE_REPORTED,
                    VIRUS_STRAIN_REPORTED) %>% 
              mutate (STUDY_TIME_COLLECTED = paste0("Day_", STUDY_TIME_COLLECTED,
                                                   "_neut_ab")) %>% 
              arrange (SUBJECT_ACCESSION, STUDY_TIME_COLLECTED) %>% 
              pivot_wider(names_from = STUDY_TIME_COLLECTED,
                          values_from = VALUE_REPORTED) %>% 
              mutate (id = paste0(VIRUS_STRAIN_REPORTED,
                                  SUBJECT_ACCESSION))


updt_hai <- hai_result %>% 
            select(SUBJECT_ACCESSION,
                   STUDY_TIME_COLLECTED,
                   VALUE_REPORTED,
                   VIRUS_STRAIN_REPORTED) %>% 
            mutate (STUDY_TIME_COLLECTED = paste0("Day_", STUDY_TIME_COLLECTED,
                                                 "_hai")) %>% 
            arrange (SUBJECT_ACCESSION, STUDY_TIME_COLLECTED) %>% 
            pivot_wider(names_from = STUDY_TIME_COLLECTED,
                        values_from = VALUE_REPORTED) %>% 
            mutate (id = paste0(VIRUS_STRAIN_REPORTED,
                                SUBJECT_ACCESSION))



hai2neut_result <- updt_hai %>% 
                    left_join(., updt_neut,
                              "id") %>% 
                    select (SUBJECT_ACCESSION = "SUBJECT_ACCESSION.x",
                            VIRUS_STRAIN_REPORTED = "VIRUS_STRAIN_REPORTED.x",
                            Day_0_hai, Day_28_hai,
                            Day_0_neut_ab, Day_28_neut_ab)

updt_lab <- lab_test %>% 
  mutate (unit = case_when(RESULT_UNIT_REPORTED == "%" ~ "perc",
                           RESULT_UNIT_REPORTED == "10^3" ~ "10_3",
                           RESULT_UNIT_REPORTED == "10^3/mm3" ~ "10_3_p_mm3",
                           RESULT_UNIT_REPORTED == "10^6" ~ "10_6",
                           
                           RESULT_UNIT_REPORTED == "fl" ~ "fl",
                           RESULT_UNIT_REPORTED == "g/dl" ~ "g_p_dl",
                           RESULT_UNIT_REPORTED == "pg" ~ "pg")) %>% 
  mutate (NAME_REPORTED = str_remove(NAME_REPORTED,
                                     " percentage| count| Volume")) %>% 
  mutate (NAME_REPORTED = paste0(NAME_REPORTED, "_", unit)) %>% 
  select (BIOSAMPLE_ACCESSION, NAME_REPORTED,
          RESULT_VALUE_REPORTED) %>% 
  arrange (BIOSAMPLE_ACCESSION) %>% 
  pivot_wider(names_from = NAME_REPORTED,
              values_from = RESULT_VALUE_REPORTED) %>% 
  janitor::clean_names()



updt_vac_hist <- assessment_component %>% 
                 select (SUBJECT_ACCESSION,
                         NAME_REPORTED,
                         RESULT_VALUE_REPORTED,
                         PLANNED_VISIT_ACCESSION) %>% 
                 arrange (SUBJECT_ACCESSION) %>% 
                 pivot_wider(names_from = NAME_REPORTED,
                             values_from = RESULT_VALUE_REPORTED) %>% 
                 janitor::clean_names()
  

arm_subj <- arm_2_subject %>% 
            select (SUBJECT_ACCESSION,
                    MAX_SUBJECT_AGE,
                    SUBJECT_PHENOTYPE) %>% 
            left_join(., subject,
                      "SUBJECT_ACCESSION") %>% 
            select(-c(WORKSPACE_ID)) %>% 
            janitor::clean_names()


subj_2_vacHist <- arm_subj %>% 
                  left_join(., updt_vac_hist,
                            "subject_accession")
  


updt_biosamp <- biosample %>% 
                select (BIOSAMPLE_ACCESSION,
                        PLANNED_VISIT_ACCESSION,
                        STUDY_TIME_COLLECTED,
                        STUDY_TIME_T0_EVENT,
                        SUBJECT_ACCESSION,
                        TYPE) %>% 
                left_join(., planned_visit %>% 
                              select (-c(ORDER_NUMBER,
                                         STUDY_ACCESSION,
                                         WORKSPACE_ID)),
                          "PLANNED_VISIT_ACCESSION")



gdata::keep(hai2neut_result, subj_2_vacHist,
            arm_subj, updt_vac_hist,
            updt_lab, updt_biosamp, sure = TRUE)

save.image("data/output/sdy296.RData")

