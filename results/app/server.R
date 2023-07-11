
function(input, output) {

  data_1 <- reactive({
            req(input$files_1)
            upload = list()

            for(nr in 1:length(input$files_1[, 1])){
              upload[[nr]] <- read.csv(
                file = input$files_1[[nr, 'datapath']]
              )
            }

            return (upload)

          })


  data_2 <- reactive({
            req(input$files_2)
            upload = list()

            for(nr in 1:length(input$files_2[, 1])){
              upload[[nr]] <- read.csv(
                file = input$files_2[[nr, 'datapath']]
              )
            }

            return (upload)

          })

  # return(linked_dta_1)

  linked_dta_1 <- list()
  linked_dta_2 <- list()
  updtDTa <- eventReactive(input$process_dta, {

    if (sum(c("hai_result.csv",
              "arm_2_subject.csv",
              "subject.csv", "biosample.csv",
              "lab_test.csv",
              "immune_exposure.csv") %in% input$files_1[,1]) == 6 &
        sum(c("hai_result.csv",
              "arm_2_subject.csv",
              "subject.csv", "biosample.csv",
              "lab_test.csv",
              "immune_exposure.csv") %in% input$files_2[,1]) == 6 &
        !is.null(input$files_1) &
        !is.null(input$files_2)){

        ### removeModal() run data merge processes here.]
      show_spinner()


      #-----------------------------
      # Process the first datasets here
      #-----------------------------

      # linked_dta_1 <- list()
        linked_dta_1[["neut_ab_titer_result"]] <-
              data_1()[[which(input$files_1[,1] == "neut_ab_titer_result.csv")]] %>%
              arrange (SUBJECT_ACCESSION,
                       BIOSAMPLE_ACCESSION,
                       EXPERIMENT_ACCESSION,
                       VIRUS_STRAIN_REPORTED) %>%
              group_by(SUBJECT_ACCESSION,
                       BIOSAMPLE_ACCESSION,
                       EXPERIMENT_ACCESSION,
                       VIRUS_STRAIN_REPORTED) %>%
              mutate (n = 1:n()) %>%
              filter (n == 1) %>% ungroup () %>%
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


        linked_dta_1[["hai_result"]] <-
          data_1()[[which(input$files_1[,1] == "hai_result.csv")]] %>%
          arrange (SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          group_by(SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          mutate (n = 1:n()) %>%
          filter (n == 1) %>% ungroup () %>%
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


        linked_dta_1[["hai2neut_result"]] <-
          linked_dta_1[["hai_result"]] %>%
          full_join(., linked_dta_1[["neut_ab_titer_result"]],
                    "id") %>%
          mutate (SUBJECT_ACCESSION = ifelse(!is.na(SUBJECT_ACCESSION.x),
                                             SUBJECT_ACCESSION.x,
                                             SUBJECT_ACCESSION.y),
                  VIRUS_STRAIN_REPORTED = ifelse(!is.na(VIRUS_STRAIN_REPORTED.x),
                                                 VIRUS_STRAIN_REPORTED.x,
                                                 VIRUS_STRAIN_REPORTED.y)) %>%
          select (SUBJECT_ACCESSION,
                  VIRUS_STRAIN_REPORTED,
                  Day_0_hai, Day_28_hai,
                  Day_0_neut_ab, Day_28_neut_ab)



        linked_dta_1[["updt_vac_hist"]] <-
          data_1()[[which(input$files_1[,1] == "assessment_component.csv")]] %>%
          select (SUBJECT_ACCESSION,
                  NAME_REPORTED,
                  RESULT_VALUE_REPORTED,
                  PLANNED_VISIT_ACCESSION) %>%
          arrange (SUBJECT_ACCESSION) %>%
          pivot_wider(names_from = NAME_REPORTED,
                      values_from = RESULT_VALUE_REPORTED) %>%
          janitor::clean_names()

        linked_dta_1[["arm_subj"]] <-
          data_1()[[which(input$files_1[,1] == "arm_2_subject.csv")]] %>%
          select (SUBJECT_ACCESSION,
                  MAX_SUBJECT_AGE,
                  SUBJECT_PHENOTYPE) %>%
          left_join(., data_1()[[which(input$files_1[,1] == "subject.csv")]],
                    "SUBJECT_ACCESSION") %>%
          select(-c(WORKSPACE_ID)) %>%
          janitor::clean_names()


        linked_dta_1[["subj_2_vacHist"]] <-
          linked_dta_1[["arm_subj"]] %>%
          left_join(., linked_dta_1[["updt_vac_hist"]],
                    "subject_accession")


        linked_dta_1[["updt_biosamp"]] <-
          data_1()[[which(input$files_1[,1] == "biosample.csv")]] %>%
          select (BIOSAMPLE_ACCESSION,
                  PLANNED_VISIT_ACCESSION,
                  STUDY_TIME_COLLECTED,
                  STUDY_TIME_T0_EVENT,
                  SUBJECT_ACCESSION,
                  TYPE) %>%
          left_join(., data_1()[[which(input$files_1[,1] == "planned_visit.csv")]] %>%
                      select (-c(ORDER_NUMBER,
                                 STUDY_ACCESSION,
                                 WORKSPACE_ID)),
                    "PLANNED_VISIT_ACCESSION") %>%
          janitor::clean_names()


        linked_dta_1[["updt_lab"]] <-
          data_1()[[which(input$files_1[,1] == "lab_test.csv")]] %>%
          filter (NAME_REPORTED == "lymphocyte count" |
                    NAME_REPORTED == "monocyte count") %>%
          select (BIOSAMPLE_ACCESSION, NAME_REPORTED,
                  RESULT_VALUE_REPORTED) %>%
          arrange (BIOSAMPLE_ACCESSION) %>%
          ungroup () %>%
          mutate (RESULT_VALUE_REPORTED = as.numeric(RESULT_VALUE_REPORTED)) %>%
          pivot_wider(names_from = NAME_REPORTED,
                      values_from = RESULT_VALUE_REPORTED) %>%
          janitor::clean_names() %>%
          left_join(.,
                    linked_dta_1[["updt_biosamp"]] %>% select (biosample_accession,
                                                 study_time_collected,
                                                 subject_accession),
                    "biosample_accession") %>%
          relocate(c(subject_accession,
                     study_time_collected),
                   .before = biosample_accession) %>%
          rename (time_of_enrollment = study_time_collected) %>%
          mutate (lmr = round(lymphocyte_count/monocyte_count, 2)) %>%
          mutate (infect_stat = ifelse(lmr < 2, "Infected", "Uninfected"))



        linked_dta_1[["lab_biosam"]] <- linked_dta_1[["updt_lab"]] %>%
          left_join(., linked_dta_1[["updt_biosamp"]],
                    "biosample_accession") %>%
          select (-c(type, max_start_day, min_start_day))

        #-----------------------------
        # Repeat the process the first datasets here
        #-----------------------------

        # linked_dta_1 <- list()
        linked_dta_2[["neut_ab_titer_result"]] <-
          data_2()[[which(input$files_2[,1] == "neut_ab_titer_result.csv")]] %>%
          arrange (SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          group_by(SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          mutate (n = 1:n()) %>%
          filter (n == 1) %>% ungroup () %>%
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


        linked_dta_2[["hai_result"]] <-
          data_2()[[which(input$files_2[,1] == "hai_result.csv")]] %>%
          arrange (SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          group_by(SUBJECT_ACCESSION,
                   BIOSAMPLE_ACCESSION,
                   EXPERIMENT_ACCESSION,
                   VIRUS_STRAIN_REPORTED) %>%
          mutate (n = 1:n()) %>%
          filter (n == 1) %>% ungroup () %>%
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


        linked_dta_2[["hai2neut_result"]] <-
          linked_dta_2[["hai_result"]] %>%
          full_join(., linked_dta_2[["neut_ab_titer_result"]],
                    "id") %>%
          mutate (SUBJECT_ACCESSION = ifelse(!is.na(SUBJECT_ACCESSION.x),
                                             SUBJECT_ACCESSION.x,
                                             SUBJECT_ACCESSION.y),
                  VIRUS_STRAIN_REPORTED = ifelse(!is.na(VIRUS_STRAIN_REPORTED.x),
                                                 VIRUS_STRAIN_REPORTED.x,
                                                 VIRUS_STRAIN_REPORTED.y)) %>%
          select (SUBJECT_ACCESSION,
                  VIRUS_STRAIN_REPORTED,
                  Day_0_hai, Day_28_hai,
                  Day_0_neut_ab, Day_28_neut_ab)



        linked_dta_2[["updt_vac_hist"]] <-
          data_2()[[which(input$files_2[,1] == "assessment_component.csv")]] %>%
          select (SUBJECT_ACCESSION,
                  NAME_REPORTED,
                  RESULT_VALUE_REPORTED,
                  PLANNED_VISIT_ACCESSION) %>%
          arrange (SUBJECT_ACCESSION) %>%
          pivot_wider(names_from = NAME_REPORTED,
                      values_from = RESULT_VALUE_REPORTED) %>%
          janitor::clean_names()

        linked_dta_2[["arm_subj"]] <-
          data_2()[[which(input$files_2[,1] == "arm_2_subject.csv")]] %>%
          select (SUBJECT_ACCESSION,
                  MAX_SUBJECT_AGE,
                  SUBJECT_PHENOTYPE) %>%
          left_join(., data_2()[[which(input$files_2[,1] == "subject.csv")]],
                    "SUBJECT_ACCESSION") %>%
          select(-c(WORKSPACE_ID)) %>%
          janitor::clean_names()


        linked_dta_2[["subj_2_vacHist"]] <-
          linked_dta_2[["arm_subj"]] %>%
          left_join(., linked_dta_2[["updt_vac_hist"]],
                    "subject_accession")


        linked_dta_2[["updt_biosamp"]] <-
          data_2()[[which(input$files_2[,1] == "biosample.csv")]] %>%
          select (BIOSAMPLE_ACCESSION,
                  PLANNED_VISIT_ACCESSION,
                  STUDY_TIME_COLLECTED,
                  STUDY_TIME_T0_EVENT,
                  SUBJECT_ACCESSION,
                  TYPE) %>%
          left_join(., data_2()[[which(input$files_2[,1] == "planned_visit.csv")]] %>%
                      select (-c(ORDER_NUMBER,
                                 STUDY_ACCESSION,
                                 WORKSPACE_ID)),
                    "PLANNED_VISIT_ACCESSION") %>%
          janitor::clean_names()


        linked_dta_2[["updt_lab"]] <-
          data_2()[[which(input$files_2[,1] == "lab_test.csv")]] %>%
          filter (NAME_REPORTED == "lymphocyte count" |
                    NAME_REPORTED == "monocyte count") %>%
          select (BIOSAMPLE_ACCESSION, NAME_REPORTED,
                  RESULT_VALUE_REPORTED) %>%
          arrange (BIOSAMPLE_ACCESSION) %>%
          ungroup () %>%
          mutate (RESULT_VALUE_REPORTED = as.numeric(RESULT_VALUE_REPORTED)) %>%
          pivot_wider(names_from = NAME_REPORTED,
                      values_from = RESULT_VALUE_REPORTED) %>%
          janitor::clean_names() %>%
          left_join(.,
                    linked_dta_2[["updt_biosamp"]] %>% select (biosample_accession,
                                                               study_time_collected,
                                                               subject_accession),
                    "biosample_accession") %>%
          relocate(c(subject_accession,
                     study_time_collected),
                   .before = biosample_accession) %>%
          rename (time_of_enrollment = study_time_collected) %>%
          mutate (lmr = round(lymphocyte_count/monocyte_count, 2)) %>%
          mutate (infect_stat = ifelse(lmr < 2, "Infected", "Uninfected"))



        linked_dta_2[["lab_biosam"]] <- linked_dta_2[["updt_lab"]] %>%
          left_join(., linked_dta_2[["updt_biosamp"]],
                    "biosample_accession") %>%
          select (-c(type, max_start_day, min_start_day))


        linked <- list(linked_dta_1, linked_dta_2)
        return(linked)

      hide_spinner()

        } else {
          showModal (
            modalDialog(
              title = "We encountered an Error!",
              easyClose = TRUE,
              footer = NULL,

              ifelse(!is.null(input$files_1) &
                       !is.null(input$files_2),
                     "The required data objects have not been uploaded.",
                     "Invalid or empty data object."
                     ),

              # div(tags$b(paste0("Emman", print(input$files[,1])))),
              tags$p(

                ifelse(!is.null(input$files) &
                         !is.null(input$files_2),
                      ("Please upload the required data objects for both study 1 and study:

                       hai_result.csv, arm_2_subject.csv, subject.csv,
                       biosample.csv, lab_test.csv, immune_exposure.csv"),

                       "Please upload your .csv files below to continue."
                ))

            )
          )
        }
  })




  #-----------------------------
  # Clean Data sets
  #-----------------------------
  lnk_arm_sub <- reactive({

    rbind (updtDTa()[[1]][["arm_subj"]] %>% mutate (link = "SDY296"),
           updtDTa()[[2]][["arm_subj"]] %>% mutate (link = "SDY301"))
  })

  lnk_hi2neut <- reactive({

    rbind (updtDTa()[[1]][["hai2neut_result"]] %>% mutate (link = "SDY296"),
           updtDTa()[[2]][["hai2neut_result"]] %>% mutate (link = "SDY301")) %>%
      mutate (VIRUS_STRAIN_REPORTED = str_remove(VIRUS_STRAIN_REPORTED,
                                                 " H1N1| H3N2"),
              VIRUS_STRAIN_REPORTED = replace (VIRUS_STRAIN_REPORTED,
                                               which(VIRUS_STRAIN_REPORTED == "A/California/7/2009"),
                                               "A/California/07/2009"))
  })


  lnk_surrogate <-  reactive({

    lnk_hi2neut() %>%
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
  })




  lnk_antiBdy_lvl <- reactive({

    lnk_surrogate() %>%
      select(subject_accession, link,
             virus_strain_reported,
             day_28_hai, day_28_neut_ab,
             hai_seroconversion,
             neut_seroconversion) %>%
      pivot_longer(cols = c(day_28_hai:neut_seroconversion),
                   values_to = "values",
                   names_to = "names") %>%
      mutate (antibody = str_extract(names, "hai|neut"),
              measure = ifelse(str_detect(names, "day_28"),
                               "titer", "fold")) %>%
      select (-c(names)) %>%
      pivot_wider(names_from = c(virus_strain_reported, measure, antibody),
                  values_from = values) %>%
      clean_names()
  })


  lnk_lab <- reactive({

    rbind (updtDTa()[[1]][["updt_lab"]] %>% mutate (link = "SDY296"),
           updtDTa()[[2]][["updt_lab"]] %>% mutate (link = "SDY301")) %>%
      mutate (time_of_enrollment = replace(time_of_enrollment,
                                           which (time_of_enrollment == 4),
                                           3),
              time_of_enrollment = replace(time_of_enrollment,
                                           which (time_of_enrollment == 8),
                                           7)) %>%
      filter (time_of_enrollment > 0 & time_of_enrollment <= 240) %>%
      filter (!is.na(infect_stat)) %>%
      group_by (subject_accession, link) %>% arrange() %>%
      summarise (infect_status = ifelse(any(infect_stat == "Infected"),
                                        "Infected", "Uninfected")) %>%
      janitor::clean_names()

  })


  lnk_corr_surr <- reactive({

    lnk_lab() %>%
      left_join(., lnk_antiBdy_lvl(),
                c("subject_accession", "link"))

  })

  lhk_surr <- reactive({

  lnk_hi2neut() %>%
    pivot_longer(cols = Day_0_hai:Day_28_neut_ab,
                 values_to = "value",
                 names_to = "days") %>%
    mutate (titer = ifelse(str_detect(days, "hai"),
                           "HAI", "NEUT")) %>%
    mutate (days = str_remove(days, "_hai|_neut_ab")) %>%
    mutate (days = paste0(ifelse((link == "SDY296"),
                                 "Year 2: ",
                                 "Year 3: "),
                          str_replace(days, "_", " ")))

  })



  gmean_dta <- reactive({

    new_data <- data.frame()

    for (virus in unique(lhk_surr()$VIRUS_STRAIN_REPORTED)) {

      for (day in unique(lhk_surr()$days)) {

        for (tites in unique(lhk_surr()$titer)) {

          gmean_data <- lhk_surr() %>% filter (days == day &
                                         VIRUS_STRAIN_REPORTED == virus &
                                         titer == tites)

          data <- DescTools::Gmean(gmean_data$value,
                                   na.rm=TRUE,
                                   conf.level = 0.95) %>% data.frame()

          f_data <- cbind(t(data), data.frame(titer = tites,
                                              days = day,
                                              virus = virus)) %>% data.frame()

          new_data <- rbind(new_data, f_data)
        }
      }

    }

    return(new_data)

})


  ###

  output$dTTplotInput <- renderPlot({
    input$update_plot


    isolate({

    if (input$viz_type == "anti_titers") {

      lhk_surr() %>%
        filter (VIRUS_STRAIN_REPORTED== input$select_virus)  %>%
        group_by(SUBJECT_ACCESSION) %>% mutate (n = n()) %>%
        ungroup () %>% filter (!is.na(value)) %>%
        ggplot(aes(days,
                   reorder(SUBJECT_ACCESSION, -n),
                   fill=value)) +

        geom_tile(aes(fill=value),color="white") +
        facet_wrap(~ titer) +
        theme_bw(base_family = "barlow",
                 base_size = 15) + theme (legend.position = "bottom",
                                                  legend.key.width = unit(2.0,"cm"),
                                                  strip.background = element_rect(fill = "white"),
                                                  strip.text = element_text(size = 14),
                                                  legend.title = element_text(family = "special"),
                                                  legend.text = element_text(family = "special"),
                                                  title = element_text(size = 14),
                                                  plot.title = element_text(size = 20, family = "special",
                                                                            lineheight = 1.5)) +
        labs (fill = "Antibody Titer",
              x = "Study Timeline", y = "",
              title = paste0("Levels of antibody titers against ", input$select_virus,
                              "\n virus strain at baseline and day 28 \npost-vaccination across the study periods")) +
        scale_fill_continuous_sequential (palette = "Blues",
                                          guide = guide_colorbar(direction = "horizontal",
                                                                 title.position = "top",
                                                                 title.hjust = 0.5,
                                                                 keywidth = unit(15.0,"cm")))
    } else if (input$viz_type == "seroconv_prev") {

      ## Prevalence of Sero Conversion
      rbind(
        lnk_surrogate() %>%
          select (virus_strain_reported, link, hai_sero_stat) %>%
          group_by (virus_strain_reported, link, hai_sero_stat) %>%
          count () %>% ungroup () %>% group_by(virus_strain_reported, link) %>%
          mutate (prop_sero = round(n/sum(n), 3) * 100) %>% filter(hai_sero_stat == "Seroconverted") %>%
          mutate (vial_link = paste0 (link, ": ", virus_strain_reported)) %>%
          rename (sero_stat = hai_sero_stat) %>%
          mutate (antibody = "HAI"),

        lnk_surrogate() %>%
          select (virus_strain_reported, link, neut_sero_stat) %>%
          group_by (virus_strain_reported, link, neut_sero_stat) %>%
          count () %>% ungroup () %>% group_by(virus_strain_reported, link) %>%
          mutate (prop_sero = round(n/sum(n), 3) * 100) %>% filter(neut_sero_stat == "Seroconverted") %>%
          mutate (vial_link = paste0 (link, ": ", virus_strain_reported)) %>%
          rename (sero_stat = neut_sero_stat) %>%
          mutate (antibody = "NEUT")) %>%
        mutate (city = str_extract(virus_strain_reported, "California|Perth|Victoria|Wisconsin|Brisbane")) %>%
        ggplot () +
        geom_col(aes (x = prop_sero,
                      y = reorder (vial_link, prop_sero),
                      fill = city)) +
        facet_wrap(~ antibody) +
        scale_fill_manual(values = c("#003f5c","#554d88","#b24b88","#ef5d5a","#ec9a00")) +
        labs (y = "", x = "Prevalence (%)",
              title =  "Prevalence of Antibody Seroconversion \n(4-fold titer increase) in Adults at 28 days") +
        theme_bw(base_family = "barlow",
                 base_size = 15) + theme (legend.position = "none",
                                          legend.key.width = unit(2.0,"cm"),
                                          panel.grid = element_line(linewidth = 0.1),
                                          legend.title = element_text(family = "special"),
                                          legend.text = element_text(family = "special"),
                                          strip.background = element_rect(fill = "white"),
                                          plot.title = element_text(size = 20, family = "special",
                                                                    lineheight = 1.5))




    } else if (input$viz_type == "gm_antiters") {

      gmean_dta() %>% filter(mean != "NaN") %>%
        filter (virus == input$select_virus) %>%
        remove_rownames() %>%
        arrange(days, virus) %>%
        ggplot(aes(x = days,
                   y = mean)) +
        geom_point() +
        geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci),
                      width = 0.1, size = 0.6) +
        facet_grid(~ titer) + labs (y = "Geometric mean antibody-titers ",
                                          title = paste0("Geometric Mean of Antibody Titers against ", input$select_virus,
                                                  "virus strain \nincorporated in vaccines for Healthy Adults \nby Study Period and Antibody")) +
        theme_bw(base_family = "barlow",
                 base_size = 15) + theme (legend.position = "bottom",
                                          legend.key.width = unit(2.0,"cm"),
                                          panel.grid = element_line(linewidth = 0.1),
                                          legend.title = element_text(family = "special"),
                                          legend.text = element_text(family = "special"),
                                          strip.background = element_rect(fill = "white"),
                                          plot.title = element_text(size = 20, family = "special",
                                                                    lineheight = 1.0)) +
        geom_hline(data = data.frame(yintercept = 40,titer="HAI"),
                   aes(yintercept = yintercept), linetype = "dotted")
    } else {

      #### Prevalence of Infection
      lnk_lab() %>% group_by(link, infect_status) %>%
        count () %>% group_by (link) %>%
        mutate (prop = round(n/(sum(n)), 3)) %>%
        ggplot(aes(infect_status,
                   prop)) +
        geom_col() + facet_wrap(~link) +
        scale_fill_manual("black") +
        scale_y_continuous(labels = scales::percent) +
        theme_bw(base_family = "barlow",
                 base_size = 15) + theme (legend.position = "bottom",
                                          legend.key.width = unit(2.0,"cm"),
                                          strip.background = element_rect(fill = "white"),
                                          legend.title = element_text(family = "special"),
                                          legend.text = element_text(family = "special"),
                                          plot.title = element_text(size = 20, family = "special",
                                                                    lineheight = 1.0)) +
        labs (y = "", x = "",
              title = "Prevalence of Influenza infection (LMR < 2)")
    }
    })
  })


  output$progressBox2 <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple", fill = TRUE
    )
  })
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow", fill = TRUE
    )
  })


  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })

    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = msgs)
  })


  ml = c("Logistic Regression", "k-Nearest Neighbours", "randomForest")
  cs = "Logistic Regression"
  data_ml <- data.frame(ml, cs)
  # # Render the select input dynamically
  output$ml_choice <- renderUI({
    selectInput("select_input", "Select a dataset:", choices = unique(data_ml[input$model][,1]))
  })


    # # Render the select input dynamically
    output$select <- renderUI({
      selectInput("select_input", "Select a dataset:", choices = input$files_1[,1])
    })




  output$dttt <-
    renderUI({

      print(input$data_id_1)
    })


  # Downloadable csv of selected dataset ----
  output$download_dta <- downloadHandler(
    filename = function() {
      paste(input$files_1[1,1], ".csv", sep = "")
    },
    content = function(file) {
      write.csv(updtDTa()[[1]][[shuffle(1:5)[1]]], file, row.names = FALSE)
    },
    contentType = "text/csv"
  )





    # Update the choices based on some event or condition
    observeEvent(input$select_input, {

      output$contents <- renderDT({
          datatable(data_1()[[which(input$files_1[,1] == input$select_input)]],
                    options=list(scrollX=T))
    })
    })



    # Update the choices based on some event or condition

    model_data <- reactive({
        req(input$files)

        if (input$select_assay == 'Quantitate Serum Antibody'){

          model_data <- list()



          model_data[["arm"]] <- gt()[[which(input$files[,1] == "arm_2_subject.csv")]] %>%
                                left_join(., gt()[[which(input$files[,1] == "arm_or_cohort.csv")]],
                                          "ARM_ACCESSION") %>%
                                left_join(., gt()[[which(input$files[,1] == "subject.csv")]],
                                          "SUBJECT_ACCESSION") %>%
                                left_join(., gt()[[which(input$files[,1] == "immune_exposure.csv")]],
                                          "SUBJECT_ACCESSION")

          model_data[["block_2"]] <-  gt()[[which(input$files[,1] == "arm_2_subject.csv")]] %>%
                              left_join(., gt()[[which(input$files[,1] == "arm_or_cohort.csv")]],
                                        "ARM_ACCESSION") %>%
                              left_join(., gt()[[which(input$files[,1] == "subject.csv")]],
                                        "SUBJECT_ACCESSION") %>%
                              left_join(., gt()[[which(input$files[,1] == "biosample.csv")]],
                                        "SUBJECT_ACCESSION") %>%
                              left_join(., gt()[[which(input$files[,1] == "lab_test.csv")]],
                                        "BIOSAMPLE_ACCESSION") %>%
                              select(!contains("WORKSPACE_ID"))
        } else {

          model_data <- list()

        }

      return(model_data)

      })





    observeEvent(input$select_assay, {



      output$descr_table <-
        renderUI({


          rbind(
            model_data()[["arm"]] %>%
              select (MAX_SUBJECT_AGE, TYPE_REPORTED) %>%
              group_by(TYPE_REPORTED) %>%
              summarise(median = median(MAX_SUBJECT_AGE),
                        sd = sd(MAX_SUBJECT_AGE)) %>%
              add_column(characteristics = "Age",
                         .before = 1) %>%
              mutate (perc_n = paste0(median, " (", round(sd,2), ")")) %>%
              select (characteristics, TYPE_REPORTED, perc_n),

            rbind (model_data()[["arm"]] %>%
                     select (GENDER, TYPE_REPORTED) %>%
                     group_by(GENDER, TYPE_REPORTED) %>%
                     count() %>% ungroup () %>%
                     group_by (TYPE_REPORTED) %>%
                     mutate (percent = round(n/sum(n),3) * 100) %>%
                     arrange (TYPE_REPORTED) %>%
                     rename (characteristics = GENDER),

                   model_data()[["arm"]] %>%
                     select (RACE, TYPE_REPORTED) %>%
                     group_by(RACE, TYPE_REPORTED) %>%
                     count() %>% ungroup () %>%
                     group_by (TYPE_REPORTED) %>%
                     mutate (percent = round(n/sum(n),3) * 100) %>%
                     arrange (TYPE_REPORTED) %>%
                     rename (characteristics = RACE),

                   model_data()[["arm"]] %>%
                     select (ETHNICITY, TYPE_REPORTED) %>%
                     group_by(ETHNICITY, TYPE_REPORTED) %>%
                     count() %>% ungroup () %>%
                     group_by (TYPE_REPORTED) %>%
                     mutate (percent = round(n/sum(n),3) * 100) %>%
                     arrange (TYPE_REPORTED) %>%
                     rename (characteristics = ETHNICITY),

                   model_data()[["arm"]] %>%
                     select (EXPOSURE_PROCESS_REPORTED, TYPE_REPORTED) %>%
                     group_by(EXPOSURE_PROCESS_REPORTED, TYPE_REPORTED) %>%
                     count() %>% ungroup () %>%
                     group_by (TYPE_REPORTED) %>%
                     mutate (percent = round(n/sum(n),3) * 100) %>%
                     arrange (TYPE_REPORTED) %>%
                     rename (characteristics = EXPOSURE_PROCESS_REPORTED)) %>%
              mutate (perc_n = paste0(n, " (", percent, "%)")) %>%
              select (characteristics, TYPE_REPORTED, perc_n)) %>%
            pivot_wider(., values_from = perc_n,
                        names_from = TYPE_REPORTED) %>%
            mutate (characteristics = str_to_title(characteristics)) %>%
            kable() %>%
            kable_styling("striped", full_width = F) %>%
            add_header_above(c(" ", "Study Arm" = 2)) %>%
            # group_rows("Gender", 2, 3) %>%
            # group_rows("Race", 4, 6) %>%
            # group_rows("Ethnicity", 7, 8) %>%
            # group_rows("Exposure Process", 9, 10) %>%
            HTML()

      })

          ## Create Visuals
          output$dtt_plot <- renderPlot({

          model_data()[["block_2"]] %>%
            filter (STUDY_TIME_T0_EVENT == "Time of initial vaccine administration") %>%
            filter (!is.na(RESULT_VALUE_PREFERRED)) %>%
            mutate (vaccine = case_when(str_detect(DESCRIPTION.x, "Fluzone") ~ "Fluzone",
                                        str_detect(DESCRIPTION.x, "Pneunomax23") ~ "Pneunomax23",
                                        str_detect(DESCRIPTION.x, "Saline") ~ "Saline")) %>%
            filter (NAME_REPORTED != "Basophils" &
                      NAME_REPORTED != "Eosinophils") %>%
            mutate (STUDY_TIME_COLLECTED = as.factor(STUDY_TIME_COLLECTED)) %>%
            ggplot (aes(y = RESULT_VALUE_REPORTED,
                        x = STUDY_TIME_COLLECTED)) +
            geom_boxplot(aes(fill = vaccine), color="red", alpha=0.2) +
            facet_grid(NAME_REPORTED ~ vaccine, scales = "free") +
            theme_minimal() + theme (legend.position = "bottom")
          })


    })


    model_results <- reactive({

      model_data <- lnk_corr_surr() %>%
        select (subject_accession,
                link, infect_status,
                contains("neut")) %>%
        filter (link == "SDY296") %>%
        select (subject_accession,
                link, infect_status,
                contains(c("california", "perth",
                           "brisbane"))) %>%
        filter (!is.na(b_brisbane_60_2008_fold_neut)) %>%
        ungroup ()

      fx <- glm(as.factor(infect_status) ~ .,
                model_data %>% dplyr::select (-c(link, subject_accession)),
                family = binomial("logit"))

      coefficients <- coef(fx) %>% data.frame() %>%
        rownames_to_column()

      colnames(coefficients) <- c("Variables", "Coefficients")


      confint <- confint(fx) %>% data.frame() %>%
        clean_names() %>% rownames_to_column("Variables")

      results <- coefficients %>%
        left_join(., confint,
                  "Variables") %>%
        mutate (Coefficients = round(Coefficients, 3),
                x2_5 = round(x2_5, 3),
                x97_5 = round(x97_5, 3))

      return(results)
    })



    output$modelOutput <- renderPlot({
      # input$run_model
      req(input$run_model)
      isolate({

        ## Prevalence of Sero Conversion

        model_results() %>% filter (Variables != "(Intercept)") %>%
          ggplot(aes(x = Coefficients,
                     y = Variables)) +
          geom_point() +
          geom_errorbar(aes(xmin = x2_5, xmax = x97_5),
                        width = 0.1, size = 0.6) +
          scale_x_continuous(limits = c(-0.35,0.35)) +
          labs (y = "Virus Strain",
                title = "Correlates of Influenza Infection") +
          theme_bw(base_family = "barlow",
                   base_size = 15) + theme (legend.position = "bottom",
                                            legend.key.width = unit(2.0,"cm"),
                                            panel.grid = element_line(linewidth = 0.1),
                                            legend.title = element_text(family = "special"),
                                            legend.text = element_text(family = "special"),
                                            strip.background = element_rect(fill = "white"),
                                            title = element_text(size = 20,
                                                                 lineheight = 0.5)) +
          geom_vline(xintercept = 0, linetype = "dotted")


      })
    })



}



