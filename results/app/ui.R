source("packages.R")



showtext_auto()
font_add_google("Barlow Condensed","barlow")
font_add_google("Special Elite", family = "special")

library(shiny)
library(shinythemes)

ui <-
  navbarPage("DiCAiDs", collapsible = TRUE, inverse = TRUE, theme = shinytheme("united"),
             tabPanel("Browse Data",
                      fluidPage(
                        use_busy_spinner(spin = "fading-circle"),
                        sidebarLayout(
                          sidebarPanel(width = 4,

                                        fileInput(
                                          inputId = "files_1",
                                          label = "Upload your study dataset(s) to continue (.csv file(s) only)",
                                          multiple = TRUE,
                                          placeholder = "No file selected",
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")
                                        ),

                                       textInput(inputId = "data_id_1",
                                                 label = "How would you like to identify this dataset?"),

                                       uiOutput("select"),


                                       hr(),

                                       ## add button to trigger filtering the data  -----------------------
                                       checkboxInput(inputId = "upload_1",
                                                       label = "Upload Another Study Datasets"),

                                       conditionalPanel(
                                         condition = "input.upload_1 == true",
                                         status = "primary",


                                         fileInput(
                                           inputId = "files_2",
                                           label = "Upload an additional study dataset",
                                           multiple = TRUE,
                                           placeholder = "No file selected",
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")
                                         ),

                                         textInput(inputId = "data_id_2",
                                                   label = "How would you like to identify this dataset?"),


                                         ),

                                       hr(),

                                        actionButton(inputId = "process_dta", width = 225,
                                                     label = "Process Study Dataset(s)"),

                                       br(),  br(),

                                       downloadButton(outputId = "download_dta", width = 225,
                                                     label = "Download Processed Dataset(s)")

                                       ),

                          mainPanel(withSpinner(DTOutput("contents")))


                      ))),
             tabPanel("Visualise",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       radioButtons("viz_type", "Visualisation:",
                                                    c("Prevalence of Infection" = "infect_prev",
                                                      "Prevalence of Antibody Seroconversion" = "seroconv_prev",
                                                      "Antibody Level patters" = "anti_titers",
                                                      "Mean Antibody Titers" = "gm_antiters")),
                                       hr(),

                                       selectInput("select_assay",
                                                   "Choose an assay result to explore:",
                                                   choices = c('Hemagglutination Inhibiting Antibody',
                                                               'Neutralizing Antibody Titer',
                                                               'Enzyme Linked ImmunoSorbant',
                                                               'Enzyme linked ImmunoSPOT',
                                                               'Flow Cytometry',
                                                               'Mass Cytometry',
                                                               'Gene Expresssion',
                                                               'Genotyping',
                                                               'Quantitate Serum Antibody',
                                                               'Human Leukocyte Antigen',
                                                               'Killer cell Immunoglobulin-like Receptors',
                                                               'Multiplex Bead Array Assay')),

                                       hr(),

                                       selectInput("select_virus",
                                                   "Choose a virus strain:",
                                                   choices = c('A/California/07/2009',
                                                               'A/Perth/16/2009',
                                                               'A/Victoria/361/2011',
                                                               'B/Brisbane/60/2008',
                                                               'B/Wisconsin/1/2010')),

                                       actionButton(inputId = "update_plot",
                                                    label = "Update Plot")),
                          mainPanel(tabsetPanel(tabPanel("Descriptive Profile of Subjects"),
                                                tabPanel("Visualisations",
                                                         br(),
                                                         withSpinner(plotOutput("dTTplotInput", height = "700"))
                                                         ))
                          )))),

             tabPanel("Analyze",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       radioButtons("model", "Statistical approach",
                                                     choices = c("Machine Learning" = "ml",
                                                                 "Classical Statistics" = "cs")),

                                       uiOutput("ml_choice"),

                                       hr(),
                                       selectInput("select_outcome",
                                                   "Choose an outcome of interest:",
                                                   choices = c('Lymphocyte to monocyte ratio (LMR)',
                                                               'Neutrophil to lymphocyte ratio (NLR)',
                                                               'Platelet (PLT)',
                                                               'Neutrophil",
                                                               "Lymphocyte (L)')),
                                       sliderInput("select_treshold",
                                                   "Choose a threshold for infection:",
                                                   min = 0, max = 10, value = 1.5,
                                                   step = 0.1),
                                       radioButtons("select_treshold",
                                                   "Should this threshold be:",
                                                   c(">" = "greater",
                                                     "<" = "less",
                                                     "=" = "equals")),


                                       actionButton(inputId = "run_model",
                                                    label = "Run Models")),
                          mainPanel(tabsetPanel(tabPanel("Correlate of Risk",

                                                         br(),
                                                         withSpinner(plotOutput("modelOutput", height = "700"))),
                                                tabPanel("Level 1 Surrogate of Protection"),
                                                tabPanel("Level 2 Surrogate of Protection"))
                        )))),
             tabPanel("Get Help",
                      fluidPage(
                        tabsetPanel(
                        ))),
             tabPanel("About",
                      fluidPage(
                        title = "About the Team",
                        # status = "primary",
                        br(),
                        tags$p(
                          "This dashboard was developed by ",
                          br(),
                          tags$a(href = "https://e.olamijuwon.com", target = "_blank", "Emmanuel Olamijuwon"),
                          br(),
                          tags$a(href = "", target = "_blank", "Kehinde Aruleba"),
                          br(),
                          tags$a(href = "", target = "_blank", "Adeniyi Fagbamigbe"),
                          "and many more collaborators"
                        )
                      ))
  )
