#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Models | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################


model_data <- lnk_corr_surr %>%
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

#' Due to the limited sample size and the prevalence of infection
#' we ultimately decided to fit a simple multivariate logistic regression model
#' Results from this model are formatted and presented in a forest plot 
#' for ease of interpretation.
#' 
fx <- glm(as.factor(infect_status) ~ .,
          #' We have excluded study period (although very important) and 
          #' participant ID from the regression model
          model_data %>% dplyr::select (-c(link, subject_accession)),
          ## We fitted a binomial logistic regression model.
          family = binomial("logit"))

## Extract co-efficients from the data.
coefficients <- coef(fx) %>% data.frame() %>% 
                rownames_to_column()

colnames(coefficients) <- c("Variables", "Coefficients")

## Extract 95% confidence intervals from the model output.
confint <- confint(fx) %>% data.frame() %>% 
           clean_names() %>% rownames_to_column("Variables")
          
## Merge extracted co-efficients and 95% confidence intervals.
results <- coefficients %>% 
          left_join(., confint,
                    "Variables") %>% 
           mutate (Coefficients = round(Coefficients, 3),
                   x2_5 = round(x2_5, 3),
                   x97_5 = round(x97_5, 3))
        
## Create a forest plot with results from the model
## based on the extracted co-efficients and 95% confidence intervals.
results %>% filter (Variables != "(Intercept)") %>% 
  ggplot(aes(x = Coefficients,
             y = Variables)) +
  geom_point() +
  geom_errorbar(aes(xmin = x2_5, xmax = x97_5),
                width = 0.1, size = 0.6) +
  scale_x_continuous(limits = c(-0.35,0.35)) +
  labs (y = "Virus Strain",
        title = "Correlates of Influenza Infection") +
  theme_bw(base_family = "barlow", 
           base_size = 45) + theme (legend.position = "bottom",
                                    legend.key.width = unit(2.0,"cm"),
                                    panel.grid = element_line(linewidth = 0.1),
                                    legend.title = element_text(family = "special"),
                                    legend.text = element_text(family = "special"),
                                    strip.background = element_rect(fill = "white"),
                                    title = element_text(size = 60,
                                                         lineheight = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dotted")


ggsave("results/regress_correlates.png", width = 9,
       height = 5, dpi = 250)
