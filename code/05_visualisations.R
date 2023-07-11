#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Data Wrangling | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################

showtext_auto()   ## Allow imported fonts to be used in a R-session.

## Import font "Barlow Condensed" into the R session.
font_add_google("Barlow Condensed","barlow")

## Import font "Special Elite" into the R session.
font_add_google("Special Elite", family = "special")

### Gather/reshape lnk_hi2neut data to long.
## This stores all antibody titers into a column = value and the corresponding time period with antibody type in days
lnk_hi2neut %>% 
  pivot_longer(cols = Day_0_hai:Day_28_neut_ab,
               values_to = "value",
               names_to = "days") %>% 
  ## Create a column titer that identified HAI vs Neutralising antibody
  mutate (titer = ifelse(str_detect(days, "hai"),
                         "HAI", "NEUT")) %>% 
  ## Remove antibody labels from all days.
  mutate (days = str_remove(days, "_hai|_neut_ab")) %>% 
  
  ## Replace data name (e.g. SDY296) with the actual study years.
  ## Create a new object [antibody_dta] with a new column that include study years and date.
  mutate (days = paste0(ifelse((link == "SDY296"),
                               "Year 2: ",
                               "Year 3: "),
                        str_replace(days, "_", " "))) -> antibody_dta

#' Create a data visualisation of trend in antibody titers against
#' A/California/07/2009 virus strain for all healthy adults 
#' in the data. Graph is faceted by antibody type (HAI/NEUT).
#' NOTE: You can also change the filter part to focus on other strains.
antibody_dta %>% 
  ## Fil
  filter (VIRUS_STRAIN_REPORTED=="A/California/07/2009")  %>% 
  ## n created as a sequential order of study period from Year 2, day 0 to year 3 day 28.
  group_by(SUBJECT_ACCESSION) %>% mutate (n = n()) %>% 
  ungroup () %>% filter (!is.na(value)) %>% 
  ggplot(aes(days,
             reorder(SUBJECT_ACCESSION, -n),
             fill=value)) +
  ## Fill tiles based on antibody titers.
  geom_tile(aes(fill=value),color="white") +
  facet_wrap(~ titer) +
  ## Use Barlow as base font for visualisation.
  theme_bw(base_family = "barlow") + theme (legend.position = "bottom", ## Legend position should be bottom.
                                            legend.key.width = unit(2.0,"cm"),   ## Width of legend key.
                                            legend.title = element_text(family = "special"),  ## Use font special for legend title.
                                            legend.text = element_text(family = "special"),  ## Use font special for plot title.
                                            title = element_text(size = 14)) +
  labs (fill = "Antibody Titer",
        x = "Study Timeline", y = "",
        title = paste0("Levels of hemagglutinin inhibition antibody titers \nagainst ",
                       "A/California/07/2009 incorporated in vaccines at \nbaseline and day 28 post-vaccination",
                       "across the study periods")) +
  scale_fill_continuous_sequential (palette = "Blues",
                                    guide = guide_colorbar(direction = "horizontal",   ## Legend direction should be horizontal. 
                                                           title.position = "top",   ## Legend title position should be top.
                                                           title.hjust = 0.5,   ## Legend title justification should be central.
                                                           keywidth = unit(15.0,"cm")))


#### The following Prevalence of Infection
  lnk_lab %>% group_by(link, infect_status) %>% 
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
                                              title = element_text(size = 14)) +
    labs (y = "",
          x = "", y = "",
          title = "Prevalence of Influenza infection (LMR < 2)")


  
new_data <- data.frame()

for (virus in unique(antibody_dta$VIRUS_STRAIN_REPORTED)) {
  
  for (day in unique(antibody_dta$days)) {
    
    for (tites in unique(antibody_dta$titer)) {
      
      gmean_data <- antibody_dta %>% filter (days == day &
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

  new_data %>% filter(mean != "NaN") %>% 
  remove_rownames() %>% 
  arrange(days, virus) %>% 
  ggplot(aes(x = days,
                 y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci),
                width = 0.1, size = 0.6) +
  facet_grid(virus ~ titer) + labs (y = "Geometric mean antibody-titers ",
                                    title = "Geometric Mean of Antibody Titers in Healthy Adults \nby Study Period and Antibody") +
  theme_bw(base_family = "barlow", 
           base_size = 45) + theme (legend.position = "bottom",
                                    legend.key.width = unit(2.0,"cm"),
                                    panel.grid = element_line(linewidth = 0.1),
                                            legend.title = element_text(family = "special"),
                                            legend.text = element_text(family = "special"),
                                    strip.background = element_rect(fill = "white"),
                                            title = element_text(size = 60,
                                                                 lineheight = 0.5)) +
  geom_hline(data = data.frame(yintercept = 40,titer="HAI"),
                        aes(yintercept = yintercept), linetype = "dotted")
    

ggsave("results/mean_titers.png", width = 12,
       height = 15, dpi = 250)

## Prevalence of Sero Conversion
rbind(
lnk_surrogate %>% 
  select (virus_strain_reported, link, hai_sero_stat) %>% 
  group_by (virus_strain_reported, link, hai_sero_stat) %>% 
  count () %>% ungroup () %>% group_by(virus_strain_reported, link) %>% 
  mutate (prop_sero = round(n/sum(n), 3) * 100) %>% filter(hai_sero_stat == "Seroconverted") %>% 
  mutate (vial_link = paste0 (link, ": ", virus_strain_reported)) %>% 
  rename (sero_stat = hai_sero_stat) %>% 
  mutate (antibody = "HAI"),

lnk_surrogate %>% 
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
         base_size = 45) + theme (legend.position = "none",
                                  legend.key.width = unit(2.0,"cm"),
                                  panel.grid = element_line(linewidth = 0.1),
                                  legend.title = element_text(family = "special"),
                                  legend.text = element_text(family = "special"),
                                  strip.background = element_rect(fill = "white"),
                                  plot.title = element_text(size = 60, family = "special",
                                                       lineheight = 0.5))
  

ggsave("results/seroconversion_prev.png", width = 12,
       height = 7, dpi = 250)




#### Prevalence of Sero-conversion for HAI
lnk_surrogate %>% 
    group_by(virus_strain_reported,
             link, hai_sero_stat, hai_sero_prot) %>% 
    select(virus_strain_reported,
             link, hai_sero_stat, hai_sero_prot) %>% 
    count() %>% ungroup () %>% 
    group_by (virus_strain_reported, link) %>% 
    mutate (cost = n/sum(n)) %>% 
    filter (!is.na(cost)) %>% 
    filter (!is.na(hai_sero_prot) &
              !is.na(hai_sero_stat)) %>% 
    ggplot() +
    geom_col (aes(x = cost,
                  y = paste0(hai_sero_prot, " & ", hai_sero_stat))) +
    facet_grid(virus_strain_reported ~ link) +
    scale_x_continuous(labels = scales::percent) +
    labs (y = "", x = "") +
    theme_bw(base_family = "barlow", 
         base_size = 45) + theme (legend.position = "none",
                                  legend.key.width = unit(2.0,"cm"),
                                  panel.grid = element_line(linewidth = 0.1),
                                  legend.title = element_text(family = "special"),
                                  legend.text = element_text(family = "special"),
                                  strip.background = element_rect(fill = "white"),
                                  plot.title = element_text(size = 60, family = "special",
                                                            lineheight = 0.5))
ggsave("results/hai_seroconver_prot.png", width = 9,
       height = 12, dpi = 250)


#### Prevalence of Sero-conversion for NEUT
lnk_surrogate %>% 
  group_by(virus_strain_reported,
           link, neut_sero_stat, neut_sero_prot) %>% 
  select(virus_strain_reported,
         link, neut_sero_stat, neut_sero_prot) %>% 
  count() %>% ungroup () %>% 
  group_by (virus_strain_reported, link) %>% 
  mutate (cost = n/sum(n)) %>% 
  filter (!is.na(cost)) %>% 
  filter (!is.na(neut_sero_prot) &
            !is.na(neut_sero_stat)) %>% 
  ggplot() +
  geom_col (aes(x = cost,
                y = paste0(neut_sero_prot, " & ", neut_sero_stat))) +
  facet_grid(virus_strain_reported ~ link) +
  scale_x_continuous(labels = scales::percent) +
  labs (y = "", x = "") +
  theme_bw(base_family = "barlow", 
           base_size = 45) + theme (legend.position = "none",
                                    legend.key.width = unit(2.0,"cm"),
                                    panel.grid = element_line(linewidth = 0.1),
                                    legend.title = element_text(family = "special"),
                                    legend.text = element_text(family = "special"),
                                    strip.background = element_rect(fill = "white"),
                                    plot.title = element_text(size = 60, family = "special",
                                                              lineheight = 0.5))
ggsave("results/neut_seroconver_prot.png", width = 9,
       height = 12, dpi = 250)
