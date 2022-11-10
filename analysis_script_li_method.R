# Title     : Let's face it: Lateralization of the face perception 
# Objective : Test effect of handedness on lateralization score
# Target    : Voxel Value on Conjunction Faces vs Houses, Faces vs Scrambled
# Created by: jose c. garcia alanis
# Created on: 2022-02-01
# R version : R version 4.1.2 (2021-11-01)

# set working directory
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

# function for installing and loading packages
pkgcheck <- function(packs) {

  # Check which packages are not installed
  if (sum(!packs %in% installed.packages()[, 'Package'])) {
    # install them
    install.packages(packs[which(!packs %in% installed.packages()[, 'Package'])],
                     dependencies = T)
  }

  # load all packages
  sapply(packs, require, character.only = T)

}

apa <- function(x, title = " ", stub = T) {
  gt(x, rownames_to_stub = stub) %>%
    tab_stubhead(label = "Predictor") %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      stub.border.color = "white",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) %>%
    cols_align(align="center") %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left")
}


# 1) get the data --------------------------------------------------------------
pkgcheck(c('dplyr', 'tidyr', 'stringr'))

# get file paths
group_max_paths <- Sys.glob('osfstorage-archive/group-max/*.csv')
indiv_max_paths <- Sys.glob('osfstorage-archive/individual-max/*.csv')
indiv_max_threshold_paths <- Sys.glob('osfstorage-archive/individual-max-threshold/*.csv')
lit_coord_paths <- Sys.glob('osfstorage-archive/literature-coordinates/*.csv')

# read in group-max files
group_max <- setNames(
  lapply(group_max_paths,
         read.table,
         sep = ',',
         header = T,
         na.strings = 9999.0000
  ),
  group_max_paths)

# read in individual-max files
indiv_max <- setNames(
  lapply(indiv_max_paths,
         read.table,
         sep = ',',
         header = T,
         na.strings = 9999.0000),
  indiv_max_paths
)

# read in individual-max (threshold) files
indiv_max_t <- setNames(
  lapply(indiv_max_threshold_paths,
         read.table,
         sep = ',',
         header = T,
         na.strings = 9999.0000),
  indiv_max_threshold_paths
)

# read in literature coordinate files
lit_coord <- setNames(
  lapply(lit_coord_paths,
         read.table,
         sep = ';',
         header = T,
         na.strings = 9999.0000),
  lit_coord_paths)

# row bind the data frames
group_max_df <- bind_rows(group_max, .id = 'file') %>%
  mutate(method = 'group max')
indiv_max_df <- bind_rows(indiv_max, .id = 'file') %>%
  mutate(method = 'individual max')
indiv_max_t_df <- bind_rows(indiv_max_t, .id = 'file') %>%
  mutate(method = 'individual max threshold')
lit_coord_df <- bind_rows(lit_coord, .id = 'file') %>%
  mutate(method = 'literature based')

# row bind dataframes
all_data <- bind_rows(
  list(group_max_df, indiv_max_df, indiv_max_t_df, lit_coord_df)
)

# drop `notes` column
all_data <- all_data %>%
  mutate(roi_size = str_extract(file, pattern = '[0-9]mm|[0-9][0-9]mm')) %>%
  select(!notes, !file)

# 2) prepare demographic data for modelling ------------------------------------

# categorical variables to factors, mean-center continuous variables
all_data <- all_data %>%
  mutate(sex = ifelse(sex == 1, 'female', 'male'),
         hand = ifelse(EHI_handedness == 1, 'right handed', 'left handed'),
         age_c = age - mean(age)) %>%
  mutate(sex = factor(sex),
         hand = factor(hand))

# select relevant columns
all_data <- all_data %>%
  select(subID, FFA_LI_wm:STS_LI_wm,
         sex, age_c, hand, EHI_handedness, method, roi_size) %>%
  gather(area, value, FFA_LI_wm:STS_LI_wm)

# remove suffix '_LI_wm' from variable area
all_data$area <- gsub(all_data$area, pattern = '_LI_wm', replacement = '')
all_data$area <- factor(all_data$area, levels = c('OFA', 'FFA', 'STS'))

# li-values should be positive for gamma model
all_data <- all_data %>%
  mutate(li_positive = (value + 1)) %>%
  mutate(roi_size = factor(roi_size, levels = c('6mm', '8mm', '10mm', '12mm', '14mm')))

# 3) plot distribution of LI-values --------------------------------------------
pkgcheck(c('dplyr', 'ggplot2', 'viridis', 'ggbeeswarm', 'see', 'Hmisc'))

pn <- position_nudge(x = 0.15)
# create density plots
for (roi_method in unique(all_data$method)) {

  width <- 20
  height <- 40

  if (roi_method == 'literature based') {
    width <- 20
    height <- 40 /4
  }

  li_plot <- ggplot(filter(all_data, method == roi_method),
                    aes(x = EHI_handedness, y = value,
                        fill = area, color = hand, shape = hand)) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = -0.2, ymax = 0.2,
             alpha = .1) +
    geom_hline(yintercept = 0.0, linetype='dotted', size = 0.8) +
    geom_beeswarm(size = 2, color = 'black', alpha = 0.75,stroke = 1.0,
                  show.legend = F) +
    geom_violinhalf(position = pn, width = 0.70, alpha = 0.75, size = 0.8, show.legend = F) +
    geom_boxplot(position = pn, width = 0.10, alpha = 1.0, size = 0.8,
                 color = 'black', outlier.shape = NA, show.legend = F) +
    scale_shape_manual(values = c(23, 21)) +
    scale_color_manual(values = c('red2', 'black')) +
    facet_wrap(vars(roi_size, area), scales = 'free', ncol = 3) +
    scale_y_continuous(limits = c(-1.0, 1.0),
                       breaks = seq(-1.0 , 1.0, 0.5)) +
    scale_x_continuous(limits = c(0.8, 2.5),
                       breaks = seq(1, 2),
                       labels = c('Right', 'Left')) +
    coord_flip() +
    labs(title = roi_method,
         x = 'Handedness',
         y = 'LI Value',
         fill = NULL,
         shape = 'Handedness',
         color = 'Handedness') +
    scale_fill_viridis(option = 'D', discrete = T, begin = 0.1) +
    geom_segment(aes(x = -Inf, y = -1.0, xend = -Inf, yend = 1.0),
                 color = 'black', size = rel(1.0), linetype = 1) +
    geom_segment(aes(x = 1, y = -Inf, xend = 2, yend = -Inf),
                 color = 'black', size = rel(1.0), linetype = 1) +
    theme(plot.margin = unit(c(10, 10, 10, 10), 'pt'),
          panel.background = element_rect(fill = 'white'),
          panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray98"),
          panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gray98"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray98"),
          plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          axis.title.x = element_text(color = 'black', size = 12, face = 'bold',
                                      margin = margin(t = 10)),
          axis.title.y= element_text(color = 'black', size = 12, face = 'bold',
                                     margin = margin(r = 10)),
          axis.text.x = element_text(color = 'black', size = 12),
          axis.text.y = element_text(color = 'black', face = 'bold', size = 12),
          strip.text = element_text(color = 'black', size = 12, face = 'bold'),
          strip.background = element_blank(),
          legend.position='bottom',
          legend.text = element_text(size = 9),
          legend.title = element_text(hjust = 0.5, face = 'bold'),
          panel.spacing = unit(1.0, "lines")) +
    guides(fill = "none",
           # shape = guide_legend(title.position = "bottom"),
           shape = "none"); li_plot
  ggsave(filename = paste0('./LI_plot_', roi_method, '_hand.png'),
         plot = li_plot,
         width = width, height = height,  units = 'cm', dpi = 300)

}


# create dot plot showing distribution of LI values by area and participants'
# characteristics
int_plot <- ggplot(data = data, 
                   aes(x = sex, y = value, 
                       color = hand, shape = sex, fill = hand)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.2, ymax = 0.2,
           alpha = .1) +
  geom_segment(aes(x = -Inf, y = -1, xend = -Inf, yend = 1),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = 'female', y = -Inf, xend = 'male', yend = -Inf),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0),
               color = 'black', linetype = 3, size = 1) +
  geom_beeswarm(dodge.width = 1.0, size = 2.0, stroke = 1.0, fill = NA) +
  stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.5),
               geom = 'linerange',
               size = 0.8, show.legend = F) +
  stat_summary(fun.data = 'mean_cl_boot', position = position_dodge(0.5),
               color = 'black', geom = 'point',
               size = 2.5, stroke = 1.0, show.legend = F) +
  facet_wrap(~ area, scales = 'free') +
  scale_color_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_fill_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_shape_manual(values = c(21, 23)) +
  theme(plot.margin = unit(c(5, 30, 5, 5), 'pt'),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 18, face = 'bold'), 
        plot.title = element_text(color = 'black', size = 18, face = 'bold', 
                                  hjust = 0),
        axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(r = 10)),
        axis.text.x = element_text(color = 'black', size = 14),
        axis.text.y = element_text(color = 'black', size = 14),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray98"),
        legend.text = element_text(color="black", size=rel(1.25)),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Observed LI-Value', x = '',
       fill = 'Handedness', color = 'Handedness',
       shape = 'Sex'); int_plot
ggsave(filename = './interaction_plot_area_sex_hand.tiff',
       plot = int_plot, 
       width = 30, height = 12.5, units = 'cm',
       dpi = 1200)

# check if lateralisation patterns are consistent across subject
data <- data %>% 
  select(subID, area, value) %>%
  filter(area == 'OFA') %>%
  mutate(lat = ifelse(area == 'OFA' & value > 0.2, 'left dom.',
                      ifelse(area == 'OFA' & value < -0.2, 'right dom.', 'bilateral'))) %>%
  select(subID, lat) %>%
  right_join(., data, 'subID')

# plot subjects by sex and handedness
pd = position_dodge(0.2)
subj_plot <- ggplot(data = filter(data, !is.na(lat)), 
                    aes(x = area,
                        y = value, group = subID, 
                        shape = sex, color = hand)) +
  scale_x_discrete(limits = c('OFA', 'FFA', 'STS')) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.2, ymax = 0.2,
           alpha = .1) +
  geom_segment(aes(x = -Inf, y = -1, xend = -Inf, yend = 1),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = 'OFA', y = -Inf, xend = 'STS', yend = -Inf),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0),
               color = 'black', linetype = 3, size = 1) +
  geom_line(position = pd, alpha = 0.15, size = 0.6, color = 'black') +
  geom_point(position = pd, size = 2.0, stroke = 1.0, fill = NA) +
  facet_grid(rows = vars(sex), cols = vars(hand), scales = "free") +
  scale_color_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_fill_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_shape_manual(values = c(21, 23)) +
  theme(plot.margin = unit(c(5, 30, 5, 5), 'pt'),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 18, face = 'bold'), 
        plot.title = element_text(color = 'black', size = 18, face = 'bold', 
                                  hjust = 0),
        axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(r = 10)),
        axis.text.x = element_text(color = 'black', size = 14),
        axis.text.y = element_text(color = 'black', size = 14),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray98"),
        legend.text = element_text(color="black", size=rel(1.25)),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Observed LI-Value', x = '',
       color = 'Handedness',
       shape = 'Sex'); subj_plot
ggsave(filename = './indiv_differences_lateralisation.tiff',
       plot = subj_plot, 
       width = 20, height = 18, units = 'cm',
       dpi = 1200)

# plot subjects by lateralisation of the OFA
pd = position_dodge(0.2)
lat_plot <- ggplot(data = filter(data, !is.na(lat)), 
                   aes(x = area,
                       y = value, group = subID, 
                       shape = sex, color = hand)) +
  scale_x_discrete(limits = c('OFA', 'FFA', 'STS')) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -0.2, ymax = 0.2,
           alpha = .1) +
  geom_segment(aes(x = -Inf, y = -1, xend = -Inf, yend = 1),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = 'OFA', y = -Inf, xend = 'STS', yend = -Inf),
               color = 'black', linetype = 1, size = 1) +
  geom_segment(aes(x = -Inf, y = 0, xend = Inf, yend = 0),
               color = 'black', linetype = 3, size = 1) +
  geom_line(position = pd, alpha = 0.15, size = 0.6, color = 'black') +
  geom_point(position = pd, size = 2.0, stroke = 1.0, show.legend = T,
             fill = NA) +
  facet_grid(rows = vars(sex), cols = vars(lat), scales = "free") +
  scale_color_manual(values = c("#EF8A62", "#67A9CF")) +
  scale_shape_manual(values = c(21, 23)) +
  theme(plot.margin = unit(c(5, 30, 5, 5), 'pt'),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 18, face = 'bold'), 
        plot.title = element_text(color = 'black', size = 18, face = 'bold', 
                                  hjust = 0),
        axis.title.x = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(t = 10)),
        axis.title.y = element_text(color = 'black', size = 16, face = 'bold',
                                    margin = margin(r = 10)),
        axis.text.x = element_text(color = 'black', size = 14),
        axis.text.y = element_text(color = 'black', size = 14),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gray98"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'solid',
                                          colour = "gray98"),
        legend.text = element_text(color="black", size=rel(1.25)),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        legend.position = "bottom") +
  labs(y = 'Observed LI-Value', x = '',
       fill = 'Handedness',
       shape = 'Sex'); lat_plot
ggsave(filename = './Fig_S2_by_lateralisation_plot.tiff',
       plot = lat_plot, 
       width = 20, height = 18, units = 'cm',
       dpi = 1200)

# check correlation between LI values
data_wide <- data %>%
  pivot_wider(names_from ='area', values_from = c('value', 'li_positive')) 

data_wide %>% 
  select(lat, sex, hand) %>% 
  group_by(lat, sex) %>% tally()

pkgcheck('apaTables')
data_wide %>%
  select(value_FFA:value_STS) %>%
  apa.cor.table(file='./Table_S1_LI_values_correlation.rtf')

# # Correlation table for Subgroups of data
# data_wide %>%
#   filter(lat == 'right dom.') %>%
#   select(value_FFA:value_STS) %>%
#   apa.cor.table(file='./Table_S2_LI_values_correlation_right.rtf')
# 
# data_wide %>%
#   filter(lat == 'left lat.') %>%
#   select(value_FFA:value_STS) %>%
#   apa.cor.table(file='./Table_S3_LI_values_correlation_left.rtf')
# 
# data_wide %>%
#   filter(lat == 'bilateral') %>%
#   select(value_FFA:value_STS) %>%
#   apa.cor.table(file='./Table_S4_LI_values_correlation_bilateral.rtf')



# 4) compute descriptive statistics --------------------------------------------
pkgcheck('sjPlot')

data %>% 
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './overall.html', digits = 3)

data %>% 
  group_by(sex) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  as.data.frame() %>%
  tab_df(file = './sex_descriptives.html', digits = 3)

data %>% 
  group_by(hand) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_descriptives.html', digits = 3)

data %>% 
  group_by(area, hand) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './area_hand_descriptives.html', digits = 3)

data %>% 
  group_by(area) %>%
  summarise(m = mean(value, na.rm=T), 
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './area_descriptives.html', digits = 3)

data %>% 
  group_by(hand, sex) %>%
  summarise(m = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_sex_descriptives.html', digits = 3)

data %>% 
  group_by(hand, sex, area) %>%
  summarise(m = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            med = median(value, na.rm=T)) %>%
  tab_df(file = './hand_sex_area_descriptives.html', digits = 3)

# 5) compute mixed effects model -----------------------------------------------
pkgcheck(c('multimode', 'lme4', 'car', 'performance', 'effectsize', 'gt'))

# check if distribution of LI values is bimodal
# whole sample
modetest(na.omit(data$value))
# only FFA
multimode::modetest(na.omit(data[data$area == 'OFA', ]$value))
multimode::modetest(na.omit(data[data$area == 'FFA', ]$value))
multimode::modetest(na.omit(data[data$area == 'STS', ]$value))

# overall model
mod_gamma_li <- glmer(data = data,
                      li_positive ~ age_c + sex * hand * area + (1|subID),
                      contrasts = list(area = 'contr.sum',
                                       sex = 'contr.sum',
                                       hand = 'contr.sum'), 
                      family = Gamma(link = "log"),
                      control = glmerControl(optimizer="bobyqa",
                                             optCtrl = list(maxfun=2e5)))
anova_mod_gamma <- car::Anova(mod_gamma_li, type = 'III')
anova_mod_gamma %>%
  apa("ANOVA table overall LI-model: Gamma model for LI values") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_overall.html", inline_css = TRUE)
tiff(filename = './gamma_overall_model_diagnostics.tiff',
     width = 15, height = 20, units = 'cm', res =  1200)
check_model(mod_gamma_li)
dev.off()
model_performance(mod_gamma_li) %>%
  apa("Overall Gamma LI-model performance", stub = F) %>%
  fmt_number(columns = c(1:7), decimals = 4) %>%
  tab_source_note(
    source_note = 
      md(paste0('*Note.* Foumla: `li ~ ', as.character(formula(mod_gamma_li))[3], '`' ))
  ) %>%
  gtsave(
    "./mod_gamma_overall_performance.html", inline_css = TRUE)
standardize_parameters(mod_gamma_li) %>%
  apa("Standardized beta coefficients: 
      Overall Gamma LI-model", stub = F) %>%
  fmt_number(columns = c(2, 5), decimals = 4) %>%
  tab_footnote(
    footnote = "age, centred around zero",
    locations = cells_body(
      columns = Parameter,
      rows = 2)) %>%
  tab_footnote(
    footnote = "sex1 = female",
    locations = cells_body(
      columns = Parameter,
      rows = 3)) %>%
  tab_footnote(
    footnote = "hand1 = left handed",
    locations = cells_body(
      columns = Parameter,
      rows = 4)) %>%
  tab_footnote(
    footnote = "area1 = OFA",
    locations = cells_body(
      columns = Parameter,
      rows = 5)) %>%
  tab_footnote(
    footnote = "area2 = FFA",
    locations = cells_body(
      columns = Parameter,
      rows = 6)) %>%
  gtsave(
    "./standardized_beta_mod_gamma.html", inline_css = TRUE)


# right-handers model
mod_gamma_li_rh <- glmer(data = filter(data, hand == 'right handed'),
                         li_positive ~ age_c + sex * area + (1|subID),
                         contrasts = list(area = 'contr.sum',
                                          sex = 'contr.sum'), 
                         family = Gamma(link = "log"))
anova_mod_gamma_rh <- car::Anova(mod_gamma_li_rh, type = 'III')
anova_mod_gamma_rh %>%
  apa("ANOVA table right handers' LI-model: 
      Gamma model for LI values from right handed participants") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_right_handers.html", inline_css = TRUE)



# left-handers model
mod_gamma_li_lh <- glmer(data = filter(data, hand == 'left handed'),
                         li_positive ~ age_c + sex * area + (1|subID),
                         contrasts = list(area = 'contr.sum',
                                          sex = 'contr.sum'), 
                         family = Gamma(link = "log"))
anova_mod_gamma_lh <- car::Anova(mod_gamma_li_lh, type = 'III')
anova_mod_gamma_lh %>%
  apa("ANOVA table left handers' LI-model: 
      Gamma model for LI values from left handed participants") %>%
  fmt_number(columns = c(2, 4), decimals = 4) %>%
  gtsave(
    "./anova_mod_gamma_left_handers.html", inline_css = TRUE)


# 6) compute exploratory OLS model ---------------------------------------------
pkgcheck(c('effectsize', 'caret', 'splitstackshape'))

# get ffa data
data_ffa <- data %>% filter(area == 'FFA')

mod_ffa <- lm(data = data_ffa,
              value ~ age_c + sex * hand,
              contrasts = list(sex = 'contr.sum',
                               hand = 'contr.sum'))
anova_mod_ffa <- car::Anova(mod_ffa, type = 'III', test = 'F')
anova_mod_ffa %>%
  apa("ANOVA table FFA LI-model") %>%
  fmt_number(columns = c(2, 5), decimals = 4) %>%
  tab_source_note(
    source_note = 
      md(paste0('*Note.* Foumla: `li ~ ', as.character(formula(mod_ffa))[3], '`' ))
  ) %>%
  gtsave(
    "./anova_mod_ffa.html", inline_css = TRUE)

# ** run cross validation **
# split data into train and test using stratified sampling
d <- tibble::rownames_to_column(data_ffa, var = "id") %>% 
  mutate_at(vars(id), as.integer)
training <- d %>% 
  stratified(., group = c("hand", 'sex'), size = 0.80)
dim(training)

# proportion check
prop.table(table(training$hand)) 

tControl <- trainControl(
  method = "cv", # cross validation
  number = 10, # 10 folds
  search = "random", # auto hyperparameter selection
  savePredictions = T
)

set.seed(256)
cv_lm <- train(
  value ~ age_c + sex * hand, 
  data = training[,-1], 
  method = "lm",
  trControl = tControl
)

# get best model
summary(cv_lm$finalModel)
print(cv_lm)
anova_cv_mod_ffa <- car::Anova(cv_lm$finalModel, type = 'III', test = 'F')
anova_cv_mod_ffa %>%
  apa("ANOVA table FFA LI-model (Cross Validation)") %>%
  fmt_number(columns = c(2, 5), decimals = 4) %>%
  tab_source_note(
    source_note = 
      md(paste0('*Note.* Foumla: `li ~ ', as.character(formula(cv_lm$finalModel))[3], '`' ))
  ) %>%
  gtsave(
    "./anova_mod_ffa_cv.html", inline_css = TRUE)

standardize_parameters(mod_ffa) %>%
  apa("Standardized beta coefficients: 
      FFA LI-model", stub = F) %>%
  fmt_number(columns = c(2:5), decimals = 4) %>%
  tab_footnote(
    footnote = "age, centred around zero",
    locations = cells_body(
      columns = Parameter,
      rows = 2)) %>%
  tab_footnote(
    footnote = "sex1 = female",
    locations = cells_body(
      columns = Parameter,
      rows = 3)) %>%
  tab_footnote(
    footnote = "hand1 = left handed",
    locations = cells_body(
      columns = Parameter,
      rows = 4))  %>%
  gtsave(
    "./standardized_beta_mod_ffa.html", inline_css = TRUE)

# check residuals
plot(mod_ffa, ask = F)

# 7) compute pairwise contrasts for interaction --------------------------------
pkgcheck('emmeans')

ffa_means <- emmeans(mod_ffa, ~ hand  | sex)
ffa_means %>%
  data.frame() %>%
  apa("Estimated marginal means (emmeans) for FFA model",
      stub = F) %>%
  fmt_number(columns = c(3:7), decimals = 4) %>%
  gtsave(
    "./emmeans_mod_ffa.html", inline_css = TRUE)

contrast(ffa_means, 'tukey', adjust = 'fdr') %>%
  data.frame() %>%
  mutate(p.corrected = p.adjust(p.value, 'hochberg')) %>%
  apa("Pairwise contrasts for FFA model",
      stub = F) %>%
  fmt_number(columns = c(3:8), decimals = 4) %>%
  gtsave(
    "./contrasts_mod_ffa.html", inline_css = TRUE)