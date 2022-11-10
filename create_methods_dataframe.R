# Title     : Let's face it too: Lateralization of the face perception 2.0
# Objective : Create dataframe containing all measurement methods
# Target    :
# Created by: jose c. garcia alanis
# Created on: 2022-11-10
# R version : R version 4.2.1 (2022-06-23)

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
  ),
  group_max_paths)

# read in individual-max files
indiv_max <- setNames(
  lapply(indiv_max_paths,
         read.table,
         sep = ',',
         header = T,
  ),
  indiv_max_paths
)

# read in individual-max (threshold) files
indiv_max_t <- setNames(
  lapply(indiv_max_threshold_paths,
         read.table,
         sep = ',',
         header = T,
  ),
  indiv_max_threshold_paths
)

# read in literature coordinate files
lit_coord <- setNames(
  lapply(lit_coord_paths,
         read.table,
         sep = ';',
         header = T,
  ),
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

# remove missing values (i,e., LI values == 9999 | 9999.9999)
all_data[abs(all_data$value) > 1.0, ]$value <- NA

# li-values should be positive for gamma model
all_data <- all_data %>%
  mutate(li_positive = (value + 1)) %>%
  mutate(roi_size = factor(roi_size))

# save
write.table(all_data, file = 'data/all_data.tsv', sep = '\t', row.names = FALSE)
