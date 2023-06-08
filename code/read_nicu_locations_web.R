# Read NICU locations from website

# Attach packages, installing as needed
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, folders, readr, dplyr, tidyr, stringr, purrr, rvest)

# Set options
options(readr.show_progress = FALSE)

# Setup folders
conf_file <- here('conf', 'folders.yml')
folders <- get_folders(conf_file)
res <- create_folders(folders)

# Get data
pg <-
  read_html('https://neonatologysolutions.com/nicu-directory-search-results/')

# Get number of NICUs
num_nicus <-
  pg %>% html_nodes("center") %>% html_nodes("strong") %>%
  html_text() %>% str_replace(' NICUs', '') %>% as.numeric()
num_nicus

# Get NICUs text
nicus <-
  pg %>% html_nodes("div.col-md-4") %>% html_text() %>% unique() %>%
  str_subset('Beds')
length(nicus)

# Parse NICUs text into key:value pairs and combine into a long dataframe
nicus_df <- map_df(nicus,
                   ~ tibble(
                     key = c('name', 'addr', 'city_state_zip', 'level_beds'),
                     value = read_lines(.x, n_max = 4, skip_empty_rows = TRUE)
                   ),
                   .id = "nicu")

# Reshape to one row per NICU
nicus_df_wide <- nicus_df %>%
  pivot_wider(id_cols = c(nicu), names_from = "key", values_from = "value") %>%
  separate(level_beds, c("level", "beds"), " \\| ") %>%
  mutate(level = str_replace(level, "Level ", ""),
         beds = str_replace(beds, " Beds", ""))
nrow(nicus_df_wide)

# Get markers text
markers <-
  pg %>% html_nodes("div.wpv-addon-maps-marker") %>% html_text() %>%
  str_subset('NICU')
length(markers)

# Parse markers text and combine into a dataframe
split_pattern <-
  '\\s+NICU Level | \\| | Beds Practice Type: | MD Contact: '
markers_df <- map_df(
  markers,
  ~ str_split(.x, pattern = split_pattern, simplify = TRUE) %>%
    as_tibble(.name_repair = "universal") %>% suppressMessages() %>% 
    set_names(c('name', 'level', 'beds', 'type', 'contact')),
  .id = "nicu"
)
nrow(markers_df)

# Cleanup markers dataframe
markers_df <- markers_df %>%
  mutate(across(everything(), ~ str_trim(.x))) %>%
  select(c(-level,-beds))

# Merge two datasets
df <-
  nicus_df_wide %>% left_join(markers_df %>% select(-name), by = c('nicu')) %>%
  mutate(beds = as.numeric(beds))
nrow(df)

# Save as CSV
write_csv(df, here(folders$results, "nicus.csv"))

# Cleanup folders
res <- cleanup_folders(folders, conf_file)
