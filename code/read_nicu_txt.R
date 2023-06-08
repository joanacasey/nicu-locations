# Read NICU locations from TXTs, which were prepared as follows:
#
#   (0) Started with 2 scanned NICU directory PDFs (one per year)...
#   (1) Used pdftk 3.3.2 (Java 16.0.2) to select/save only the pages needed
#       pdftk Neonatologists_USA_1996_Directory.pdf cat 129-189 output \
#             Neonatologists_USA_1996_Directory_129_to_189.pdf
#       pdftk Neonatologists_USA_2011_Directory.pdf cat 8-99 output \
#             Neonatologists_USA_2011_Directory_8_to_99.pdf
#   (2) Cropped pages by column (3) with Briss 2.0 (Java 16.0.2)
#   (3) OCR with R and {tesseract}... see: read_nicu_locations.R
#   (4) Manually editing (about 1 record per minute) TXT files to:
#       - remove unwanted characters and lines
#       - replace names of neonatologists with a count of neonatologists
#       - fix typos in the remaining text
#       - leave one blank line between records
#       - mark end of edited section (if not finished editing)
#           ### End of editing ###
#
# Once prepared, this script will read in the TXTs and write to CSV.

# Attach packages, installing as needed
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, folders, readr, dplyr, tidyr, stringr, purrr)
  
# Setup folders
conf_file <- here('conf', 'folders.yml')
folders <- get_folders(conf_file)
res <- create_folders(folders)
dir.create(here(folders$results, "edited"), 
           showWarnings = FALSE, recursive = TRUE)

# Get data
filenames <- list.files(path = here(folders$results, "edited"), 
                        pattern = 'Neonatologists_.*Directory\\.txt',
                        #pattern = 'Neonatologists_.*Directory_TEST\\.txt',
                        full.names = TRUE)
yrs <- basename(filenames) %>% str_extract('\\B\\d{4}\\B')
txt <- map(filenames, read_file) %>% set_names(yrs)

# Remove anything after "###" where the unedited records start
txt <- map(txt, ~ str_split(.x, '\\n[#]{3}', simplify = TRUE)[[1]])

# For each year (file), extract data and combine years into a single dataframe 
nicu_df <- map_df(txt, ~ {
  # Split records by blank line ("\n\n"), then "\nLevel", then "[\n;]"
  txt_list <- .x %>% 
    str_replace_all('\\n[a-z ]+', '\n') %>%  # Lines start with [A-Z0-9]
    str_replace_all('\\n\\nNICU|\\n[\\d\\s]+NICU', '\nNICU') %>% 
    str_split("\\n\\s*\\n", simplify = TRUE) %>%
    map(., ~ {
      #print(.x)     # To track progress ... if it fails, you'll know where
      map(.x, ~ str_trim(.x) %>%      
            str_replace('(\\d{4,})\\s*\\nNeon', '\\1\nLevel ; NICU Beds:\nNeon') %>%
            str_replace('\\nN[\\w]{2,} B[\\w]{2,}:', '\nLevel ; NICU Beds:') %>% 
            str_split("\\nLevel\\s*", simplify = TRUE))[[1]] %>% 
        map(., ~ str_split(.x, "\\s*[\\n;]\\s*", simplify = TRUE)
            %>% str_trim()) %>% 
        set_names(c("address", "nicu_data"))
      })
  
  # Process organization name and address
  addresses <- map_df(
    txt_list, ~ {
      x <- .x[['address']]
      .name <- x[str_detect(x, '^[^a-z0-9]*$')]
      .addr <- x[! x %in% .name]
      tibble(name = paste(.name, collapse = " "), 
             addr = paste(.addr[1:(length(.addr) - 1)], collapse = "\n"),
             city_state_zip = .addr[length(.addr)])
      }, .id = "nicu")
  
  # Process NICU data (# of NICU beds and # of neonatologists)
  nicu_data <- map_df(
    txt_list, ~ {
      x <- .x[['nicu_data']]
      tibble(level = x[1], 
             beds = x[2] %>% 
               str_replace('NICU Beds:\\s*', '') %>% 
               str_replace_all('\\D', '') %>% as.numeric() %>% 
               suppressWarnings(),
             neonatologists = x[3] %>% 
               str_replace('^.*:\\s*', '') %>% 
               str_replace_all('\\D', '') %>% as.numeric() %>% 
               suppressWarnings(),
             refer = paste(x[4:length(x)], 
               collapse = " ") %>% str_replace('^NA .*', ''))
      }, .id = "nicu")
  
  # Merge organization addresses and NICU data
  addresses %>% left_join(nicu_data, by = "nicu")
}, .id = "year")

# View results
nicu_df

# Write results to a CSV file
write_csv(nicu_df, here(folders$results, "nicus_from_txt.csv"))

# Cleanup folders
res <- cleanup_folders(folders, conf_file)
