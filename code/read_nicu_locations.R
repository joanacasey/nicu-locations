# Read NICU locations from directory PDFs. For each year (one per PDF file):
#
# (1) Convert PDFs for PNGs, one per page, with pdf_convert()
# (2) Extract text from PNGs with ocr()
# (3) Sort extracted texts into same order as original PDF
# (4) Combine texts and save as one TXT file per year

# Attach packages, installing as needed
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, folders, readr, dplyr, tidyr, stringr, pdftools, tesseract)

# Setup folders
folders <- get_folders(here('conf', 'folders.yml'))
res <- create_folders(folders)
map(c('pdf', 'png', 'txt'), ~ {
  dir.create(here(folders$data, .x), showWarnings = FALSE, recursive = TRUE)
})

# Setup tesseract OCR engine for English language
eng <- tesseract("eng")

# Extract pages from PDF as PNG
setwd(here(folders$data, "png"))
pdfs <- list.files(path = here(folders$data, "pdf"), "\\cropped_col[123].pdf$", 
                   full.names = TRUE, recursive = TRUE)
res <- map(pdfs, ~ pdf_convert(pdf = .x, dpi = 300))

# Extract text from PNG and write to TXT files
setwd(here(folders$data, "txt"))
pngs <- list.files(path = here(folders$data, "png"), "\\.png$", 
                                     full.names = TRUE, recursive = TRUE)
res <- map(pngs, ~ {
  ocr(.x, engine = eng) %>% 
    write(here(folders$data, "txt", 
               str_replace(basename(.x), "\\.png", ".txt")))
})


# Create a dataframe of TXT files with IDs to use for merging in next step
txts <- list.files(path = here(folders$data, "txt"), "\\.txt$", 
                   full.names = TRUE, recursive = TRUE)
txts_df <- tibble(filename = basename(txts)) %>% mutate(ID = row_number())

# Create an index of text files and sort to correct sequence for joining files
txts_index_df <- map_df(
  basename(txts_df$filename), 
  ~ str_split(str_replace(
    .x,
    'Neonatologists_USA_(\\d+)_.*_cropped_col(\\d)_(\\d+)\\.txt$', 
    '\\1_\\2_\\3'), "_", simplify = TRUE) %>%
  as_tibble(.name_repair = "universal") %>% suppressMessages() %>%
  set_names(c('Year', 'column', 'page')), .id = "ID") %>%
  mutate(page = as.numeric(page),
         ID = as.numeric(ID)) %>% 
  arrange(`Year`, page, column) %>% 
  left_join(txts_df, by = c("ID"))

# Read TXT files in order, combine, and save as one TXT file per year
years <- unique(txts_index_df$Year)
txt_filenames <- map(years, ~ here(folders$data, "txt", 
                      txts_index_df %>% filter(`Year` == .x) %>% 
                        pull(filename))) %>% setNames(years)
res <- sapply(years, function(yr) {
  sapply(txt_filenames[[yr]], function(fn) {
    read_file(fn) %>% 
      write_file(file = here(folders$results, 
        paste0('Neonatologists_USA_', yr, '_Directory.txt')), append = TRUE)
    })
  })

# Cleanup folders
unlink(here(folders$data, "png"), recursive = TRUE, force = TRUE)
res <- cleanup_folders(folders)
