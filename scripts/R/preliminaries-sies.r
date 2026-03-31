
# -----------------------------------------------------------------------------
# 1. LOAD LIBRARIES
# -----------------------------------------------------------------------------

# Data import
library("haven")       # Import SAS, Stata (.dta), SPSS files
library("readxl")      # Read Excel files
library("foreign")     # Additional foreign data formats

# Data manipulation
library("tidyverse")   # Core tidyverse (dplyr, ggplot2, etc.)
library("tidyr")       # Data reshaping
library("magrittr")    # Pipe operators (%>%, %<>%)
library("forcats")     # Factor manipulation
library("stringr")     # String manipulation
library("labelled")    # Work with labelled data

library("openxlsx")    # Read/write Excel files with merged cells
# Parallel processing
library("furrr")       # Parallel purrr operations

# Statistical analysis
library("plm")         # Panel data econometrics
library("estimatr")    # Robust standard errors
library("broom")       # Tidy model outputs
library("modi")        # Additional statistical tools
library("spatstat")    # Spatial statistics
library("diagis")      # Diagnostic tools

# Visualization
library("ggplot2")     # Grammar of graphics
library("ggrepel")     # Non-overlapping text labels
library("gridExtra")   # Arrange multiple plots
library("ggpubr")      # Publication-ready plots, correlation coefficients
library("scales")      # Scale functions for visualization

# Output and reporting
library("stargazer")   # LaTeX/HTML regression tables
library("texreg")      # Regression output formatting

library("fuzzyjoin")   # For fuzzy string matching (e.g., stringdist_join)


# -----------------------------------------------------------------------------
# 2. SET UP DIRECTORIES
# -----------------------------------------------------------------------------

sies <- "C:\\Users\\djaar\\Dropbox\\research\\data\\chile-education\\sies"
files <- list.files(sies,  full.names = TRUE)


# -------------------------------------------------------------------------------
# 3. MATRICULA
# -------------------------------------------------------------------------------

# files are separated by ; and encoded in Windows-1252 (Latin-1)
matricula <- read_delim(
  files[ str_detect(files, "Matricula") ],
  delim = ";",
  locale = locale(encoding = "windows-1252")
)

# to lower and eliminate weird charaters and tildes
matricula <- matricula %>%
    rename_all( ~ str_to_lower(.) ) %>%
    rename_all( ~ str_replace_all(., " ", "_") ) %>%
    rename_all( ~ str_replace_all(., "-", "_") ) %>%
    rename_all( ~ str_replace_all(., "á", "a") ) %>%
    rename_all( ~ str_replace_all(., "é", "e") ) %>%
    rename_all( ~ str_replace_all(., "í", "i") ) %>%
    rename_all( ~ str_replace_all(., "ó", "o") ) %>%
    rename_all( ~ str_replace_all(., "ú", "u") ) %>% 
    rename_all( ~ str_replace_all(., "ñ", "n") ) %>%
    # fixing ano
    mutate( ano = parse_number(ano) ) %>%
    # renaming
    rename( nombre = nombre_institucion,
            code = codigo_de_institucion, 
            tipo1 = clasificacion_institucion_nivel_1,
            tipo2 = clasificacion_institucion_nivel_2,
            tipo3 = clasificacion_institucion_nivel_3 ) %>%
    # character id
    mutate( code = as.character(code) )


# Getting total student enrollment by institution and year, universities only
matricula_panel <-
    matricula %>%
    # selecting Unis
    filter( tipo1 == "Universidades" ) %>%
    group_by( nombre, code, ano ) %>%
    summarise( matricula_total = sum( total_matricula, na.rm = TRUE ),
               matricula_1yr = sum( total_matricula_primer_ano, na.rm = TRUE ) ) %>%
    ungroup() %>%
    # only undergrad carreers
    left_join( 
        matricula %>%
        # selecting Unis
        filter( tipo1 == "Universidades" ) %>%
        # discarding posgrado
        filter( carrera_clasificacion_nivel_2 %in% c("Carreras Profesionales", "Carreras Técnicas") ) %>%
        group_by( nombre, code, ano ) %>%
        summarise( matricula_pregrado = sum( total_matricula, na.rm = TRUE ),
                matricula_1yr_pregrado = sum( total_matricula_primer_ano, na.rm = TRUE ) ) %>%
        ungroup(),
        by = c("nombre", "code", "ano") )






# -------------------------------------------------------------------------------
# 4. ACADEMICOS
# -------------------------------------------------------------------------------


file_pac <- files[ str_detect(files, "PAC") ]

# Read both header rows (fillMergedCells fills the group name across all its columns)
headers <- openxlsx::read.xlsx(file_pac, sheet = 2, rows = 1:3,
                                colNames = FALSE, fillMergedCells = TRUE)

col_names <- mapply(function(r1, r2, r3) {
  parts <- c(r1, r2, r3)
  parts <- parts[ c(TRUE, parts[-1] != parts[-length(parts)]) ]
  paste(parts, collapse = "_")
}, headers[1, ], headers[2, ], headers[3, ])

# Read data starting from row 3, apply combined names
academicos <- openxlsx::read.xlsx(file_pac, sheet = 2, startRow = 4,
                                   colNames = FALSE, fillMergedCells = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(col_names)


academicos <-
    academicos %>%

    rename_all( ~ str_to_lower(.) ) %>%
    rename_all( ~ str_replace_all(., " ", "_") ) %>%
    rename_all( ~ str_replace_all(., "-", "_") ) %>%
    rename_all( ~ str_replace_all(., "á", "a") ) %>%
    rename_all( ~ str_replace_all(., "é", "e") ) %>%
    rename_all( ~ str_replace_all(., "í", "i") ) %>%
    rename_all( ~ str_replace_all(., "ó", "o") ) %>%
    rename_all( ~ str_replace_all(., "ú", "u") ) %>% 
    rename_all( ~ str_replace_all(., "ñ", "n") ) %>%

    mutate( ano = parse_number(periodo) ) %>%
    rename( nombre = nombre_institucion,
            code = codigo_institucion,
            tipo1 = tipo_institucion_i ) %>%
    # character id
    mutate( code = as.character(code) )


academicos_panel <-
    academicos %>%
    select( code, ano,
                'n°_de_jce_por_institucion_total_general',
                starts_with( "n°_de_jce_por_nivel_de_formacion" ),
                starts_with( "n°_de_jce_por_rango_de_horas_contratadas" ) ) %>%
    rename( jec_total = 3 ) %>%
    rename_with( ~ str_replace(., "n°_de_jce_por_nivel_de_formacion_", "jec_" ) ) %>%
    rename_with( ~ str_replace(., "n°_de_jce_por_rango_de_horas_contratadas_", "jec_horas_" ) )
    

                

    






# -------------------------------------------------------------------------------
# 5. OFERTA ACADEMICA
# -------------------------------------------------------------------------------


# files are separated by ; and encoded in Windows-1252 (Latin-1)
oferta <- read_delim(
  files[ str_detect(files, "Oferta") ],
  delim = ";",
  locale = locale(encoding = "windows-1252")
)











# -------------------------------------------------------------------------------
# 6. COMBINED PANEL
# -------------------------------------------------------------------------------

matricula_panel <-
    matricula_panel %>%
    select( - nombre ) %>%
    left_join( academicos_panel, by = c("code", "ano") )