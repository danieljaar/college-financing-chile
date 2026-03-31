# =============================================================================
#                         EEFF DATA EXPLORATION
# =============================================================================
#
#
# =============================================================================


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

eeffDir = "C:\\Users\\djaar\\Dropbox\\research\\data\\chile-education\\estados-financieros"
eeffDir2 = "C:\\Users\\djaar\\Dropbox\\research\\data\\chile-education\\estados-financieros\\superintendencia-edu-superior"

infDir = "C:\\Users\\djaar\\Dropbox\\research\\data\\chile-education"


# -----------------------------------------------------------------------------
# 3. LOAD DATA
# -----------------------------------------------------------------------------


# Files from mifuturo
files_old = list.files( eeffDir, full.names = TRUE, pattern = "xlsx" )
# Files from SES
files_new = list.files( eeffDir2, full.names = TRUE )
# files_new correspond to the year, files_old correspond to the previous year

# Inflation data for adjusting to real values 
deflator <- read_excel( list.files( infDir, full.names = TRUE, pattern = "inflation" ), skip = 2 ) %>%
		  rename( country = 1, year = 2, inflation = 3, ind = 4) %>%
          filter( country == "Chile" ) %>%
          select( -ind ) %>%
          # turn everything to character
          mutate( across( everything(), as.character ) ) %>%
          pivot_longer(-country, names_to = "year", values_to = "deflator" ) %>%
          filter( year %in% 1990:2023 ) %>%
          mutate( year = as.integer(year), deflator = as.numeric(deflator)/100 ) %>%
          mutate( level = 1, deflator = ifelse( year == 1990, 0.0, deflator ) ) %>%
          # Expressing everything in 1990 CLP
          mutate( level = 1 / cumprod(1 + deflator) ) %>%
          # normalize everything to year 2016
          mutate( level = level / level[ year == 2016 ] )


years_old <- list()
for ( i in 1:length(files_old) ) {

    x = parse_number (gsub("[^\\p{L}0-9\\s]","", basename( files_old[i] ), perl = TRUE) )

    years_old[ i ] = x

}

years_new <- list()
for ( i in 1:length(files_new) ) {

    x = parse_number (gsub("[^\\p{L}0-9\\s]","", basename( files_new[i] ), perl = TRUE) )

    years_new[ i ] = x

}


# File list
files <- tibble( files = files_old, year = unlist(years_old) ) %>% arrange( year ) %>%
        # adjusting years
        mutate( year = year - 1 ) %>%
        bind_rows( tibble( files = files_new, year = unlist(years_new) ) %>% arrange( year ) ) %>%
        mutate( n_sheets = map_int(files, ~ length(excel_sheets(.x))) )

# year 2023 has 4 sheets for some reason

eeff_list <- list()
fecu_list <- list()


for (i in 1:nrow(files)) {

    f    <- files$files[i]
    year <- files$year[i]
    cat("Reading:", basename(f), "\n")
    cat("Year:", year, "\n")

    needs_skip <- function(df) {
      all(grepl("^\\d+$", names(df))) || any(grepl("^\\.\\.\\.\\d+$", names(df)))
    }

    # Sheet 1: Balance sheet (EEFF)
    df1 <- read_excel(f, sheet = 1)
    # Some files have merged-cell headers: row 1 = group labels (with ...N placeholders),
    # row 2 = actual column names. Re-read with skip = 1 in that case.
    if (needs_skip(df1)) df1 <- read_excel(f, sheet = 1, skip = 1)
    eeff_list[[i]] <- df1 %>% mutate( year = year )

    # Sheet 2: Income & expenses (FECU)
    # 2023 file has 4 sheets; FECU is on sheet 4 instead of 2
    fecu_sheet <- if (year == 2023) 4 else 2
    df2 <- read_excel(f, sheet = fecu_sheet)
    if (needs_skip(df2)) df2 <- read_excel(f, sheet = fecu_sheet, skip = 1)
    fecu_list[[i]] <- df2 %>% mutate( year = year)
  }

# Name lists by year for robust indexing (decouples from file order)
names(fecu_list) <- as.character(purrr::map_int(fecu_list, ~ as.integer( unique(.x$year) ) ))
names(eeff_list) <- as.character(purrr::map_int(eeff_list, ~ as.integer( unique(.x$year))))



fecu_names = list()
for (i in 1:length(fecu_list)) {
  fecu_names[[i]] <- names(fecu_list[[i]])
}


eeff_names = list()
for (i in 1:length(eeff_list)) {
  eeff_names[[i]] <- names(eeff_list[[i]])
}

# -----------------------------------------------------------------------------
# FECU DATA CLEANING
# -----------------------------------------------------------------------------


# Simplified names for fecu (based on names2 ordering)
new_names_fecu <- c(
  "id",
  "nombre",
  "aranceles",
  "aportes_basales",
  "ingresos_extension",
  "ingresos_servicios",
  "donaciones",
  "otros_ingresos",
  "ingresos_no_op",
  "remuneraciones",
  "gastos_adm_ventas",
  "otros_gastos_op",
  "gasto_no_op",
  "rem_academicos",
  "rem_directivos",
  "rem_administrativos",
  "otras_rem",
  "year"
)

# Rename maps: c("new_name" = "original_name"), keyed by year.
# 2018 is the only old-format file with a "tipo" col; 2019–2023 also carry tipo.
fecu_rename_maps <- list(

  # 2013 — ID_IES; "Aranceles" (no footnote); all-lowercase gastos/ingresos;
  #         Administrativos before Directivos
  "2013" = c("id"                  = "ID_IES",
             "nombre"              = "Nombre institución",
             "aranceles"           = "Aranceles",
             "aportes_basales"     = "Aportes basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros ingresos",
             "ingresos_no_op"      = "Ingresos no operacionales",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos de administración y ventas",
             "otros_gastos_op"     = "Otros gastos operacionales",
             "gasto_no_op"         = "Gastos no operacionales",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "rem_directivos"      = "Directivos",
             "otras_rem"           = "Otras remuneraciones"),

  # 2014 — ID_IES; "Aranceles (1)"; lowercase gastos with footnotes;
  #         singular Directivo/Académico/Administrativo
  "2014" = c("id"                  = "ID_IES",
             "nombre"              = "Nombre institución",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros ingresos",
             "ingresos_no_op"      = "Ingresos no operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos de administración y ventas (3)",
             "otros_gastos_op"     = "Otros gastos operacionales (4)",
             "gasto_no_op"         = "Gastos no operacionales (5)",
             "rem_directivos"      = "Directivo",
             "rem_academicos"      = "Académico",
             "rem_administrativos" = "Administrativo",
             "otras_rem"           = "Otras remuneraciones"),

  # 2015 — ID_IES; "Nombre Institución" (capital I); double-space aportes;
  #         title-case gastos without "de"; Académicos/Directivos/Administrativos
  "2015" = c("id"                  = "ID_IES",
             "nombre"              = "Nombre Institución",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_directivos"      = "Directivos",
             "rem_administrativos" = "Administrativos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2016 — ID (not ID_IES); "Prestación de servicios" (singular);
  #         "Gasto (beneficio) no Operacional (5)"; double-space aportes
  "2016" = c("id"                  = "ID",
             "nombre"              = "Nombre institución",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestación de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gasto (beneficio) no Operacional (5)",
             "rem_academicos"      = "Académicos",
             "rem_directivos"      = "Directivos",
             "rem_administrativos" = "Administrativos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2017 — ID_IES; "Nombre Institución" (capital I); double-space aportes;
  #         title-case gastos without "de"; Académicos/Directivos/Administrativos
  "2017" = c("id"                  = "ID_IES",
             "nombre"              = "Nombre Institución",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_directivos"      = "Directivos",
             "rem_administrativos" = "Administrativos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2018 — ID (not ID_IES); "Tipo de institución" (only old-format file with tipo);
  #         single-space aportes; Directivos before Académicos
  "2018" = c("id"                  = "ID",
             "nombre"              = "Nombre institución",
             "tipo"                = "Tipo de institución",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_directivos"      = "Directivos",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2019 — "Año" extra col; COD_IES/Nombre_IES/Tipo_IES_2;
  #         double-space aportes basales; Académicos/Directivos/Administrativos
  "2019" = c("anio_dato"           = "Año",
             "id"                  = "COD_IES",
             "nombre"              = "Nombre_IES",
             "tipo"                = "Tipo_IES_2",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  basales y fondos concursables",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_directivos"      = "Directivos",
             "rem_administrativos" = "Administrativos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2020 — COD_IES/Nombre_IES/Tipo_IES; "Aportes  fiscales" (double-space);
  #         Académicos/Administrativos/Directivos
  "2020" = c("id"                  = "COD_IES",
             "nombre"              = "Nombre_IES",
             "tipo"                = "Tipo_IES",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  fiscales",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "rem_directivos"      = "Directivos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2021 — merged-cell header: ...1/...2/...3; "Aportes  fiscales";
  #         Académicos/Administrativos/Directivos
  "2021" = c("id"                  = "...1",
             "nombre"              = "...2",
             "tipo"                = "...3",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  fiscales",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "rem_directivos"      = "Directivos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2022 — identical structure to 2021
  "2022" = c("id"                  = "...1",
             "nombre"              = "...2",
             "tipo"                = "...3",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes  fiscales",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no Operacionales (2)",
             "remuneraciones"      = "Remuneraciones",
             "gastos_adm_ventas"   = "Gastos Administración y Ventas (3)",
             "otros_gastos_op"     = "Otros Gastos Operacionales (4)",
             "gasto_no_op"         = "Gastos no Operacionales (5)",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "rem_directivos"      = "Directivos",
             "otras_rem"           = "Otras Remuneraciones"),

  # 2023 
  "2023" = c("id"                  = "...1",
             "nombre"              = "...2",
             "tipo"                = "...3",
             "aranceles"           = "Aranceles (1)",
             "aportes_basales"     = "Aportes fiscales",
             "ingresos_extension"  = "Ingresos de cursos y programas de extensión",
             "ingresos_servicios"  = "Prestaciones de servicios",
             "donaciones"          = "Donaciones",
             "otros_ingresos"      = "Otros Ingresos",
             "ingresos_no_op"      = "Ingresos no operacionales(2)",
             "remuneraciones"      = "Total Remuneraciones",
             "gastos_adm_ventas"   = "Gastos de administración (4)",
             "otros_gastos_op"     = "Otros gastos",
             "gasto_no_op"         = "Costos financieros",
             "rem_academicos"      = "Académicos",
             "rem_administrativos" = "Administrativos",
             "rem_directivos"      = "Directivos",
             "otras_rem"           = "Otras remuneraciones")
)



purrr::map(fecu_list, ~ purrr::map_chr(.x, class)) %>%
  purrr::map_dfr(~ as_tibble(as.list(.x)), .id = "year") %>%
  tidyr::pivot_longer(-year) %>%
  tidyr::pivot_wider(names_from = year, values_from = value) %>%
  filter(if_any(-name, ~ .x != `1`))   # show only rows where type differs across years


# Apply rename maps and bind into panel
panel_fecu <- purrr::imap_dfr(fecu_list, ~ {
  rename_map <- fecu_rename_maps[[.y]]
  if (is.null(rename_map))
    stop(sprintf("No rename map for year %s. Add an entry to fecu_rename_maps.", .y))
  .x %>%
    rename(any_of(rename_map)) %>%
    select(any_of(new_names_fecu)) %>%
    mutate(
      across(any_of("id"), as.character),
      across(-any_of(c("id", "nombre", "tipo", "year")), as.numeric)
    )
})




# -----------------------------------------------------------------------------
# EEFF DATA CLEANING
# -----------------------------------------------------------------------------


new_names_eeff <- c(
  "id",
  "nombre",
  "tipo",
  "naturaleza_juridica",
  "rut",
  "estados_financieros",
  "criterio_contabilizacion",  # "Criterio contabilizacion" / "Principio Contable" (absent in [[3]], [[4]])
  "acreditacion",              # only in [[2]], [[5]]
  "anos_acreditacion",         # only in [[2]], [[5]]
  "matricula_total",
  "matricula_1er_anio",
  "activo_corriente",
  "activo_no_corriente",
  "total_activos",
  "pasivo_corriente",
  "pasivo_no_corriente",
  "pasivo_total",              # explicit only in [[1]] ("Pasivo Total") and [[4]] ("Total Pasivos")
  "patrimonio",
  "total_patrimonio_pasivos",
  "ingresos_operacion",
  "costos_gastos_operacion",
  "resultado_operacion",
  "otras_ganancias_perdidas",
  "resultado_financiero",
  "resultado_ejercicio",
  "year"
)



eeff_rename_maps <- list(

  # 2013 — id_ies; "Principio Contable"; acreditacion with embedded spaces;
  #         "Matrícula 2013"; all-lowercase balance sheet; "Resultado financiero (4)" (lowercase f)
  "2013" = c("id"                       = "id_ies",
             "nombre"                   = "Nombre institución",
             "tipo"                     = "Tipo de institución",
             "naturaleza_juridica"      = "Naturaleza jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estado Financiero",
             "criterio_contabilizacion" = "Principio Contable",
             "acreditacion"             = "Acreditación           (a diciembre 2013)",
             "anos_acreditacion"        = "Años de acreditación",
             "matricula_total"          = "Matrícula 2013",
             "matricula_1er_anio"       = "Matrícula 1er año 2013",
             "activo_corriente"         = "Activo corriente",
             "activo_no_corriente"      = "Activo no corriente",
             "total_activos"            = "Total activos",
             "pasivo_corriente"         = "Pasivo corriente",
             "pasivo_no_corriente"      = "Pasivo no corriente",
             "patrimonio"               = "Patrimonio",
             "total_patrimonio_pasivos" = "Total Pasivo  y patrimonio",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado financiero (4)",
             "resultado_ejercicio"      = "Resultado del ejercicio"),

  # 2014 — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Principio Contable"; no acreditacion;
  #         "Matrícula_2014"; "Patrimonio (Total)"; "Total Pasivo  Y  Patrimonio"
  "2014" = c("id"                       = "id_ies",
             "nombre"                   = "Nom_IES_mifuturo",
             "tipo"                     = "tipo_ies_2",
             "naturaleza_juridica"      = "Naturaleza Jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estado Financiero",
             "criterio_contabilizacion" = "Principio Contable",
             "matricula_total"          = "Matrícula_2014",
             "matricula_1er_anio"       = "Matrícula_Nueva_2014",
             "activo_corriente"         = "Activo Corriente",
             "activo_no_corriente"      = "Activo no Corriente",
             "total_activos"            = "Total Activos",
             "pasivo_corriente"         = "Pasivo Corriente",
             "pasivo_no_corriente"      = "Pasivo no Corriente",
             "patrimonio"               = "Patrimonio (Total)",
             "total_patrimonio_pasivos" = "Total Pasivo  Y  Patrimonio",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado Financiero (4)",
             "resultado_ejercicio"      = "Resultado del Ejercicio"),

  # 2015 — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Principio Contable"; acreditacion "Abril de 2016";
  #         "Matrícula_2015"; "Patrimonio (Total)"; "Total Pasivo  Y  Patrimonio"
  "2015" = c("id"                       = "id_ies",
             "nombre"                   = "Nom_IES_mifuturo",
             "tipo"                     = "tipo_ies_2",
             "naturaleza_juridica"      = "Naturaleza Jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estado Financiero",
             "criterio_contabilizacion" = "Principio Contable",
             "acreditacion"             = "Acreditación (Abril de 2016)",
             "anos_acreditacion"        = "Años de Acreditación",
             "matricula_total"          = "Matrícula_2015",
             "matricula_1er_anio"       = "Matrícula_Nueva_2015",
             "activo_corriente"         = "Activo Corriente",
             "activo_no_corriente"      = "Activo no Corriente",
             "total_activos"            = "Total Activos",
             "pasivo_corriente"         = "Pasivo Corriente",
             "pasivo_no_corriente"      = "Pasivo no Corriente",
             "patrimonio"               = "Patrimonio (Total)",
             "total_patrimonio_pasivos" = "Total Pasivo  Y  Patrimonio",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado Financiero (4)",
             "resultado_ejercicio"      = "Resultado del Ejercicio"),

  # 2016 — ID; "Criterio contabilizacion"; "Estados Financieros" (plural); "Matrícula Total 2016";
  #         "Pasivo Total" explicit; "Total de Patrimonio y Pasivos"
  "2016" = c("id"                       = "ID",
             "nombre"                   = "Nombre institución",
             "tipo"                     = "Tipo de institución",
             "naturaleza_juridica"      = "Naturaleza jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estados Financieros",
             "criterio_contabilizacion" = "Criterio contabilizacion",
             "matricula_total"          = "Matrícula Total 2016",
             "matricula_1er_anio"       = "Matrícula 1er año 2016",
             "activo_corriente"         = "Activo Corriente",
             "activo_no_corriente"      = "Activo no Corriente",
             "total_activos"            = "Total Activos",
             "pasivo_corriente"         = "Pasivo Corriente",
             "pasivo_no_corriente"      = "Pasivo no Corriente",
             "pasivo_total"             = "Pasivo Total",
             "patrimonio"               = "Patrimonio",
             "total_patrimonio_pasivos" = "Total de Patrimonio y Pasivos",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado Financiero (4)",
             "resultado_ejercicio"      = "Resultado del Ejercicio"),

  # 2017 — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Estados Financieros" (plural); no criterio/acreditacion;
  #         "Matrícula_Total_2017"; "Total Pasivos" explicit; "Total de patrimonio y pasivos" (lowercase)
  "2017" = c("id"                       = "id_ies",
             "nombre"                   = "Nom_IES_mifuturo",
             "tipo"                     = "tipo_ies_2",
             "naturaleza_juridica"      = "Naturaleza Jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estados Financieros",
             "matricula_total"          = "Matrícula_Total_2017",
             "matricula_1er_anio"       = "Matrícula_Nueva_2017",
             "activo_corriente"         = "Activo Corriente",
             "activo_no_corriente"      = "Activo no Corriente",
             "total_activos"            = "Total Activos",
             "pasivo_corriente"         = "Pasivo Corriente",
             "pasivo_no_corriente"      = "Pasivo no Corriente",
             "pasivo_total"             = "Total Pasivos",
             "patrimonio"               = "Patrimonio",
             "total_patrimonio_pasivos" = "Total de patrimonio y pasivos",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado Financiero (4)",
             "resultado_ejercicio"      = "Resultado del Ejercicio"),

  # 2018 — ID; "Estados Financieros" (plural); no criterio/acreditacion;
  #         "Matrícula Pregrado 1er año 2018"; no pasivo_total; "Patrimonio (Total)"
  "2018" = c("id"                       = "ID",
             "nombre"                   = "Nombre institución",
             "tipo"                     = "Tipo de institución",
             "naturaleza_juridica"      = "Naturaleza Jurídica",
             "rut"                      = "RUT",
             "estados_financieros"      = "Estados Financieros",
             "matricula_total"          = "Matrícula Total 2018",
             "matricula_1er_anio"       = "Matrícula Pregrado 1er año 2018",
             "activo_corriente"         = "Activo Corriente",
             "activo_no_corriente"      = "Activo no Corriente",
             "total_activos"            = "Total Activos",
             "pasivo_corriente"         = "Pasivo Corriente",
             "pasivo_no_corriente"      = "Pasivo no Corriente",
             "patrimonio"               = "Patrimonio (Total)",
             "total_patrimonio_pasivos" = "Total Pasivo  Y  Patrimonio",
             "ingresos_operacion"       = "Ingresos de la operación (1)",
             "costos_gastos_operacion"  = "Costos y gastos de la operación (2)",
             "resultado_operacion"      = "Resultado de la operación",
             "otras_ganancias_perdidas" = "Otras ganancias o pérdidas (3)",
             "resultado_financiero"     = "Resultado Financiero (4)",
             "resultado_ejercicio"      = "Resultado del Ejercicio"),

  # 2019 — "Año"/ID_IES/IES/"Tipo IES"; three matricula cols → keep one;
  #         "Activos/Pasivos corrientes/no corrientes totales"; no total_patrimonio_pasivos;
  #         "Suma de Ganancia (pérdida)"
  "2019" = c("anio_dato"               = "Año",
             "id"                      = "ID_IES",
             "nombre"                  = "IES",
             "tipo"                    = "Tipo IES",
             "matricula_total"         = "Matricula total 2020",
             "activo_corriente"        = "Activos corrientes totales",
             "activo_no_corriente"     = "Activos no corrientes totales",
             "total_activos"           = "Activos totales",
             "pasivo_corriente"        = "Pasivos corrientes totales",
             "pasivo_no_corriente"     = "Pasivos no corrientes totales",
             "pasivo_total"            = "Pasivos totales",
             "patrimonio"              = "Patrimonio total",
             "ingresos_operacion"      = "Ingresos de la operación (1)",
             "costos_gastos_operacion" = "Costos y gastos de la operación (2)",
             "resultado_operacion"     = "Resultado de la operación",
             "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
             "resultado_financiero"    = "Resultado Financiero (4)",
             "resultado_ejercicio"     = "Suma de Ganancia (pérdida)"),

  # 2020 — COD_IES/Nombre_IES/Tipo_IES; no metadata cols; "Gastos de la Operación (2)";
  #         "Ingresos de la Operación (1)" (capital O); "Resultados (ganancia;pérdida)"
  "2020" = c("id"                      = "COD_IES",
             "nombre"                  = "Nombre_IES",
             "tipo"                    = "Tipo_IES",
             "activo_corriente"        = "Activos corrientes",
             "activo_no_corriente"     = "Activos no corrientes",
             "total_activos"           = "Total Activos",
             "pasivo_corriente"        = "Pasivos corrientes",
             "pasivo_no_corriente"     = "Pasivos no corrientes",
             "pasivo_total"            = "Total Pasivos",
             "patrimonio"              = "Patrimonio",
             "ingresos_operacion"      = "Ingresos de la Operación (1)",
             "costos_gastos_operacion" = "Gastos de la Operación (2)",
             "resultado_operacion"     = "Resultado de la operación",
             "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
             "resultado_financiero"    = "Resultado Financiero (4)",
             "resultado_ejercicio"     = "Resultados (ganancia;pérdida)"),

  # 2021 — merged-cell header: ...1/...2/...3; no metadata cols;
  #         "Costos y gastos de la operación (2)"; "Ganancia (pérdida)"
  "2021" = c("id"                      = "...1",
             "nombre"                  = "...2",
             "tipo"                    = "...3",
             "activo_corriente"        = "Activos corrientes",
             "activo_no_corriente"     = "Activos no corrientes",
             "total_activos"           = "Total Activos",
             "pasivo_corriente"        = "Pasivos corrientes",
             "pasivo_no_corriente"     = "Pasivos no corrientes",
             "pasivo_total"            = "Total Pasivos",
             "patrimonio"              = "Patrimonio",
             "ingresos_operacion"      = "Ingresos de la operación (1)",
             "costos_gastos_operacion" = "Costos y gastos de la operación (2)",
             "resultado_operacion"     = "Resultado de la operación",
             "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
             "resultado_financiero"    = "Resultado Financiero (4)",
             "resultado_ejercicio"     = "Ganancia (pérdida)"),

  # 2022 — merged-cell header: ...1/...2/...3; no metadata cols;
  #         no footnote suffixes on income-statement cols; "Ganancia (pérdida)"
  "2022" = c("id"                      = "...1",
             "nombre"                  = "...2",
             "tipo"                    = "...3",
             "activo_corriente"        = "Activos corrientes",
             "activo_no_corriente"     = "Activos no corrientes",
             "total_activos"           = "Total Activos",
             "pasivo_corriente"        = "Pasivos corrientes",
             "pasivo_no_corriente"     = "Pasivos no corrientes",
             "pasivo_total"            = "Total Pasivos",
             "patrimonio"              = "Patrimonio",
             "ingresos_operacion"      = "Ingresos de la operación",
             "costos_gastos_operacion" = "Costos y gastos de la operación",
             "resultado_operacion"     = "Resultado de la operación",
             "otras_ganancias_perdidas"= "Otras ganancias o pérdidas",
             "resultado_financiero"    = "Resultado Financiero",
             "resultado_ejercicio"     = "Ganancia (pérdida)"),

  # 2023 — identical structure to 2022
  "2023" = c("id"                      = "...1",
             "nombre"                  = "...2",
             "tipo"                    = "...3",
             "activo_corriente"        = "Activos corrientes",
             "activo_no_corriente"     = "Activos no corrientes",
             "total_activos"           = "Total Activos",
             "pasivo_corriente"        = "Pasivos corrientes",
             "pasivo_no_corriente"     = "Pasivos no corrientes",
             "pasivo_total"            = "Total Pasivos",
             "patrimonio"              = "Patrimonio",
             "ingresos_operacion"      = "Ingresos de la operación",
             "costos_gastos_operacion" = "Costos y gastos de la operación",
             "resultado_operacion"     = "Resultado de la operación",
             "otras_ganancias_perdidas"= "Otras ganancias o pérdidas",
             "resultado_financiero"    = "Resultado Financiero",
             "resultado_ejercicio"     = "Ganancia (pérdida)")
)


# Apply rename maps and bind into panel
panel_eeff <- purrr::imap_dfr(eeff_list, ~ {
  rename_map <- eeff_rename_maps[[.y]]
  if (is.null(rename_map))
    stop(sprintf("No rename map for year %s. Add an entry to eeff_rename_maps.", .y))
  .x %>%
    rename(any_of(rename_map)) %>%
    select(any_of(new_names_eeff)) %>%
    mutate(
      across(any_of("id"), as.character),
      across(-any_of(c("id", "nombre", "tipo", "naturaleza_juridica",
                       "rut", "estados_financieros", "criterio_contabilizacion",
                       "acreditacion", "year")), as.numeric)
    )
})











# -----------------------------------------------------------------------------
# UNIVERSITY NAME CROSSWALK
# -----------------------------------------------------------------------------
# Maps every raw name variant to a single canonical form.
# Canonical convention: lowercase, no diacritics, no trailing acronym/qualifier
# (consistent with the nombre2 column used in the main pipeline).

univ_crosswalk <- tibble(
  nombre_raw = c(
    "universidad gabriela mistral",
    "universidad finis terrae",
    "universidad diego portales",
    "universidad central de chile",
    "universidad bolivariana",
    "universidad pedro de valdivia",
    "universidad mayor",
    "universidad academia de humanismo cristiano",
    "universidad santo tomas",
    "universidad la republica",
    "universidad sek",                                                    # → internacional sek
    "universidad de las americas",
    "universidad andres bello",
    "universidad de vina del mar",                                   # ñ
    "universidad adolfo ibanez",                               # á + ñ
    "universidad iberoamericana de ciencias y tecnologia unicit",         # → without acronym
    "universidad de artes, ciencias y comunicacion uniacc",               # → without acronym
    "universidad del mar",
    "universidad ucinf",
    "universidad autonoma de chile",
    "universidad de los andes",
    "universidad adventista de chile",
    "universidad san sebastian",
    "universidad catolica cardenal silva henriquez",
	"universidad catolica cardenal raul silva henriquez",
    "universidad del desarrollo",
    "universidad del pacifico",
    "universidad los leones",
    "universidad bernardo o'higgins",
    "universidad tecnologica de chile inacap",
    "universidad miguel de cervantes",
    "universidad alberto hurtado",
    "universidad de chile",
    "universidad de santiago de chile",
    "universidad de valparaiso",
    "universidad de antofagasta",
    "universidad de la serena",
    "universidad del bio-bio",
    "universidad de la frontera",
    "universidad de magallanes",
    "universidad de talca",
    "universidad de atacama",
    "universidad de tarapaca",
    "universidad arturo prat",
    "universidad metropolitana de ciencias de la educacion",
    "universidad de playa ancha de ciencias de la educacion",
    "universidad de los lagos",
    "universidad tecnologica metropolitana",
    "pontificia universidad catolica de chile",
    "universidad de concepcion",
    "universidad tecnica federico santa maria",
    "pontificia universidad catolica de valparaiso",
    "universidad austral de chile",
    "universidad catolica del norte",
    "universidad catolica del maule",
    "universidad catolica de la santisima concepcion",
    "universidad catolica de temuco",
    "universidad chileno britanica de cultura",
    "universidad de aysen",
    "universidad de arte y ciencias sociales arcis",
    "universidad de aconcagua",
    "universidad internacional sek",                                      # duplicate of #11
    "universidad nacional andres bello",                                  # duplicate of #13
    "universidad de artes, ciencias y comunicacion (uniacc)",             # duplicate of #17
    "universidad de o'higgins",
    "universidad de o`higgins",                                           # backtick variant of #64
    "universidad la araucana",
    "universidad iberoamericana de ciencias y tecnologia (unicit)",       # duplicate of #16
    "universidad los leones (ex universidad maritima)",                   # duplicate of #27
    NA_character_
  ),
  nombre_canon = c(
    "universidad gabriela mistral",
    "universidad finis terrae",
    "universidad diego portales",
    "universidad central de chile",
    "universidad bolivariana",
    "universidad pedro de valdivia",
    "universidad mayor",
    "universidad academia de humanismo cristiano",
    "universidad santo tomas",
    "universidad la republica",
    "universidad internacional sek",
    "universidad de las americas",
    "universidad andres bello",
    "universidad de vina del mar",
    "universidad adolfo ibanez",
    "universidad iberoamericana de ciencias y tecnologia",
    "universidad de artes, ciencias y comunicacion",
    "universidad del mar",
    "universidad ucinf",
    "universidad autonoma de chile",
    "universidad de los andes",
    "universidad adventista de chile",
    "universidad san sebastian",
    "universidad catolica cardenal silva henriquez",
	"universidad catolica cardenal silva henriquez",
    "universidad del desarrollo",
    "universidad del pacifico",
    "universidad los leones",
    "universidad bernardo o'higgins",
    "universidad tecnologica de chile inacap",
    "universidad miguel de cervantes",
    "universidad alberto hurtado",
    "universidad de chile",
    "universidad de santiago de chile",
    "universidad de valparaiso",
    "universidad de antofagasta",
    "universidad de la serena",
    "universidad del bio-bio",
    "universidad de la frontera",
    "universidad de magallanes",
    "universidad de talca",
    "universidad de atacama",
    "universidad de tarapaca",
    "universidad arturo prat",
    "universidad metropolitana de ciencias de la educacion",
    "universidad de playa ancha de ciencias de la educacion",
    "universidad de los lagos",
    "universidad tecnologica metropolitana",
    "pontificia universidad catolica de chile",
    "universidad de concepcion",
    "universidad tecnica federico santa maria",
    "pontificia universidad catolica de valparaiso",
    "universidad austral de chile",
    "universidad catolica del norte",
    "universidad catolica del maule",
    "universidad catolica de la santisima concepcion",
    "universidad catolica de temuco",
    "universidad chileno britanica de cultura",
    "universidad de aysen",
    "universidad de arte y ciencias sociales arcis",
    "universidad de aconcagua",
    "universidad internacional sek",
    "universidad andres bello",
    "universidad de artes, ciencias y comunicacion",
    "universidad de o'higgins",
    "universidad de o'higgins",
    "universidad la araucana",
    "universidad iberoamericana de ciencias y tecnologia",
    "universidad los leones",
    NA_character_
  )
)












# -----------------------------------------------------------------------------
# GRATUIDAD
# -----------------------------------------------------------------------------




gratuidad_aux <- tibble(
  nombre_canon = c(
    "universidad gabriela mistral",
    "universidad finis terrae",
    "universidad diego portales",
    "universidad central de chile",
    "universidad bolivariana",
    "universidad pedro de valdivia",
    "universidad mayor",
    "universidad academia de humanismo cristiano",
    "universidad santo tomas",
    "universidad la republica",
    "universidad internacional sek",
    "universidad de las americas",
    "universidad andres bello",
    "universidad de vina del mar",
    "universidad adolfo ibanez",
    "universidad iberoamericana de ciencias y tecnologia",
    "universidad de artes, ciencias y comunicacion",
    "universidad del mar",
    "universidad ucinf",
    "universidad autonoma de chile",
    "universidad de los andes",
    "universidad adventista de chile",
    "universidad san sebastian",
    "universidad catolica cardenal silva henriquez",
    "universidad del desarrollo",
    "universidad del pacifico",
    "universidad los leones",
    "universidad bernardo o'higgins",
    "universidad tecnologica de chile inacap",
    "universidad miguel de cervantes",
    "universidad alberto hurtado",
    "universidad de chile",
    "universidad de santiago de chile",
    "universidad de valparaiso",
    "universidad de antofagasta",
    "universidad de la serena",
    "universidad del bio-bio",
    "universidad de la frontera",
    "universidad de magallanes",
    "universidad de talca",
    "universidad de atacama",
    "universidad de tarapaca",
    "universidad arturo prat",
    "universidad metropolitana de ciencias de la educacion",
    "universidad de playa ancha de ciencias de la educacion",
    "universidad de los lagos",
    "universidad tecnologica metropolitana",
    "pontificia universidad catolica de chile",
    "universidad de concepcion",
    "universidad tecnica federico santa maria",
    "pontificia universidad catolica de valparaiso",
    "universidad austral de chile",
    "universidad catolica del norte",
    "universidad catolica del maule",
    "universidad catolica de la santisima concepcion",
    "universidad catolica de temuco",
    "universidad chileno britanica de cultura",
    "universidad de aysen",
    "universidad de arte y ciencias sociales arcis",
    "universidad de aconcagua",
    "universidad de o'higgins",
    "universidad la araucana"
  ),
  
  gratuidad_year = c(
    NA_integer_,
    2016L,
    2016L,
    2023L,
    NA_integer_,
    NA_integer_,
    2021L,
    2016L,
    2022L,
    NA_integer_,
    NA_integer_,
    2024L,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    2016L,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    2016L,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    2019L,
    NA_integer_,
    NA_integer_,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    2016L,
    NA_integer_,
    2017L,
    NA_integer_,
    NA_integer_,
    2017L,
    NA_integer_
  )
)















# -----------------------------------------------------------------------------
#  PREPARE DATA
# -----------------------------------------------------------------------------

    tibble <-
        panel_fecu %>%
        # keep only universities
        filter( str_detect(nombre, regex("universidad", ignore_case = TRUE)) ) %>%
    
        # eliminating * and ** from names, also " * " in some cases (e.g. "Matrícula Total 2016 *") to ensure proper merging
        mutate( nombre = str_replace_all(nombre, "\\s*\\*+\\s*", "") ) %>%
    
        # merge balance sheet and income statement data
        full_join( panel_eeff %>%
                    filter( str_detect(nombre, regex("universidad", ignore_case = TRUE)) ) %>%
                    mutate( nombre = str_replace_all(nombre, "\\s*\\*+\\s*", "") ) %>%
                    select( -nombre ),
                 by = c("id", "year") ) %>%

        # Unifying names
        # eliminating tildes vowels, everyting to lowercase
        mutate( nombre_raw = str_to_lower( nombre ) %>%
                            str_replace_all("á", "a") %>%
                            str_replace_all("é", "e") %>%
                            str_replace_all("í", "i") %>%
                            str_replace_all("ó", "o") %>%
                            str_replace_all("ú", "u")  %>%
							str_replace_all("ñ", "n") ) %>%
        left_join( univ_crosswalk, by = c("nombre_raw") ) %>%
        rename( nombre = nombre_canon, nombre_full = nombre ) %>%
        select( -nombre_raw )  %>%
		drop_na( nombre ) %>%

        # Tipo extended for all years based on 2019 classification 
        group_by( nombre ) %>%
        rename( tipo_old = tipo ) %>%
		# only for unis with data in 2019, to avoid misclassification of unis with missing data in that year
		mutate( tipo = tipo_old[match(2019, year)] ) %>%
        ungroup() %>%

        # Importing price deflator to 2016 CLP, and adjusting nominal variables
        left_join( deflator %>% transmute( year, deflator_2016 = level ), by = "year" ) %>%
        mutate( across( c(matricula_total, matricula_1er_anio, activo_corriente, activo_no_corriente,
                           total_activos, pasivo_corriente, pasivo_no_corriente, pasivo_total,
                           patrimonio, total_patrimonio_pasivos, ingresos_operacion,
                           costos_gastos_operacion, resultado_operacion, otras_ganancias_perdidas,
                           resultado_financiero, resultado_ejercicio),
                       ~ .x / deflator_2016 ) ) %>%

        # GRATUIDAD, by institution
        # Gratuidad is a panel, as universities applied for admission at different moments in time
        left_join( gratuidad_aux, by = c( "nombre" = "nombre_canon" ) ) %>%
        mutate( gratuidad_ind = if_else( !is.na(gratuidad_year) & year >= gratuidad_year, 1L, 0L ),
                gratuidad_ever = if_else( !is.na(gratuidad_year), 1L, 0L ) ) %>%

        # ACREDITACION: create groups based on years of accreditation, based on year 2015
        # basica (3 years), intermedia (4-5 years), avanzada (6-7 years)
        group_by( nombre ) %>%
        mutate( acreditacion2015 = anos_acreditacion[match(2015, year)] ) %>%
        mutate( acreditacion_cat = case_when(
                  !is.na(acreditacion2015) & acreditacion2015 <= 3 ~ "basica",
                  !is.na(acreditacion2015) & acreditacion2015 >= 4 & acreditacion2015 <= 5 ~ "intermedia",
                  !is.na(acreditacion2015) & acreditacion2015 >= 6 & acreditacion2015 <= 7 ~ "avanzada",
                  TRUE ~ NA_character_
                ) ) %>%
        ungroup() %>%

        # MERGING WITH SIES DATA ON MATRICULA, ACADEMICOS
        select( - matricula_total ) %>% # we will use matricula from the matricula dataset
        left_join( matricula_panel, by = c('id' = 'code', 'year' = 'ano') ) 


    # Table with university type
    summaryTable <- 
		tibble %>% 
		group_by( nombre ) %>%
        summarize( nYears = n(), tipo = first(tipo), 
                    acreditacion_cat = first(acreditacion_cat), 
                    gratuidad = first(gratuidad_ever),
                    gratuidad_year = first(gratuidad_year), .groups = "drop" )
	
	# troublesome unis
	summaryTable %>% filter( nYears < 10 )

    tibble %>% filter( str_detect(nombre, "santiago") ) %>% select( nombre, year, aranceles, matricula_total, aportes_basales)

   


