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
        bind_rows( tibble( files = files_new, year = unlist(years_new) ) %>% arrange( year ) )
  

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
    df2 <- read_excel(f, sheet = 2)
    if (needs_skip(df2)) df2 <- read_excel(f, sheet = 2, skip = 1)
    fecu_list[[i]] <- df2 %>% mutate( year = year)
  }




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

# Rename maps: c("new_name" = "original_name"), one entry per file in chronological order.
# [[6]] is the only old file with a "tipo" col; [[7]]–[[10]] also carry tipo.
fecu_rename_maps <- list(

  # [[1]] — ID_IES; "Aranceles" (no footnote); all-lowercase gastos/ingresos;
  #         Administrativos before Directivos
  c("id"                  = "ID_IES",
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

  # [[2]] — ID_IES; "Aranceles (1)"; lowercase gastos with footnotes;
  #         singular Directivo/Académico/Administrativo
  c("id"                  = "ID_IES",
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

  # [[3]] — ID_IES; "Nombre Institución" (capital I); double-space aportes;
  #         title-case gastos without "de"; Académicos/Directivos/Administrativos
  c("id"                  = "ID_IES",
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

  # [[4]] — ID (not ID_IES); "Prestación de servicios" (singular);
  #         "Gasto (beneficio) no Operacional (5)"; double-space aportes
  c("id"                  = "ID",
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

  # [[5]] — ID_IES; "Nombre Institución" (capital I); double-space aportes;
  #         title-case gastos without "de"; Académicos/Directivos/Administrativos
  c("id"                  = "ID_IES",
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

  # [[6]] — ID (not ID_IES); "Tipo de institución" (only old file with tipo);
  #         single-space aportes; Directivos before Académicos
  c("id"                  = "ID",
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

  # [[7]] year 2020 — "Año" extra col; COD_IES/Nombre_IES/Tipo_IES_2;
  #         double-space aportes basales; Académicos/Directivos/Administrativos
  c("anio_dato"           = "Año",
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

  # [[8]] year 2021 — COD_IES/Nombre_IES/Tipo_IES; "Aportes  fiscales" (double-space);
  #         Académicos/Administrativos/Directivos
  c("id"                  = "COD_IES",
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

  # [[9]] year 2022 — merged-cell header: ...1/...2/...3; "Aportes  fiscales";
  #         Académicos/Administrativos/Directivos
  c("id"                  = "...1",
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

  # [[10]] year 2023 — identical structure to [[9]]
  c("id"                  = "...1",
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
    "otras_rem"           = "Otras Remuneraciones")
)



purrr::map(fecu_list, ~ purrr::map_chr(.x, class)) %>%
  purrr::map_dfr(~ as_tibble(as.list(.x)), .id = "year") %>%
  tidyr::pivot_longer(-year) %>%
  tidyr::pivot_wider(names_from = year, values_from = value) %>%
  filter(if_any(-name, ~ .x != `1`))   # show only rows where type differs across years


# Apply rename maps and bind into panel
panel_fecu <- purrr::imap_dfr(fecu_list, ~ {
  rename_map <- fecu_rename_maps[[.y]]
  .x %>%
    rename(any_of(rename_map)) %>%
    select(any_of(new_names_fecu)) %>%
    mutate(
      across(any_of("id"), as.character),
      across(-any_of(c("id", "nombre", "tipo", "year")), as.numeric)
    )
})



# Rename columns in each data frame using the corresponding rename map
fecu_list[[1]] %>% rename( any_of( fecu_rename_maps[[1]] ) ) %>% names()

length( fecu_rename_maps[[1]] )
fecu_list[[1]] %>% ncol()



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

  # [[1]] — id_ies; "Principio Contable"; acreditacion with embedded spaces;
  #         "Matrícula 2013"; all-lowercase balance sheet; "Resultado financiero (4)" (lowercase f)
  c("id"                       = "id_ies",
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

  # [[2]] — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Principio Contable"; no acreditacion;
  #         "Matrícula_2014"; "Patrimonio (Total)"; "Total Pasivo  Y  Patrimonio"
  c("id"                       = "id_ies",
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

  # [[3]] — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Principio Contable"; acreditacion "Abril de 2016";
  #         "Matrícula_2015"; "Patrimonio (Total)"; "Total Pasivo  Y  Patrimonio"
  c("id"                       = "id_ies",
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

  # [[4]] — ID; "Criterio contabilizacion"; "Estados Financieros" (plural); "Matrícula Total 2016";
  #         "Pasivo Total" explicit; "Total de Patrimonio y Pasivos"
  c("id"                       = "ID",
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

  # [[5]] — id_ies/Nom_IES_mifuturo/tipo_ies_2; "Estados Financieros" (plural); no criterio/acreditacion;
  #         "Matrícula_Total_2017"; "Total Pasivos" explicit; "Total de patrimonio y pasivos" (lowercase)
  c("id"                       = "id_ies",
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

  # [[6]] — ID; "Estados Financieros" (plural); no criterio/acreditacion;
  #         "Matrícula Pregrado 1er año 2018"; no pasivo_total; "Patrimonio (Total)"
  c("id"                       = "ID",
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

  # [[7]] year 2020 — "Año"/ID_IES/IES/"Tipo IES"; three matricula cols → keep 2020;
  #                   "Activos/Pasivos corrientes/no corrientes totales"; no total_patrimonio_pasivos;
  #                   "Suma de Ganancia (pérdida)"
  c("anio_dato"              = "Año",
    "id"                     = "ID_IES",
    "nombre"                 = "IES",
    "tipo"                   = "Tipo IES",
    "matricula_total"        = "Matricula total 2020",
    "activo_corriente"       = "Activos corrientes totales",
    "activo_no_corriente"    = "Activos no corrientes totales",
    "total_activos"          = "Activos totales",
    "pasivo_corriente"       = "Pasivos corrientes totales",
    "pasivo_no_corriente"    = "Pasivos no corrientes totales",
    "pasivo_total"           = "Pasivos totales",
    "patrimonio"             = "Patrimonio total",
    "ingresos_operacion"     = "Ingresos de la operación (1)",
    "costos_gastos_operacion"= "Costos y gastos de la operación (2)",
    "resultado_operacion"    = "Resultado de la operación",
    "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
    "resultado_financiero"   = "Resultado Financiero (4)",
    "resultado_ejercicio"    = "Suma de Ganancia (pérdida)"),

  # [[8]] year 2021 — COD_IES/Nombre_IES/Tipo_IES; no metadata cols; "Gastos de la Operación (2)";
  #                   "Ingresos de la Operación (1)" (capital O); "Resultados (ganancia;pérdida)"
  c("id"                     = "COD_IES",
    "nombre"                 = "Nombre_IES",
    "tipo"                   = "Tipo_IES",
    "activo_corriente"       = "Activos corrientes",
    "activo_no_corriente"    = "Activos no corrientes",
    "total_activos"          = "Total Activos",
    "pasivo_corriente"       = "Pasivos corrientes",
    "pasivo_no_corriente"    = "Pasivos no corrientes",
    "pasivo_total"           = "Total Pasivos",
    "patrimonio"             = "Patrimonio",
    "ingresos_operacion"     = "Ingresos de la Operación (1)",
    "costos_gastos_operacion"= "Gastos de la Operación (2)",
    "resultado_operacion"    = "Resultado de la operación",
    "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
    "resultado_financiero"   = "Resultado Financiero (4)",
    "resultado_ejercicio"    = "Resultados (ganancia;pérdida)"),

  # [[9]] year 2022 — merged-cell header: ...1/...2/...3; no metadata cols;
  #                   "Costos y gastos de la operación (2)"; "Ganancia (pérdida)"
  c("id"                     = "...1",
    "nombre"                 = "...2",
    "tipo"                   = "...3",
    "activo_corriente"       = "Activos corrientes",
    "activo_no_corriente"    = "Activos no corrientes",
    "total_activos"          = "Total Activos",
    "pasivo_corriente"       = "Pasivos corrientes",
    "pasivo_no_corriente"    = "Pasivos no corrientes",
    "pasivo_total"           = "Total Pasivos",
    "patrimonio"             = "Patrimonio",
    "ingresos_operacion"     = "Ingresos de la operación (1)",
    "costos_gastos_operacion"= "Costos y gastos de la operación (2)",
    "resultado_operacion"    = "Resultado de la operación",
    "otras_ganancias_perdidas"= "Otras ganancias o pérdidas (3)",
    "resultado_financiero"   = "Resultado Financiero (4)",
    "resultado_ejercicio"    = "Ganancia (pérdida)"),

  # [[10]] year 2023 — merged-cell header: ...1/...2/...3; no metadata cols;
  #                    no footnote suffixes on income-statement cols; "Ganancia (pérdida)"
  c("id"                     = "...1",
    "nombre"                 = "...2",
    "tipo"                   = "...3",
    "activo_corriente"       = "Activos corrientes",
    "activo_no_corriente"    = "Activos no corrientes",
    "total_activos"          = "Total Activos",
    "pasivo_corriente"       = "Pasivos corrientes",
    "pasivo_no_corriente"    = "Pasivos no corrientes",
    "pasivo_total"           = "Total Pasivos",
    "patrimonio"             = "Patrimonio",
    "ingresos_operacion"     = "Ingresos de la operación",
    "costos_gastos_operacion"= "Costos y gastos de la operación",
    "resultado_operacion"    = "Resultado de la operación",
    "otras_ganancias_perdidas"= "Otras ganancias o pérdidas",
    "resultado_financiero"   = "Resultado Financiero",
    "resultado_ejercicio"    = "Ganancia (pérdida)")
)


# Apply rename maps and bind into panel
panel_eeff <- purrr::imap_dfr(eeff_list, ~ {
  rename_map <- eeff_rename_maps[[.y]]
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
# 4. EXPLORE DATA
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
                       ~ .x / deflator_2016 ) ) 



		# Gratuidad is a panel, as universities applied for admission at different moments in time
        # # GRATUIDAD, by institution
        # mutate( gratuidad = if_else( tipo != 'Universidad Privada', 1, 0 ) ) %>%
        # # These institutions joined in year 2016
        # mutate( gratuidad = if_else( nombre %in% c("universidad alberto hurtado",
        #                                          "universidad diego portales",
        #                                          "universidad autonoma de chile",
        #                                          "universidad cardenal silva henriquez",
        #                                          "universidad finis terrae"), 1, gratuidad ) ) 

        


    # Table with university type
    summaryTable <- 
		tibble %>% 
		group_by( nombre ) %>%
        summarize( nYears = n(), tipo = first(tipo), .groups = "drop" ) 
	
	# troublesome unis
	summaryTable %>% filter( nYears < 10 )

	tibble %>% filter( str_detect(nombre, "santiago") ) %>% select( nombre, year, aranceles, matricula_total, aportes_basales)



	# PLOTS that I want to see, in order:
	# BY TYPE OF UNIVERSITY
	# ingreso_alumno = (aranceles + aportes_basales) / matricula_total, simple average
	# ingreso_alumno = (aranceles + aportes_basales) / matricula_total, weighted by matricula_total
	# arancel_alumno = aranceles / matricula_total, simple average
	# arancel_alumno = aranceles / matricula_total, weighted by matricula_total
	# aporte_alumno = aportes_basales / matricula_total, simple average
	# aporte_alumno = aportes_basales / matricula_total, weighted by matricula_total
	# docencia_alumno = rem_academicos / matricula_total, simple average
	# docencia_alumno = rem_academicos / matricula_total, weighted by matricula_total
    # patrimonio_alumno = patrimonio / matricula_total, simple average
    # patrimonio_alumno = patrimonio / matricula_total, weighted by matricula_total


	# Helper: base data filtered to obs with valid tipo and matricula_total
	base <- tibble %>%
		filter( !is.na(tipo), !is.na(matricula_total), matricula_total > 0 )

	# ── 1. ingreso_alumno — simple average ──────────────────────────────────────
	plot1 <- base %>%
		mutate( ingreso_alumno = (aranceles + aportes_basales) / matricula_total ) %>%
		group_by( tipo, year ) %>%
		summarize( ingreso_alumno = mean( ingreso_alumno, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ingreso_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ingresos por alumno (promedio simple)",
			  subtitle = "(Aranceles + Aportes basales) / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 2. ingreso_alumno — weighted average ────────────────────────────────────
	plot2 <- base %>%
		filter( !is.na(aranceles), !is.na(aportes_basales) ) %>%
		group_by( tipo, year ) %>%
		summarize( ingreso_alumno = sum( aranceles + aportes_basales, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ingreso_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ingresos por alumno (ponderado por matrícula)",
			  subtitle = "(Aranceles + Aportes basales) / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 3. arancel_alumno — simple average ──────────────────────────────────────
	plot3 <- base %>%
		mutate( arancel_alumno = aranceles / matricula_total ) %>%
		group_by( tipo, year ) %>%
		summarize( arancel_alumno = mean( arancel_alumno, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = arancel_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aranceles por alumno (promedio simple)",
			  subtitle = "Aranceles / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 4. arancel_alumno — weighted average ────────────────────────────────────
	plot4 <- base %>%
		filter( !is.na(aranceles) ) %>%
		group_by( tipo, year ) %>%
		summarize( arancel_alumno = sum( aranceles, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = arancel_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aranceles por alumno (ponderado por matrícula)",
			  subtitle = "Aranceles / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 5. aporte_alumno — simple average ───────────────────────────────────────
	plot5 <- base %>%
		mutate( aporte_alumno = aportes_basales / matricula_total ) %>%
		group_by( tipo, year ) %>%
		summarize( aporte_alumno = mean( aporte_alumno, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = aporte_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aportes basales por alumno (promedio simple)",
			  subtitle = "Aportes basales / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 6. aporte_alumno — weighted average ─────────────────────────────────────
	plot6 <- base %>%
		filter( !is.na(aportes_basales) ) %>%
		group_by( tipo, year ) %>%
		summarize( aporte_alumno = sum( aportes_basales, na.rm = TRUE ) /
								  sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = aporte_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aportes basales por alumno (ponderado por matrícula)",
			  subtitle = "Aportes basales / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 7. docencia_alumno — simple average ─────────────────────────────────────
	plot7 <- base %>%
		mutate( docencia_alumno = rem_academicos / matricula_total ) %>%
		group_by( tipo, year ) %>%
		summarize( docencia_alumno = mean( docencia_alumno, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = docencia_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Remuneraciones académicas por alumno (promedio simple)",
			  subtitle = "Rem. académicos / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 8. docencia_alumno — weighted average ───────────────────────────────────
	plot8 <- base %>%
		filter( !is.na(rem_academicos) ) %>%
		group_by( tipo, year ) %>%
		summarize( docencia_alumno = sum( rem_academicos, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = docencia_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Remuneraciones académicas por alumno (ponderado por matrícula)",
			  subtitle = "Rem. académicos / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 9. patrimonio_alumno — simple average ───────────────────────────────────
	plot9 <- base %>%
		mutate( patrimonio_alumno = patrimonio / matricula_total ) %>%
		group_by( tipo, year ) %>%
		summarize( patrimonio_alumno = mean( patrimonio_alumno, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = patrimonio_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Patrimonio por alumno (promedio simple)",
			  subtitle = "Patrimonio / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── 10. patrimonio_alumno — weighted average ─────────────────────────────────
	plot10 <- base %>%
		filter( !is.na(patrimonio) ) %>%
		group_by( tipo, year ) %>%
		summarize( patrimonio_alumno = sum( patrimonio, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = patrimonio_alumno, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Patrimonio por alumno (ponderado por matrícula)",
			  subtitle = "Patrimonio / Matrícula total",
			  x = "Año", y = "CLP", color = "Tipo" ) +
		theme_minimal()

	# ── Display all 10 plots ────────────────────────────────────────────────────
	print(plot1)
	print(plot2)
	print(plot3)
	print(plot4)
	print(plot5)
	print(plot6)
	print(plot7)
	print(plot8)
	print(plot9)
	print(plot10)