
# PLOTS that I want to see, in order:
	# BY TYPE OF UNIVERSITY:
        # matricula_total, simple average
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
        # ratio alumno/jec = matricula_total / jec_total, simple average
        # ratio alumno/jec = matricula_total / jec_total, weighted by matricula_total
        # % jec con doctorado, simple average = jec_doctor / jec_total
        # % jec con doctorado, weighted by matricula_total = sum(jec_doctor) / sum(jec_total)

    # ADOPTED GRATUIDAD IN 2016 VERSUS NEVER
        # matricula_total, simple average
        # ingreso_alumno = (aranceles + aportes_basales) / matricula_total, weighted by matricula_total
        # arancel_alumno = aranceles / matricula_total, weighted by matricula_total
        # aporte_alumno = aportes_basales / matricula_total, weighted by matricula_total
        # docencia_alumno = rem_academicos / matricula_total, weighted by matricula_total
        # patrimonio_alumno = patrimonio / matricula_total, weighted by matricula_total
        # ratio alumno/jec = matricula_total / jec_total, weighted by matricula_total
        # % jec con doctorado, weighted by matricula_total = sum(jec_doctor) / sum(jec_total)


     # ADOPTED GRATUIDAD IN 2016 VERSUS NEVER, ONLY PRIVATE INSTITUTIONS
        # matricula_total, simple average
        # ingreso_alumno = (aranceles + aportes_basales) / matricula_total, weighted by matricula_total
        # arancel_alumno = aranceles / matricula_total, weighted by matricula_total
        # aporte_alumno = aportes_basales / matricula_total, weighted by matricula_total
        # docencia_alumno = rem_academicos / matricula_total, weighted by matricula_total
        # patrimonio_alumno = patrimonio / matricula_total, weighted by matricula_total
        # ratio alumno/jec = matricula_total / jec_total, weighted by matricula_total
        # % jec con doctorado, weighted by matricula_total = sum(jec_doctor) / sum(jec_total)

# =============================================================================
# SECTION 1: BY TYPE OF UNIVERSITY
# =============================================================================

	# Helper: base data filtered to obs with valid tipo and matricula_total
	base <- tibble %>%
		filter( !is.na(tipo), !is.na(matricula_total), matricula_total > 0 ) %>%
        # only high quality institutions
        filter( acreditacion_cat %in% c("intermedia", "avanzada") )

	# ── s1_plot0. matricula_total — simple average ───────────────────────────────
	s1_plot0 <- base %>%
		group_by( tipo, year ) %>%
		summarize( matricula_total = mean( matricula_total, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = matricula_total, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Matrícula total (promedio simple)",
			  x = "Año", y = "Estudiantes", color = "Tipo" ) +
		theme_minimal()

	# ── s1_plot1. ingreso_alumno — simple average ────────────────────────────────
	s1_plot1 <- base %>%
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

	# ── s1_plot2. ingreso_alumno — weighted average ──────────────────────────────
	s1_plot2 <- base %>%
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

	# ── s1_plot3. arancel_alumno — simple average ────────────────────────────────
	s1_plot3 <- base %>%
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

	# ── s1_plot4. arancel_alumno — weighted average ──────────────────────────────
	s1_plot4 <- base %>%
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

	# ── s1_plot5. aporte_alumno — simple average ─────────────────────────────────
	s1_plot5 <- base %>%
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

	# ── s1_plot6. aporte_alumno — weighted average ───────────────────────────────
	s1_plot6 <- base %>%
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

	# ── s1_plot7. docencia_alumno — simple average ───────────────────────────────
	s1_plot7 <- base %>%
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

	# ── s1_plot8. docencia_alumno — weighted average ─────────────────────────────
	s1_plot8 <- base %>%
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

	# ── s1_plot9. patrimonio_alumno — simple average ─────────────────────────────
	s1_plot9 <- base %>%
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

	# ── s1_plot10. patrimonio_alumno — weighted average ──────────────────────────
	s1_plot10 <- base %>%
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

	# ── s1_plot11. ratio alumno/jec — simple average ─────────────────────────────
	s1_plot11 <- base %>%
		filter( !is.na(jec_total), jec_total > 0 ) %>%
		mutate( ratio_alumno_jec = matricula_total / jec_total ) %>%
		group_by( tipo, year ) %>%
		summarize( ratio_alumno_jec = mean( ratio_alumno_jec, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ratio_alumno_jec, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ratio alumnos / JEC (promedio simple)",
			  subtitle = "Matrícula total / JEC total",
			  x = "Año", y = "Ratio", color = "Tipo" ) +
		theme_minimal()

	# ── s1_plot12. ratio alumno/jec — weighted average ───────────────────────────
	s1_plot12 <- base %>%
		filter( !is.na(jec_total), jec_total > 0 ) %>%
		group_by( tipo, year ) %>%
		summarize( ratio_alumno_jec = sum( matricula_total, na.rm = TRUE ) /
								     sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ratio_alumno_jec, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ratio alumnos / JEC (ponderado por matrícula)",
			  subtitle = "Matrícula total / JEC total",
			  x = "Año", y = "Ratio", color = "Tipo" ) +
		theme_minimal()

	# ── s1_plot13. % jec con doctorado — simple average ──────────────────────────
	s1_plot13 <- base %>%
		filter( !is.na(jec_total), jec_total > 0, !is.na(jec_doctor) ) %>%
		mutate( pct_jec_doctor = jec_doctor / jec_total ) %>%
		group_by( tipo, year ) %>%
		summarize( pct_jec_doctor = mean( pct_jec_doctor, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = pct_jec_doctor, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::percent ) +
		labs( title = "% JEC con doctorado (promedio simple)",
			  subtitle = "JEC con doctorado / JEC total",
			  x = "Año", y = "%", color = "Tipo" ) +
		theme_minimal()

	# ── s1_plot14. % jec con doctorado — weighted average ────────────────────────
	s1_plot14 <- base %>%
		filter( !is.na(jec_total), jec_total > 0, !is.na(jec_doctor) ) %>%
		group_by( tipo, year ) %>%
		summarize( pct_jec_doctor = sum( jec_doctor, na.rm = TRUE ) /
								      sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = pct_jec_doctor, color = tipo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::percent ) +
		labs( title = "% JEC con doctorado (ponderado por matrícula)",
			  subtitle = "JEC con doctorado / JEC total",
			  x = "Año", y = "%", color = "Tipo" ) +
		theme_minimal()

	# ── Display all 15 Section 1 plots ───────────────────────────────────────────
	print(s1_plot0)
	print(s1_plot1)
	print(s1_plot2)
	print(s1_plot3)
	print(s1_plot4)
	print(s1_plot5)
	print(s1_plot6)
	print(s1_plot7)
	print(s1_plot8)
	print(s1_plot9)
	print(s1_plot10)
	print(s1_plot11)
	print(s1_plot12)
	print(s1_plot13)
	print(s1_plot14)


# =============================================================================
# SECTION 2: ADOPTED GRATUIDAD IN 2016 VERSUS NEVER
# =============================================================================

	# Helper: base data filtered to gratuidad_year == 2016 or never adopted
	base_grat <- tibble %>%
		filter( !is.na(tipo), !is.na(matricula_total), matricula_total > 0 ) %>%
		filter( acreditacion_cat %in% c("intermedia", "avanzada") ) %>%
		filter( gratuidad_year == 2016 | is.na(gratuidad_year) ) %>%
		mutate( gratuidad_grupo = if_else( !is.na(gratuidad_year) & gratuidad_year == 2016,
										   "Adoptó gratuidad (2016)",
										   "Nunca adoptó gratuidad" ) )
        # Excluding CRUCH institutions
        # filter( tipo != 'Universidades Cruch')

	# ── s2_plot1. matricula_total — simple average ───────────────────────────────
	s2_plot1 <- base_grat %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( matricula_total = mean( matricula_total, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = matricula_total, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Matrícula total (promedio simple)",
			  subtitle = "Gratuidad 2016 vs. nunca",
			  x = "Año", y = "Estudiantes", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot2. ingreso_alumno — weighted average ──────────────────────────────
	s2_plot2 <- base_grat %>%
		filter( !is.na(aranceles), !is.na(aportes_basales) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( ingreso_alumno = sum( aranceles + aportes_basales, na.rm = TRUE ) /
									sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ingreso_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ingresos por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — (Aranceles + Aportes basales) / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot3. arancel_alumno — weighted average ──────────────────────────────
	s2_plot3 <- base_grat %>%
		filter( !is.na(aranceles) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( arancel_alumno = sum( aranceles, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = arancel_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aranceles por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — Aranceles / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot4. aporte_alumno — weighted average ───────────────────────────────
	s2_plot4 <- base_grat %>%
		filter( !is.na(aportes_basales) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( aporte_alumno = sum( aportes_basales, na.rm = TRUE ) /
								  sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = aporte_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aportes basales por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — Aportes basales / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot5. docencia_alumno — weighted average ─────────────────────────────
	s2_plot5 <- base_grat %>%
		filter( !is.na(rem_academicos) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( docencia_alumno = sum( rem_academicos, na.rm = TRUE ) /
									sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = docencia_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Remuneraciones académicas por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — Rem. académicos / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot6. patrimonio_alumno — weighted average ───────────────────────────
	s2_plot6 <- base_grat %>%
		filter( !is.na(patrimonio) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( patrimonio_alumno = sum( patrimonio, na.rm = TRUE ) /
									  sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = patrimonio_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Patrimonio por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — Patrimonio / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot7. ratio alumno/jec — weighted average ────────────────────────────
	s2_plot7 <- base_grat %>%
		filter( !is.na(jec_total), jec_total > 0 ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( ratio_alumno_jec = sum( matricula_total, na.rm = TRUE ) /
								     sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ratio_alumno_jec, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ratio alumnos / JEC (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — Matrícula total / JEC total",
			  x = "Año", y = "Ratio", color = "Grupo" ) +
		theme_minimal()

	# ── s2_plot8. % jec con doctorado — weighted average ─────────────────────────
	s2_plot8 <- base_grat %>%
		filter( !is.na(jec_total), jec_total > 0, !is.na(jec_doctor) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( pct_jec_doctor = sum( jec_doctor, na.rm = TRUE ) /
								      sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = pct_jec_doctor, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::percent ) +
		labs( title = "% JEC con doctorado (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — JEC con doctorado / JEC total",
			  x = "Año", y = "%", color = "Grupo" ) +
		theme_minimal()

	# ── Display all 8 Section 2 plots ────────────────────────────────────────────
	print(s2_plot1)
	print(s2_plot2)
	print(s2_plot3)
	print(s2_plot4)
	print(s2_plot5)
	print(s2_plot6)
	print(s2_plot7)
	print(s2_plot8)


# =============================================================================
# SECTION 3: ADOPTED GRATUIDAD IN 2016 VERSUS NEVER, ONLY NON-CRUCH INSTITUTIONS
# =============================================================================

	# Helper: base data filtered to gratuidad_year == 2016 or never adopted,
	#         excluding CRUCH institutions
	base_grat_priv <- tibble %>%
		filter( !is.na(tipo), !is.na(matricula_total), matricula_total > 0 ) %>%
		filter( acreditacion_cat %in% c("intermedia", "avanzada") ) %>%
		filter( gratuidad_year == 2016 | is.na(gratuidad_year) ) %>%
		mutate( gratuidad_grupo = if_else( !is.na(gratuidad_year) & gratuidad_year == 2016,
										   "Adoptó gratuidad (2016)",
										   "Nunca adoptó gratuidad" ) ) %>%
        filter( tipo != 'Universidades Cruch')

	# ── s3_plot1. matricula_total — simple average ───────────────────────────────
	s3_plot1 <- base_grat_priv %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( matricula_total = mean( matricula_total, na.rm = TRUE ), .groups = "drop" ) %>%
		ggplot( aes(x = year, y = matricula_total, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Matrícula total (promedio simple)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH",
			  x = "Año", y = "Estudiantes", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot2. ingreso_alumno — weighted average ──────────────────────────────
	s3_plot2 <- base_grat_priv %>%
		filter( !is.na(aranceles), !is.na(aportes_basales) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( ingreso_alumno = sum( aranceles + aportes_basales, na.rm = TRUE ) /
									sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ingreso_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ingresos por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — (Aranceles + Aportes basales) / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot3. arancel_alumno — weighted average ──────────────────────────────
	s3_plot3 <- base_grat_priv %>%
		filter( !is.na(aranceles) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( arancel_alumno = sum( aranceles, na.rm = TRUE ) /
								   sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = arancel_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aranceles por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — Aranceles / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot4. aporte_alumno — weighted average ───────────────────────────────
	s3_plot4 <- base_grat_priv %>%
		filter( !is.na(aportes_basales) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( aporte_alumno = sum( aportes_basales, na.rm = TRUE ) /
								  sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = aporte_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Aportes basales por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — Aportes basales / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot5. docencia_alumno — weighted average ─────────────────────────────
	s3_plot5 <- base_grat_priv %>%
		filter( !is.na(rem_academicos) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( docencia_alumno = sum( rem_academicos, na.rm = TRUE ) /
									sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = docencia_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Remuneraciones académicas por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — Rem. académicos / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot6. patrimonio_alumno — weighted average ───────────────────────────
	s3_plot6 <- base_grat_priv %>%
		filter( !is.na(patrimonio) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( patrimonio_alumno = sum( patrimonio, na.rm = TRUE ) /
									  sum( matricula_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = patrimonio_alumno, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Patrimonio por alumno (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — Patrimonio / Matrícula total",
			  x = "Año", y = "CLP", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot7. ratio alumno/jec — weighted average ────────────────────────────
	s3_plot7 <- base_grat_priv %>%
		filter( !is.na(jec_total), jec_total > 0 ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( ratio_alumno_jec = sum( matricula_total, na.rm = TRUE ) /
								     sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = ratio_alumno_jec, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::comma ) +
		labs( title = "Ratio alumnos / JEC (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — Matrícula total / JEC total",
			  x = "Año", y = "Ratio", color = "Grupo" ) +
		theme_minimal()

	# ── s3_plot8. % jec con doctorado — weighted average ─────────────────────────
	s3_plot8 <- base_grat_priv %>%
		filter( !is.na(jec_total), jec_total > 0, !is.na(jec_doctor) ) %>%
		group_by( gratuidad_grupo, year ) %>%
		summarize( pct_jec_doctor = sum( jec_doctor, na.rm = TRUE ) /
								      sum( jec_total, na.rm = TRUE ),
				   .groups = "drop" ) %>%
		ggplot( aes(x = year, y = pct_jec_doctor, color = gratuidad_grupo) ) +
		geom_line() + geom_point() +
		scale_y_continuous( labels = scales::percent ) +
		labs( title = "% JEC con doctorado (ponderado por matrícula)",
			  subtitle = "Gratuidad 2016 vs. nunca — no CRUCH — JEC con doctorado / JEC total",
			  x = "Año", y = "%", color = "Grupo" ) +
		theme_minimal()

	# ── Display all 8 Section 3 plots ────────────────────────────────────────────
	print(s3_plot1)
	print(s3_plot2)
	print(s3_plot3)
	print(s3_plot4)
	print(s3_plot5)
	print(s3_plot6)
	print(s3_plot7)
	print(s3_plot8)
