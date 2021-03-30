pre_process <- function(dados, dia, semanas_trunca) {
  dados %>%
    dplyr::select(DT_INTERNA, DT_NOTIFIC, DT_SIN_PRI, SG_UF_NOT, NU_IDADE_N, CLASSI_FIN) %>%
    dplyr::filter(!is.na(NU_IDADE_N), !is.na(DT_INTERNA)) %>%
    dplyr::mutate(
      DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
      DT_INTERNA = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
      DT_SIN_PRI = as.Date(DT_SIN_PRI, format = "%d/%m/%Y")
    ) %>%
    dplyr::filter(
      DT_INTERNA > as.Date("2020-12-23"),
      DT_INTERNA <= dia - semanas_trunca * 7
    ) %>%
    dplyr::mutate(idade_cat = cut(NU_IDADE_N, c(0,40,60,70,80,90,120), right = FALSE)) %>%
    tibble::as_tibble()
}

#' Update SRAG data
#'
#' @export
update_srag_data <- function() {
  ckanr::ckanr_setup("https://opendatasus.saude.gov.br")

  arqs <- ckanr::package_search("srag 20")$results %>%
    purrr::map("resources") %>%
    purrr::map(purrr::keep, ~.x$mimetype == "text/csv") %>%
    purrr::map_chr(purrr::pluck, 1, "url")

  dia <- lubridate::dmy(basename(arqs[1]))
  semanas_trunca <- 3

  # demora
  dados <- purrr::map(arqs, data.table::fread)

  # rapido
  dados_todos <- purrr::map_dfr(
    dados, pre_process,
    dia = dia, semanas_trunca = semanas_trunca
  )

  dados_todos
}
