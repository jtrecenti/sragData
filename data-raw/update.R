dados_todos <- sragData::update_srag_data()
readr::write_rds(dados_todos, "data-raw/dados_SRAG.RDS")
