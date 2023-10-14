library(tidyr)
library(dplyr)
library(pdftools)
library(tibble)
library(stringr)
library(shiny)

# Atribui o arquivo .PDF para uma variável
# A função pdf_text converte os dados do arquivo .PDF em texto.

arquivo <- pdf_text("ARQUIVO_ORIGEM/produto.pdf")

# Atribuição do tibble para transformar a variável arquivo em um DataFrame
# Quebra o texto contínuo em várias linhas com o mesmo visual de um arquivo de texto.

removendo_sujeira <- tibble(texto = arquivo) |>
  mutate(texto = str_split(texto, pattern = "\\n")) |>
  unnest(cols = c(texto)) |>
  mutate(
    texto = str_trim(texto, side = "left"),
    across(everything(), toupper)
  ) |>
  filter(
    texto != "",
    !str_starts(texto, "R\\$"),
    !str_starts(texto, "LOJA"),
    !str_starts(texto, "TOTAL")
  ) |>
  mutate(
    texto = str_replace_all(texto, " BLUSAS", "     BLUSAS"),
    texto = str_replace_all(texto, " DATA SYSTEM ", "  DATASYSTEM  "),
    texto = str_replace_all(texto, " CALCA", "     CALCA")
  )

# Transformando o dataframe
tratativa_1 <- removendo_sujeira |>
  separate(
    texto,
    into = c("LOJA", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  tidyr::separate(
    texto,
    into = c("PRODUTO", "texto"),
    extra = "merge",
    sep = "  "
  ) |>
  mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("CATEGORIA", "texto"),
    extra = "merge",
    sep = "R\\$"
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("PRECO", "texto"),
    extra = "merge",
    sep = "R\\$"
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("CUSTO", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("REFERENCIA", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("COD_BARRAS", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("TAMANHO", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("COR", "texto"),
    extra = "merge",
    sep = "R\\$"
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("TOTAL_PRECO", "texto"),
    extra = "merge",
    sep = "R\\$"
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("TOTAL_CUSTO", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(texto = stringr::str_trim(texto, side = "left")) |>
  tidyr::separate(
    texto,
    into = c("QUANTIDADE", "texto"),
    extra = "merge",
    sep = " "
  ) |>
  dplyr::mutate(
    texto = str_replace_all(texto, " 6", "        6"),
    texto = str_replace_all(texto, "   TRIBUTADO", "TRIBUTADO")
  ) |>
  tidyr::separate(
    texto,
    into = c("ALIQUOTA", "texto"),
    extra = "merge",
    sep = "   "
  ) |>
  mutate(
    ALIQUOTA = str_trim(ALIQUOTA, side = "left"),
    texto = stringr::str_trim(texto, side = "left")
  ) |>
  tidyr::separate(
    texto,
    into = c("NCM", "texto"),
    sep = " "
  )

writexl::write_xlsx(tratativa_1, "ARQUIVO_FINAL/PRODUTO_FINAL.xlsx")

library(shiny)
runExample("10_download")
