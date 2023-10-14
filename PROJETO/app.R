library(tidyr)
library(dplyr)
library(stringr)
library(pdftools)
library(tibble)
library(openxlsx)
library(shiny)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
       
table {
text-align: center;font-size: 1.2vh;text-align: center;}
form {
background-color: lightblue;text-align: center;
width: 400px;heigth: 250px;}
h1{text-align: center;}table, th, td {border: 1px
solid black;}table {border-collapse:
collapse;margin: auto;}th, td{padding:
10px;text-align: center;width:
120px;}th{font-weight: bold;}tr:nth-child(even)
{background-color: #DCEBE6;}tr:hover:nth-child(1n
+ 2) {background-color: #085F63;color: #fff;}
form a { border: 0 solid #E5E7EB; box-sizing: border-box;
color: #000000; display: flex;
font-weight: 700; justify-content: center; line-height: 1.75rem;
padding: .75rem 1.65rem;position: relative;text-align: center;
width: 100%;position: relative;
}
                    
"))
  ),
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', HTML('<h2><strong>Selecione o seu PDF</h2></strong> <br> <small>Serão retornadas apenas 28 linhas de exemplo.</small>'), accept = c(".pdf")),
      downloadButton('downloadData', 'Baixar' )
    ),
    mainPanel(
      tableOutput('df_table')
    )
  )
)

server <- function(input, output) {
  arquivo_processado <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    arquivo <- pdf_text(inFile$datapath)
    
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
    
    tratativa <- removendo_sujeira |>
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
    
    df <- data.frame(Text = tratativa)
    colnames(df) <- gsub("Text\\.", "", colnames(df))
    df <- subset(df, select = -ncol(df))
    
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("relatorio_produto", Sys.Date(), Sys.time(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(arquivo_processado(), file)
    }
  )
  output$df_table <- renderTable({
    df <- head(arquivo_processado(), 28)
  })
}

shinyApp(ui, server)
