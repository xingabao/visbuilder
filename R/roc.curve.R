roc.curve <- function(port = 1234) {

  css.styles <- system.file("css", "styles.css", package = "your_package")

  shinyApp(
    ui = fluidPage(
      useShinyjs(),
      # web.statistic,
      tags$head(
        includeCSS(css.styles),
        HTML("<html lang='en'>"),
        tags$link(rel = "shortcut icon", href = "favicon.ico"),
      ),
      tags$style(
        HTML(".col-sm-1 { width:3.5%; !important }")
      ),
      wellPanel(
        column(width = 1, uiOutput(outputId = "left_edit")),
        column(width = 4,
               tags$style(
                 HTML(paste0('#', 'download_demo_data', " { background-color:#E3B4B8; border-color:#000000; }"))
               ),
               tags$div(
                 style = "width:1200px",
                 HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> step.1}</strong>")),
                 HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'),
                 downloadButton(outputId = "download_demo_data", label = 'download_demo_data'),
                 tags$a(href = glue("www/demo_plot.png"), target = "_blank", tags$img(src = glue("www/demo_plot.png"), width = "auto", height = "50", alt = "ZOOM IN")),
                 HTML('</div>')
               ),
               visbuilder::import_ui_demo(
                 id = "show_data",
                 from = c('env', 'file', 'copypaste', 'url'),
                 file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".json", ".rds")
               ),

               HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> step.2}</strong></div>")),
               HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Response</span>, select <span class='var-num'>one</span> variable, it is a factor, numeric or character vector of responses (true class), typically encoded with 0 (controls) and 1 (cases). Only two classes can be used in a ROC curve.</span>"),
               HTML("<span class='help-block m-b-none var-select'><i class='fa fa-info-circle'></i> [required] <span class='var-var'>Predictor</span>, select <span class='var-num'>one</span> variable, it is a numeric or ordered vector of the same length than response, containing the predicted value of each observation.</span>"),

               uiOutput(outputId = "ui_aesthetics"),

               HTML(glue("<div style='padding-top:10px;padding-bottom:5px'><strong><i class='fa-solid fa-play'></i> step.3}</strong></div>")),
               uiOutput(outputId = "is_add_plot_buttom"),

        ),
        column(width = 7,
               column(width = 12, uiOutput(outputId = "modify_plot_params")),
               column(width = 12,
                      mainPanel(
                        fluidRow(div(style = 'width:60vw;overflow-x:scroll;height:700px;overflow-y: scroll;', uiOutput(outputId = "io.albert.gg")))
                      )),

               column(width = 12, uiOutput(outputId = "intro_box")),

        )
      )
    ),
    server = function(input, output) {


      imported <- visbuilder::import_server_demo(id = "show_data", choices = c('roc_curve_demo'), return_class = "tbl_df")



    },
    # uiPattern = "/roc-curve",
    options = list(port = port)
  )
}
#
# library(glue)
# library(shiny)
# library(shinyjs)
# library(visbuilder)
# library(shinyWidgets)
# library(datamods)
#
# suppressMessages(suppressWarnings(library(shiny.i18n)))
# visbuilder::roc.curve()
# data(x.EGFR)
# visbuilder::x.EGFR
