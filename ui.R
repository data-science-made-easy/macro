ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "switch-button.css")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "cpb.css")),

  shinybusy::use_busy_spinner(spin_id = "spin_fig", spin = "scaling-squares", color = col_rose, position = "top-left", margins = c(290, 240)),
  shinybusy::use_busy_spinner(spin_id = "spin_rep", spin = "fulfilling-square", color = col_rose, position = "top-left", margins = c(290, 240)),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c('disableTab','enableTab')),
  shinyjs::inlineCSS(app_css),
  # tags$head(tags$style(
  #         type="text/css",
  #         "#plot img, #plot2 img {
  #             width: 60%;
  #             display: block;
  #             margin-left: auto;
  #             margin-right: auto;
  #         }"
  #     )),
  # fluidRow(column(ui_width, offset = ui_offset, h3("Draai aan de knoppen van de Nederlandse economie", style = paste0("color:", col_rose)))),
  fluidRow(column(ui_width, offset = ui_offset, p("Hoe zou jij het economische beleid wijzigen als jij het voor het zeggen had? Wat gebeurt er als de (internationale) economische omgeving verandert? Bekijk het verwachte resultaat hieronder.", style = " text-align: justify"))),
  fluidRow(column(ui_width, offset = ui_offset,
    div(style = "position:absolute;right:1em;",
      actionButton("reset", label = "reset"),
      downloadButton("report", HTML("&nbsp;&nbsp;download als pdf"), icon("file-pdf"), style = paste0("color: #fff; background-color: ", col_anakiwa, "; border-color: ", col_anakiwa))
    ),
    tabsetPanel(id = "tabs", type = "tabs",
      # tabPanel("Struct", icon = icon("bolt"),
      #   DT::dataTableOutput("struct_table") #DTOutput("struct_table")
      # ),
      tabPanel("Impulsen", icon = icon("bolt"),
      fluidRow(
          column(width = 11, br(),p("Wij veronderstellen dat de economie een zeker basispad volgt. Hieronder kun je het basispad van een of twee economische variabelen verstoren door permanente maar beperkte impulsen toe te dienen. Bij te veel of te grote impulsen tonen we geen resultaten (stoplicht springt op rood).", style = " text-align: justify")),
          column(width = 1,
            tags$div(id="thermometer", style = "margin-top:10px; height:50px; width:25px;", plotOutput("thermometer"))
          )
        ),
        # fluidRow(
        DT::dataTableOutput("struct_table"),
        span(textOutput("warning"), style="color:#e6006e")
        # div(style="display: inline-block;", p(textOutput("warning"), style = "color:#e6006e"))
        # )
        # DTOutput("impulse_table"),
        # br(),
        # p("Voorbeeld: stel de wereldhandel heeft de komende vijf jaar achtereenvolgens waarden 100, 200, 300, 300, 300. Je geeft een impuls van +1 %-punt in jaar 2. De waarden zijn dan respectievelijk 100, 202, 302, 302, 302.", style = "color: darkgray; text-align: justify")
      ),
      tabPanel("Resultaattabel", icon = icon("table"),
        br(),
        p("De tabel hieronder benadert de verwachte gevolgen van de impulsen voor de komende tien jaar. Elke cel beschrijft de verandering in procenten (%) ten opzichte van het basispad in dat jaar (", actionLink("show", "disclaimer", .noWS = "outside"), "). Hieronder kun je de reeksen selecteren die je in je figuur wilt zien.", style = " text-align: justify;", .noWS = c("after-begin", "before-end")),
        DTOutput("effect_table"),
        p("Selecteer reeks(en) voor in je figuur.", style = paste0("color:", col_rose))
      ),
      tabPanel("Figuur", icon = icon("chart-line"),
        br(),
        p("Hieronder zie je het verwachte effect van de impulsen op de in de resultaattabel geselecteerde tijdreeksen.", style = " text-align: justify"),
        # div(style = "position:absolute;right:1em;",
        #   prettyToggle(inputId = "barLineToggle", label_on = "lijn", label_off = "staaf", status_on = "info", status_off = "info", outline = TRUE, plain = TRUE, icon_on = icon("chart-line"), icon_off = icon("chart-bar"), inline = T)
        # ),
        # shinycssloaders::withSpinner(plotOutput("plot"), color = col_rose, hide.ui = FALSE),
        plotOutput("plot"),
        br(),
        br(),
        br(),
        br(),
        br(),
        p("Je kunt je selectie wijzigen door andere tijdreeksen te selecteren in de resultaattabel.", style = paste0("color:", col_rose))
      )
    )
  )# , column(width = 12 - ui_width - ui_offset, offset = 0,
#     # p("impulsmeter"),
#     p("Impulsthermometer:", style = paste0("color:", col_rose)),
#     # plotOutput("thermometer")
#     tags$div(id="thermometer", style = "text-align:left;width:400px", plotOutput("thermometer"))
#   )
  )# ,
#   fluidRow(column(ui_width, offset = ui_offset, align = "right",
#     div(style="display: inline-block; color: darkgray; height:50px;",
#       "Hoe bevalt deze app?",
#       barRating::barRating(theme = "bootstrap-stars", choices = 1:5, initialRating = NULL, showSelectedRating = T, includeEmpty = T)
#     )
#   ))
)