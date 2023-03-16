ui <- fluidPage(
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
  fluidRow(column(ui_width, offset = ui_offset, h3("Webtool Saffier", style = paste0("color:", col_rose)))),
  fluidRow(column(ui_width, offset = ui_offset, p("Hoe zou jij het economische beleid wijzigen als jij het voor het zeggen had? Wat gebeurt er als de (internationale) economische omgeving verandert? Bekijk het verwachte resultaat hieronder.", style = " text-align: justify", .noWS = c("after-begin", "before-end")) #  Lees eerst over de ", actionLink("id_intro", "werking en beperking", .noWS = "outside"), " van deze tool (", actionLink("id_disclaimer", "disclaimer", .noWS = "outside"), ").
  )),
  # fluidRow(column(ui_width, offset = ui_offset,
  #   div(
  #     shiny::HTML(markdown::markdownToHTML(knitr::knit(get_snippet_file_name("intro"), quiet = TRUE), fragment.only = TRUE)),
  #     style="text-align: justify"
  #   )
  # )),
  fluidRow(column(ui_width, offset = ui_offset,
    div(style = "position:absolute;right:1em;",
      shinyBS::tipify(
        actionButton("reset", label = "Reset"),
        title = "Reset impulstabel",
        placement = "left",
        trigger = "hover",
        options = NULL
      ),
      shinyBS::tipify(
        downloadButton("report", shiny::HTML("&nbsp;&nbsp;Download resultaten"), icon("file-pdf"), style = paste0("color: #fff; background-color: ", col_anakiwa, "; border-color: ", col_anakiwa)),
        title = "PDF met impulstabel, resultaattabel, figuur en uitleg",
        placement = "left",
        trigger = "hover",
        options = NULL
      )
    ),
    tabsetPanel(id = "tabs", type = "tabs",
      tabPanel("Impulsen", icon = icon("bolt"),
        fluidRow(
          # column(width = 12,
            br(),
            tags$table(
              style = "margin-left:15px;",
              tags$tr(
                tags$td(
                  p("Wij veronderstellen dat de economie een zeker basispad volgt. Hieronder kun je het basispad van één of twee economische variabelen veranderen door permanente of tijdelijke impulsen toe te dienen. Als je een tijdelijke impuls wilt geven, moet je een later jaar een tegengestelde impuls geven (bijv. +1 in jaar 1 en -1 in jaar 4). De impulsen treden in werking op 1 januari van het jaar. Bij te veel of te grote impulsen tonen we geen resultaten (stoplicht springt op rood).", style = " text-align: justify")
                ),
                tags$td(
                  style = "width: 70px; vertical-align: top;",
                  tags$div(id="thermometer", style = "height:50px;", plotOutput("thermometer")) #style = "margin-top:20px; height:50px; width:25px;",
                )
              )
            )
          # )
        ),
        # fluidRow(
        #   column(width = 11, br(),p("Wij veronderstellen dat de economie een zeker basispad volgt. Hieronder kun je het basispad van één of twee economische variabelen veranderen door impulsen toe te dienen. De impulsen zijn permanent en treden in werking op 1 januari van het jaar. Als je een tijdelijke impuls wilt geven, moet je een later jaar een tegengestelde impuls invullen (bijv. +1 in jaar 1 en -1 in jaar 4). Bij te veel of te grote impulsen tonen we geen resultaten (stoplicht springt op rood).", style = " text-align: justify")),
        #   column(width = 1,
        #     tags$div(id="thermometer", style = "margin-top:20px; height:50px; width:25px;", plotOutput("thermometer"))
        #   )
        # ),
        # fluidRow(
        DT::dataTableOutput("struct_table"),
        span(textOutput("warning"), style = paste0("color:", col_rose))
        # span(textOutput("variable_explanation"), style = paste0("color:", light_green))
        # div(style="display: inline-block;", p(textOutput("warning"), style = "color:#e6006e"))
        # )
        # DTOutput("impulse_table"),
        # br(),
        # p("Voorbeeld: stel de wereldhandel heeft de komende vijf jaar achtereenvolgens waarden 100, 200, 300, 300, 300. Je geeft een impuls van +1 %-punt in jaar 2. De waarden zijn dan respectievelijk 100, 202, 302, 302, 302.", style = "color: darkgray; text-align: justify")
      ),
      tabPanel("Resultaattabel", icon = icon("table"),
        br(),
        bsTooltip("tooltipid", "initialization tooltips"),
        p("De tabel hieronder toont de verwachte gevolgen van de impulsen voor de komende 1, 2, 4, 7 en 10 jaar (", actionLink("popup_var_effect_id", "toelichting variabelen", .noWS = "outside"), "). Elke cel geeft het gemiddelde effect in een jaar, ten opzichte van het basispad. Hieronder kun je de reeksen selecteren die je in het figuur wilt zien.", uiOutput("variable_explanation"), style = " text-align: justify;", .noWS = c("after-begin", "before-end")),
        DT::dataTableOutput("effect_table"),
        # DTOutput("effect_table"),
        span("Selecteer reeks(en) voor in je figuur.", style = paste0("color:", col_rose))
        # uiOutput("variable_explanation")
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
      ),
      tabPanel("Uitleg", icon = icon("info"),
        fluidRow(
          column(width = 12,
            br(),
            div(
              shiny::HTML(markdown::markdownToHTML(get_snippet_file_name("intro"), fragment.only = TRUE)),
              style="text-align: justify"
            ),
            actionLink("id_disclaimer", "Disclaimer", .noWS = "outside"), "."
          )
        )
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
