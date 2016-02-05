shinyUI(navbarPage(theme=shinytheme("spacelab"), inverse=TRUE,
  #tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  title=HTML('<div><a href="http://snap.uaf.edu" target="_blank"><img src="./img/SNAP_acronym_100px.png" width="100%"></a></div>'),
  windowTitle="NWT",
  collapsible=TRUE,
  id="nb",
  tabPanel("NWT Projections", value="vis",
  div(class="outer",
  tags$head(includeCSS("www/styles.css")),
  leafletOutput("Map", width="100%", height="100%"),
  selectInput("location", "Community", c("", locs$loc), selected="", multiple=F),
  conditionalPanel("input.show_communities == true && input.location !== null && input.location !== ''",
                   actionButton("btn_modal_loc", "View Summary", class="btn-block")),
  bsModal("Modal_Loc", "Community Insights", "btn_modal_loc", size = "large",
          plotOutput("TestPlot"),
          dataTableOutput("TestTable")
  ),
  absolutePanel(id="controls", top=360, right=0, height=300, width=300,
    conditionalPanel("input.show_colpal == true",
      wellPanel(
        fluidRow(
          column(6,
            conditionalPanel("(input.colpal_type == 'Divergent' && input.colpal_div == 'Custom') ||
                             (input.colpal_type == 'Sequential' && input.colpal_seq == 'Custom')",
              colourInput("col_low", "Low", value = "#8C0050"),
              conditionalPanel("input.colpal_type == 'Divergent'", colourInput("col_med", "Med", value = "#CEEBF0")),
              colourInput("col_high", "High", value = "#000470"))
          ),
          column(6,
            selectInput("colpal_type", "Style", c("Divergent", "Sequential"), "Divergent"),
            conditionalPanel("input.colpal_type == 'Divergent'", uiOutput("Colpal_div_options")),
            conditionalPanel("input.colpal_type == 'Sequential'", uiOutput("Colpal_seq_options"))
          )
        )
      )
    )
  ),
  absolutePanel(id="controls", top=20, right=0, height=300, width=300,
    sliderInput("dec", "Decade", min=min(decades), max=max(decades), value=decades[1], step=10, sep="", post="s"),
    fluidRow(
      column(6,
        selectInput("mon_or_sea", "Time of year", c("Monthly", "Seasonal"), "Monthly"),
        selectInput("variable", "Variable", var.labels, var.labels[1]),
        selectInput("mod_or_stat", "GCM data", c("Single GCM", "Statistic"), "Single GCM")
      ),
      column(6,
        conditionalPanel("input.mon_or_sea == 'Monthly'", selectInput("mon", "Month", month.abb, month.abb[1])),
        conditionalPanel("input.mon_or_sea == 'Seasonal'", selectInput("sea", "Season", season.labels, season.labels[1])),
        selectInput("rcp", "RCP", rcps, rcps[1]),
        conditionalPanel("input.mod_or_stat == 'Single GCM'", selectInput("model", "Model", models, models[1])),
        conditionalPanel("input.mod_or_stat == 'Statistic'", selectInput("model_stats", "Stat", c("Mean", "Min", "Max", "Spread"), "Mean"))
      )
    )
  ),
  absolutePanel(id="controls", top=20, left=0, height=300, width=300, draggable=TRUE,
    plotOutput("sp_density_plot", width="100%", height="auto")
  ),
  absolutePanel(bottom=10, left=10,
    conditionalPanel("input.mod_or_stat == 'Single GCM'", checkboxInput("deltas", "Display deltas", FALSE)),
    checkboxInput("show_communities", "Show communities", TRUE),
    checkboxInput("legend", "Show legend", TRUE),
    checkboxInput("show_colpal", "Show color options", FALSE)
  )
  )
  ),
  tabPanel("About", value="about")
))
