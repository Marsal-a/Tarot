library(shiny)
navbarPage("scores", id="nav",
           tabPanel("scores",
                    fluidRow(
                        column(3,
                               selectInput("Squads", "config",config$players, multiple=TRUE)
                        )
                    ),
                    fluidRow(
                        column(2,
                               conditionalPanel("input.Squads",selectInput("Preneur", "Preneur",c(""), multiple=F))
                        ),
                        column(2,
                               conditionalPanel("input.Squads",selectInput("contrat_type", "contrat_type",selected = "Garde",Contrat_table$contrat, multiple=F))
                        ),
                        column(1,
                               conditionalPanel("input.Squads",checkboxInput("itpasses",label = "it passes ?",value=T))
                        ),
                        column(2,
                               conditionalPanel("input.Squads",numericInput("ecart",label = "cmb",min = -50,max = 50,step = 5,value=0))
                        ),
                        column(1,
                               conditionalPanel(condition="input.Squads && input.itpasses==true",checkboxInput("petit",label = "petitaubot",value=F,width = 1))
                        )
                    ),
                    fluidRow(
                        column(1),
                        conditionalPanel("input.Squads",
                            actionButton("RUN_CALC","calc",width="30%",icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        )
                    ),
                    hr(),
                    DT::dataTableOutput("scores")
           )
)
