my_ui <- navbarPage(
  "Our Title",
  tabPanel(
    "Introduction"
  ),
  tabPanel(
    "Yusha"
  ), # Yusha
  tabPanel(
    "Jin"
  ), # Jin
  tabPanel(
    "Ben"
  ), # Ben
  tabPanel(
    "Kevin", textOutput( "message"),
             plotlyOutput( "pie" ), 
             column( width = 12,
                     fluidPage( 
                       column( width = 6, plotOutput( "gender")), 
                       column( width = 6, plotOutput( "trip")) 
                       ) )
  ) # Kevin this is a test a test
)
