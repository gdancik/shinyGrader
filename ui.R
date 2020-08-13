
# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).

if (!require(shinyjs)) install.packages('shinyjs')
if (!require(shinyFiles)) install.packages('shinyFiles')

library(shinyjs)
library(shinyFiles)


source("uiTabPanelUpload.R", local = TRUE)
source("uiTabPanelGradeQuestions.R", local = TRUE)
source("uiTabPanelFinal.R", local = TRUE)

# Use a fluid Bootstrap layout
fluidPage(

  includeCSS('www/styles.css'),
  
  inlineCSS(list(
    #"body" = "background-color: yellow",
    "#shiny-notification-panel" = "width: 33% !important; bottom: 10% !important;",
    "#shiny-notification-graded_exists" = "border: 1px solid black;",
    "#previousq, #nextq, #previousp, #nextp"= "margin-top:25px;",
    "#previouspFinal, #nextpFinal" = "margin-top:25px;",
    ".shiny-notification" = "border-style: solid; border-color: black; opacity: 1;"
    
  )), 
  
  # Give the page a title
 # titlePanel("Help Me Grade"),
useShinyjs(),
 #shinythemes::themeSelector(),
  navbarPage('Help me grade', inverse = TRUE,# theme = shinytheme("cerulean"),
    id = 'tab',
    
    tabPanelUpload, 
    tabPanelGradeQuestions,
    tabPanelFinal, 


  tabPanel('Gradebook', 
           h3('Gradebook'),
           p('Exporting option coming soon'), br(), br(),
           tableOutput('gradebook'))
  )
)


