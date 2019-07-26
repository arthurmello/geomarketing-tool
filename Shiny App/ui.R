#######################################################################################
# SHINY APP TO FIND IRIS IN PARIS ACCORDING TO USER-SET CRITERIA
# AUTHOR: ARTHUR MELLO
# CREATED ON: 22/04/2019
#######################################################################################

# Loading libraries
library("leaflet")
library("shiny")
library("shinythemes")

fluidPage(titlePanel("Find your target market in Paris:"),
          sidebarLayout(
            sidebarPanel(width = 3,
                         h4('Gender'),
                         checkboxInput('H', 'Men', value = TRUE),
                         checkboxInput('F', 'Women', value = FALSE),
                         
                         h4('Age'),
                         checkboxInput('0014', '0 to 14 years', value = FALSE),
                         checkboxInput('1529', '15 to 29 years', value = FALSE),
                         checkboxInput('3044', '30 to 44 years', value = FALSE),
                         checkboxInput('4559', '45 to 59 years', value = FALSE),
                         checkboxInput('6074', '60 to 74 years', value = FALSE),
                         checkboxInput('75P', 'more than 75 years', value = FALSE)
            ),
            mainPanel(bootstrapPage(div(class="outer",
                                        shiny::tags$style(
                                          type ="text/css", ".outer {position: fixed; top: 63px;
                                          left: 240px; right: 0; bottom: 0; overflow: hidden;
                                          padding: 0}"),leafletOutput("map", width="100%", height="100%")
                                        
                                        ))
            )
            )
            )







