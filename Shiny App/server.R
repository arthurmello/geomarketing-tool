#######################################################################################
# SHINY APP TO FIND IRIS IN PARIS ACCORDING TO USER-SET CRITERIA
# AUTHOR: ARTHUR MELLO
# CREATED ON: 22/04/2019
#######################################################################################

# Loading libraries and Token_map_box
library('tidyverse')
library('leaflet')
library('shiny')

Token_map_box = 'https://api.mapbox.com/styles/v1/arthurmello/cjrrhiraq019v2sl8kz15p59p/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXJ0aHVybWVsbG8iLCJhIjoiY2pycmhkcXRxMXphajQzbXRoZ2dqMTFsdiJ9.SGlFjvQl9cNE4cM5k5EnaQ'

# Getting current file location
#getCurrentFileLocation <-  function()
#{
#  this_file <- commandArgs() %>% 
#    tibble::enframe(name = NULL) %>%
#    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
#    dplyr::filter(key == "--file") %>%
#    dplyr::pull(value)
#  if (length(this_file)==0)
#  {
#    this_file <- rstudioapi::getSourceEditorContext()$path
#  }
#  return(dirname(this_file))
#}

#setwd(getCurrentFileLocation())
#path = dirname(getCurrentFileLocation())

path = ""
# Loading data
load("INSEE.RData")

# Generating leaflet
function(input, output, session) {
  
  sex = reactive({
    # If no sex is selected, we take the whole population for a given age range as input
    if(input$H==TRUE & input$'F' ==FALSE) {'H'}
    else
    {if (input$H==FALSE & input$'F' ==TRUE) {'F'}
        else {'POP'}}
    })
  
  
  x = c(rep(0,nrow(INSEE@data)))
  
  # The final_score is the population according to the given input divided by the IRIS area
  final_score = reactive({
    
    for (i in c('0014','1529','3044','4559','6074','75P')){
      # If no age range is selected, we take the whole population for a given sex as input
      if (sum(input[[i]])==0){
        boolean = c(1,1,1,1,1,1) 
      } else {
        boolean = input[[i]]
      }
      x = rowSums(cbind(x, (INSEE@data[, paste('P14_', sex(), i, sep = "")] * boolean)))
    }
    
    return(x/INSEE@data$area)
  })
  
  # The polygon colors will be given according to the final_score
  pal <- reactive({
    colorNumeric(palette = "YlOrRd", domain = final_score())
  })
  
  output$map = renderLeaflet({
    leaflet() %>%  addTiles(urlTemplate = Token_map_box) %>%
      addPolygons(data = INSEE, color='black', fillColor = ~pal()(final_score()), label = paste(round(final_score()),"targeted people/km²"),
                  fillOpacity = 0.5, weight = 1,
                  highlightOptions = highlightOptions(color = "white", weight = 7,bringToFront = FALSE,fillOpacity = 0.5),
                  labelOptions = labelOptions(noHide = FALSE,direction = 'top',offset=c(0,0),textOnly = TRUE,
                                              style=list('color'='rgba(0,0,0,1)','font-family'= 'Arial Black','font-style'= 'bold',
                                                         'box-shadow' = '0px 0px rgba(0,0,0,0.25)','font-size' = '14px',
                                                         'background-color'='rgba(255,255,255,0.7)','border-color' = 'rgba(0,0,0,0)'))) %>%
      addLegend("bottomright", pal = pal(), values = final_score(),
                title = "Targeted people/km²",
                opacity = 1
      )
})
}

