library(shiny)
library(animint)

nms <- names(mtcars)

# This example follows the shiny layout guide -- http://shiny.rstudio.com/articles/layout-guide.html
# Buy you can use whatever layout you wish :)
shinyUI(fluidPage(
  
  titlePanel("animint meets shiny"),
  
  h4(HTML("<a href='https://github.com/tdhock/animint/tree/master/inst/examples/shiny'> Click here </a> to view source")),
  
  br(),
  
  animintOutput("animint")
  
))