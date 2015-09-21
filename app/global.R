#setwd("D:/libraries/EconCurves/EconCurves/app")

library(EconCurves)
ES = initEconStories()
load.collection("makro.yaml")

app = shinyStoriesApp(ES = ES)
app$verbose = FALSE
app$is.running = TRUE

#shinyApp(ui = app$ui, server = app$server)
#runEventsApp(app,launch.browser = rstudio::viewer)
