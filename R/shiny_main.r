
examples.shiny.stories = function() {
  library(EconCurves)
  set.restore.point.options(display.restore.point = TRUE)

  add.restore.point.test(scenario.test = function(env,name,...) {
    if (exists("em",env,inherits = FALSE)) {
      if (is.null(env$em) & name!="init.story") {
        stop("em is null")
      }
      #if (!is.null(env$em$T) & length(env$em$T)==0) {
      #  stop("length(em$T) ==0")
      #}

    }
  })

  setwd("D:/libraries/EconCurves/EconCurves")
  #setwd("~/libraries/EconCurves")
  initEconStories()
  ES = getES()
  load.collection("makro.yaml")
  load.collection("hotelling.yaml")

  app = shinyStoriesApp(ES = ES)


  #coll.run.story.btn(storyId = "GreenParadoxQuiz")

  runEventsApp(app,launch.browser = rstudio::viewer)
  runEventsApp(app,launch.browser = TRUE)

}

load.collection = function(file, dir = paste0(ES$main.path,"/collections"), stories.dir = ES$stories.path, models.dir = ES$models.path, ES=getES()) {
  restore.point("load.collection")
  ES$collection = coll = read.yaml(paste0(dir,"/",file))

  story.names = names(coll$stories)
  stories = lapply(story.names, function(id) {
    load.story(storyId = id,dir = stories.dir)
  })
  names(stories) = story.names

  model.names = unique(sapply(stories, function(es) es$modelId))
  models = lapply(model.names, function(id) {
    load.model(id,dir = models.dir)
  })
  names(models) = model.names

  ES$models = models
  ES$stories = stories


  invisible(ec)
}

shinyStoriesApp = function(ES = getES(),title = "Stories from simple economic models", can.change.code = FALSE, ...) {
  restore.point("shinyStoriesApp")

  library(shinyEvents)
  library(shinyAce)
  library(shinyBS)

  app = eventsApp()
  app$..ES = ES
  app$can.change.code = can.change.code

  ui = fluidPage(title = title,uiOutput("storiesBaseUI"))

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    # copy ec for a new app instance
    nES = as.environment(as.list(ES))
    app$ES = nES

  }, app=app)

  app$ui = ui

  ES$choose.ui = stories.choose.ui()
  setUI("storiesBaseUI",ES$choose.ui)
  app
}


stories.choose.ui = function(app=getApp(), ES=app$ES,...) {
  restore.point("stories.choose.ui")
  coll = ES$collection

  cp = lapply(ES$stories, function(es) {
    title = es$title
    Encoding(title) = "UTF-8"
    if (is.null(title)) title = es$storyId
    descr = compile.to.html(es$descr)

    runBtnId = paste0(es$storyId,"_CollRunBtn")
    showStoryBtnId = paste0(es$storyId,"_CollShowStoryBtn")
    showModelBtnId = paste0(es$modelId,"_CollShowModelBtn")

    ui = bsCollapsePanel(title = title, value = paste0(es$storyId,"_CollectionPanel"),
      actionButton(runBtnId, label = "Run"),
      actionButton(showModelBtnId, label = "Show Model File"),
      actionButton(showStoryBtnId, label = "Show Story File"),
      HTML(descr)
    )

    buttonHandler(runBtnId,if.handler.exists = "skip",fun = coll.run.story.btn, storyId = es$storyId)

    buttonHandler(showStoryBtnId,if.handler.exists = "skip",fun = coll.show.story.btn, storyId = es$storyId)

    buttonHandler(showModelBtnId,if.handler.exists = "skip",fun = coll.show.model.btn, modelId = es$modelId)

    ui
  })
  names(cp) = NULL

  foots = lapply(coll$footnotes, function(ft) {
    title = ft$title
    Encoding(title) = "UTF-8"
    if (is.null(title)) title = ""
    descr = compile.to.html(ft$descr)
    ui = bsCollapsePanel(title = title,
      HTML(descr)
    )
    ui
  })
  names(foots) = NULL

  panel.ui = do.call("bsCollapse",c(list(id="collStoriesCollapse"), cp, foots))

  title.html = coll$title
  if (!is.null(title.html)) {
    Encoding(title.html) = "UTF-8"
    title.html = h3(title.html)
  }
  descr.html = compile.to.html(coll$descr)
  ui = list(title.html,HTML(descr.html),panel.ui)
  fluidRow(column(offset=1, width=10,ui))
}

coll.run.story.btn = function(app=getApp(), ES=app$ES, storyId,...) {
  restore.point("coll.run.story.btn")
  app=getApp()
  app$es = as.environment(as.list(ES$stories[[storyId]]))
  modelId = app$es$modelId

  app$em = as.environment(as.list(ES$models[[modelId]]))
  app$es$em = app$em

  init.story(es = app$es, em=app$em)
  ui = story.ui(es=app$es)

  setUI("storiesBaseUI",ui)
  run.story(app$es)

}


coll.show.story.btn = function(app=getApp(), ES=app$ES, storyId,...) {
  restore.point("coll.show.story.btn")
  es = ES$stories[[storyId]]

  setBtn = NULL

  if (app$can.change.code) {
    setBtn = actionButton("showStorySetBtn","Apply changes")
  }

  ui = list(
    aceEditor("storyYamlAce",value = es$yaml, mode="yaml"),
    bsAlert("showStoryAlert"),
    setBtn,
    actionButton("showStoryExitBtn","Exit")
  )
  buttonHandler("showStoryExitBtn", exit.to.main)

  if (app$can.change.code) {
    buttonHandler("showStorySetBtn", storyId = storyId,
      function(app=getApp(), ES=app$ES, storyId,...) {
        new.es = try({
          new.es = load.story(storyId = storyId)
          new.es = init.story(es = new.es)
          new.es
        })
        if (is(new.es,"try-error")) {
          createAlert(session = app$session,"showStoryAlert",title = "Error:",content = as.character(new.es),style = "warning")
        } else {
          ES$stories[[storyId]] = new.es
          createAlert(session = app$session,"showStoryAlert",title = "Changes are applied.",style = "success")

        }
      }
    )
  }
  setUI("storiesBaseUI",ui)
}



coll.show.model.btn = function(app=getApp(), ES=app$ES, modelId,...) {
  restore.point("coll.show.model.btn")
  em = ES$models[[modelId]]
  ui = list(
    aceEditor("modelYamlAce",value = em$yaml, mode="yaml"),
    actionButton("showStoryExitBtn","Exit")
  )
  buttonHandler("showStoryExitBtn", exit.to.main)
  setUI("storiesBaseUI",ui)
}


exit.to.main = function(app=getApp(),ES = app$ES,...) {
  if (is.null(ES$collection)) return()

  setUI("storiesBaseUI",ES$choose.ui)


}

compile.to.html = function(txt) {
  if (is.list(txt)) {
    return(lapply(txt,compile.to.html))
  }

  if (is.null(txt)) return("")

  restore.point("compile.to.html")
  Encoding(txt) = "UTF-8"
  html =  markdownToHTML(text=txt, fragment.only=TRUE, encoding="UTF-8")
  html
}
