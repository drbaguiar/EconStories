
examples.shiny.story = function() {
  set.restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/EconStories/EconStories")
  initEconStories()
  ES = getES()
  es = load.story("ThreeEq_G_langfristig")
  es = load.story("SimpleLabor3EqStory")
  es = load.story("PhillipsCurveIntroStory")
  init.story(es)

  app = shinyStoryApp(es)
  part = es$parts[[2]]
  ui = story.part.ui(part = part,es=es)
  addResourcePath("images",paste0(getES()$stories.path,"/images"))
  runEventsApp(app,launch.browser = rstudio::viewer)

  view.html(ui)
  html = as.character(ui)


  set.dynry.step(t=1,es=es)
  part = es$parts[[2]]
  layout = part$layout


  em = es$em
  sim = em$sim
  dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)
}

shinyStoryApp = function(es,...) {
  restore.point("shinyStoryApp")

  library(shinyEvents)
  library(shinyAce)
  library(shinyBS)

  app = eventsApp()
  app$es = es
  app$allow.edit = getES()$allow.edit
  ui = story.ui()
  ui = fluidPage(
    title = es$storyId,
    ui
  )

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    addResourcePath("images",paste0(getES()$stories.path,"/images"))
    app$es = es
  }, app=app)

  app$ui = ui
  show.story.part(es=es)
  app
}


add.story.button.handlers = function() {
  buttonHandler("stNextBtn", story.next.btn.click,if.handler.exists = "skip")
  buttonHandler("stForwardBtn", story.forward.btn.click,if.handler.exists = "skip")
  buttonHandler("stPrevBtn", story.prev.btn.click,if.handler.exists = "skip")
  buttonHandler("stExitBtn", exit.to.main,if.handler.exists = "skip")
  buttonHandler("stRefreshBtn", story.refresh.click,if.handler.exists = "skip")
}


story.ui = function(app=getApp(), es=app$es) {
  restore.point("story.ui")
  add.story.button.handlers()
  uiOutput("storyMainUI")

}


story.wait.for.answer = function(app=getApp(), es=app$es) {
  restore.point("story.wait.for.answer")
  part = es$cur$part
  if (length(part$task)==0) {
    es$wait.for.answer = FALSE
    return(FALSE)
  }
  es$attempts = 0
  es$wait.for.answer = TRUE
  return(TRUE)
}

story.process.click.answer = function(app=getApp(), es=app$es,xy, pane.name,...) {
  restore.point("story.check.click.answer")
  res = check.click.answer(es = es,xy = xy,pane.name = pane.name)

  # correct
  if (res) {
    es$wait.for.answer = FALSE
    story.tell.part.sol()
  } else {
    es$attempts = es$attempts+1
    setUI(id = "answerUI",p(paste0("Not correct. (", es$attempts, " attempts.)")))
  }
}

story.pane.click = function(app=getApp(), es=app$es,pane.name,id, session, value,...) {
  #args = list(...)
  restore.point("shiny.pane.click")
  if (length(value)==0)
    return()
  restore.point("shiny.pane.click.with.val")
  xy = c(x=value$x,y=value$y)
  cat("\nclick: ")
  print(value)

  # outside a task just continue
  if (!is.true(es$wait.for.answer)) {
    story.next.btn.click()
  } else {
    story.process.click.answer(app = app,es=es,xy = xy,pane.name = pane.name)
  }
  #cat("Click!")
}


story.refresh.click = function(app=getApp(), es=app$es,...) {
  cat("Refresh story...")
  t = es$cur$t; step.num = es$cur$step.num
  id = es$storyId
  restore.point("refresh.story.click")

  # reload and init story
  es = load.story(id)
  app$es = es
  init.story(es)
  set.story.cur(es=es, cur=cur)
  show.story.part(es=es)
}

story.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("stNextBtnClicked")
  #cat("stNextBtn was clicked...")

  if (isTRUE(es$wait.for.answer)) {
    setUI(id = "answerUI",p(paste0("You have not yet answered the question.")))
    return()
  }

  if (es$storyType == "dynamics") {
    res = set.dynry.next.part(es=es)
  } else {
    res = set.story.next.part(es=es)
  }
  if (res$end) return()
  show.story.part(es=es)
}

story.forward.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("story.forward.btn.click")

  if (es$storyType == "dynamics") {
    res = set.dynry.forward.part(es=es)
  } else {
    res = set.story.forward.part(es=es)
  }

  show.story.part(es=es)
}


story.prev.btn.click = function(app=getApp(), es=app$es,...)  {
  restore.point("stPrevBtnClicked")

  if (es$storyType == "dynamics") {
    res = set.dynry.prev.part(es=es)
  } else {
    res = set.story.prev.part(es=es)
  }
  if (res$start) return()
  show.story.part(es=es)
}



