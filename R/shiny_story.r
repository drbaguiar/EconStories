
examples.shiny.story = function() {
  set.restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/EconStories/EconStories")
  initEconStories()
  ES = getES()
  es = load.story("SimpleLabor3EqStory")
  es = load.story("PhillipsCurveIntroStory")
  es = load.story("ThreeEq_G_langfristig")
  init.story(es)

  app = shinyStoryApp(es)
  frame = es$frames[[1]]
  ui = story.frame.ui(frame = frame,es=es)
  addResourcePath("images",paste0(getES()$stories.path,"/images"))
  runEventsApp(app,launch.browser = rstudio::viewer)

  view.html(ui)
  html = as.character(ui)


  set.dynry.step(t=1,es=es)
  frame = es$frames[[2]]
  layout = frame$layout


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
    buttonHandler("storyQuiz__checkBtn",click.story.check.quiz, es=es)
  }, app=app)

  app$ui = ui
  show.story.frame(es=es)
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
  frame = es$cur$frame
  if (length(frame$task)==0) {
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
    story.tell.frame.sol()
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
  show.story.frame(es=es)
}

story.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("stNextBtnClicked")
  #cat("stNextBtn was clicked...")

  if (isTRUE(es$wait.for.answer)) {
    setUI(id = "answerUI",p(paste0("You have not yet answered the question.")))
    return()
  }

  if (es$storyType == "dynamics") {
    res = set.dynry.next.frame(es=es)
  } else {
    res = set.story.next.frame(es=es)
  }
  if (res$end) return()
  show.story.frame(es=es)
}

story.forward.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("story.forward.btn.click")

  if (es$storyType == "dynamics") {
    res = set.dynry.forward.frame(es=es)
  } else {
    res = set.story.forward.frame(es=es)
  }

  show.story.frame(es=es)
}


story.prev.btn.click = function(app=getApp(), es=app$es,...)  {
  restore.point("stPrevBtnClicked")

  if (es$storyType == "dynamics") {
    res = set.dynry.prev.frame(es=es)
  } else {
    res = set.story.prev.frame(es=es)
  }
  if (res$start) return()
  show.story.frame(es=es)
}



