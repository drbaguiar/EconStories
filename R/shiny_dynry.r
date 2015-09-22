
examples.shiny.dynry = function() {
  set.restore.point.options(display.restore.point = TRUE)
  setwd("D:/libraries/EconStories/EconStories")
  initEconStories()
  ES = getES()
  es = load.story("ThreeEq_G_langfristig")
  es = load.story("SimpleLabor3EqStory")
  init.story(es)
  set.dynry.step(t=2,es=es)

  app = shinyStoryApp(es)
  runEventsApp(app,launch.browser = rstudio::viewer)

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
  ui = fluidPage(title = es$storyId,ui)

  appInitHandler(initHandler = function(app,...) {
    restore.point("app.initHandler")
    app$es = es
  }, app=app)

  app$ui = ui
  show.story.part(es=es)
  app
}


edit.dynry.click = function(app=getApp(), es=app$es,...) {
  restore.point("edit.dynry.click")
  cat("Edit Story...")

}

dynry.wait.for.answer = function(app=getApp(), es=app$es) {
  restore.point("stNextBtnClicked")
  t=es$cur$t
  step.num=es$cur$step.num
  part = get.dynry.part(es=es,t=t, step.num=step.num)
  if (length(part$task)==0) {
    es$wait.for.answer = FALSE
    return(FALSE)
  }
  es$attempts = 0
  es$wait.for.answer = TRUE
  return(TRUE)
}

dynry.process.click.answer = function(app=getApp(), es=app$es,xy, pane.name,...) {
  restore.point("dynry.check.click.answer")
  res = check.click.answer(es = es,xy = xy,pane.name = pane.name)

  # correct
  if (res) {
    es$wait.for.answer = FALSE
    dynry.tell.part.sol()
  } else {
    es$attempts = es$attempts+1
    setUI(id = "answerUI",p(paste0("Not correct. (", es$attempts, " attempts.)")))
  }
}

shiny.pane.click = function(app=getApp(), es=app$es,pane.name,id, session, value,...) {
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
    dynry.next.btn.click()
  } else {
    dynry.process.click.answer(app = app,es=es,xy = xy,pane.name = pane.name)
  }
  #cat("Click!")
}


refresh.dynry.click = function(app=getApp(), es=app$es,...) {
  cat("Refresh dynry...")
  t = es$cur$t; step.num = es$cur$step.num
  id = es$storyId
  restore.point("refresh.dynry.click")

  # reload and init story
  es = load.story(id)
  app$es = es
  init.story(es)
  set.dynry.step(t=t, step.num=step.num, stage="start", es=es)
  show.story.part(es=es)

}


dynry.next.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("stNextBtnClicked")
  #cat("stNextBtn was clicked...")

  if (isTRUE(es$wait.for.answer)) {
    setUI(id = "answerUI",p(paste0("You have not yet answered the question.")))
    return()
  }

  res = dynry.next(es=es)
  if (res$end) return()
  show.story.part(es=es)
}

dynry.forward.btn.click = function(app=getApp(), es=app$es,...) {
  restore.point("dynry.forward.btn.click")

  dynry.forward(es, update.es=TRUE)

  show.story.part(es=es)
}


dynry.prev.btn.click = function(app=getApp(), es=app$es,...)  {
  restore.point("stPrevBtnClicked")
  res = dynry.prev(es=es,update.es = TRUE)
  if (res$start) return()
  show.story.part(es=es)
}



