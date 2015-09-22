init.story = function(es, em=NULL) {
  restore.point("init.story")

  if (is.null(em))
    em = load.model(es$modelId)
  es$em = em
  init.model(em = es$em)


  if (is.null(es[["storyType"]])) es$storyType = "dynamics"

  if (es$storyType=="dynamics") {
    init.dynry(es,em=em)
  } else if (es$storyType=="scenarios") {
    init.scenry(es,em=em)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
}



add.story.button.handlers = function() {
  buttonHandler("stNextBtn", dynry.next.btn.click,if.handler.exists = "skip")
  buttonHandler("stForwardBtn", dynry.forward.btn.click,if.handler.exists = "skip")
  buttonHandler("stPrevBtn", dynry.prev.btn.click,if.handler.exists = "skip")
  buttonHandler("stExitBtn", exit.to.main,if.handler.exists = "skip")
  buttonHandler("stEditBtn", edit.dynry.click,if.handler.exists = "skip")
  buttonHandler("stRefreshBtn", refresh.dynry.click,if.handler.exists = "skip")
}


story.ui = function(app=getApp(), es=app$es) {
  restore.point("story.ui")
  add.story.button.handlers()
  uiOutput("storyMainUI")

}

run.story = function(es) {
  restore.point("run.story")
  if (es$storyType=="dynamics") {
    show.story.part(es=es)
  } else if (es$storyType=="scenarios") {
    # No need to run if ui is shown
    show.story.part(es=es)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
}


get.story.part.task.type = function(part) {
  restore.point("get.story.step.task.type")
  types = c("find","shift","select","findPoint")
  not.null = which(sapply(types, function(type) {
    !is.null(part$task[[type]])
  }))
  if (length(not.null)==0) return("unknown")

  types[not.null]
}


story.part.task.symbols = function(part) {
  restore.point("step.task.symbols")
  tasks = setdiff(names(part$task),c("pane","type") )
  symbols = unique(unlist(lapply(part$task[tasks], function(ta) ta$symbol)))
  symbols
}

init.story.pane = function(pane, curves = c(es$curves,es$em$curves),es=NULL,...) {
  restore.point("init.story.pane")

  as.environment(init.model.pane(pane=pane, curves=curves,...))
}

current.story.values = function(es, cur=es$cur) {
  as.list(es$sim[cur$sim.row,])
}
