init.story = function(es, em=NULL) {
  restore.point("init.story")

  if (is.null(em) & !is.null(es$modelId)) {
    em = load.model(es$modelId)
    es$em = em
  }
  if (!is.null(es$em)) {
    init.model(em = es$em)
  }


  if (is.null(es[["storyType"]])) {
    if (!is.null(es$em)) {
      es$storyType = "dynamics"
    } else {
      es$storyType = "free"
    }
  }
  if (es$storyType=="dynamics") {
    init.dynry(es,em=em)
  } else if (es$storyType=="scenarios") {
    init.scenry(es,em=em)
  } else if (es$storyType=="free") {
    init.free.story(es=es)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
}



run.story = function(es) {
  restore.point("run.story")
  if (es$storyType=="dynamics") {
    show.story.frame(es=es)
  } else if (es$storyType=="scenarios") {
    # No need to run if ui is shown
    show.story.frame(es=es)
  } else {
    stop(paste0("Unknown storyType ", es$storyType, " specified in story file." ))
  }
}


get.story.frame.task.type = function(frame) {
  restore.point("get.story.step.task.type")
  types = c("find","shift","select","findPoint")
  not.null = which(sapply(types, function(type) {
    !is.null(frame$task[[type]])
  }))
  if (length(not.null)==0) return("unknown")

  types[not.null]
}


story.frame.task.symbols = function(frame) {
  restore.point("step.task.symbols")
  tasks = setdiff(names(frame$task),c("pane","type") )
  symbols = unique(unlist(lapply(frame$task[tasks], function(ta) ta$symbol)))
  symbols
}

init.story.pane = function(pane, curves = c(es$curves,es$em$curves),es=NULL,...) {
  restore.point("init.story.pane")

  as.environment(init.model.pane(pane=pane, curves=curves,...))
}

current.story.values = function(es, cur=es$cur) {
  as.list(es$sim[cur$sim.row,])
}

set.story.cur = function(es, cur=es$cur) {
  es$cur = cur
}



init.free.story = function(es) {
  init.free.story.frames(es=es)
  set.story.frame(frame.ind=1, es=es)
}


init.free.story.frames = function(es) {
  restore.point("init.free.story.frames")

  prev.frame = list(shown=NULL)
  frame.ind = 0
  frame.names = names(es$frames)
  frames = vector("list",length(es$frames))

  i = 1
  hvals = list(section=NULL)

  while(TRUE) {
    if (i>length(es$frames)) break
    name = frame.names[i]

    if (str.starts.with(name,"Section ")) {
      i = i +1
      hvals$section = str.right.of(name, "Section ")
      next
    }

    frame = es$frames[[i]]
    frame.ind = frame.ind+1

    for (var in names(hvals)) {
      if (is.null(frame[[var]])) {
        if (!is.null(hvals[[var]])) {
          frame[[var]] = hvals[[var]]
        } else {
          frame[[var]] = prev.frame[[var]]
        }
      }
    }
    hvals = lapply(hvals, function(val) NULL)

    if (!is.null(frame$append)) {
      if (frame$append[[1]]=="all") {
        frame$append = c("show","tell")
      }
    }

    frame =init.story.frame(frame=frame, prev.frame = prev.frame, es=es)

    frames[[frame.ind]] = frame
    names(frames)[frame.ind] = name
    prev.frame = frame
    i = i+1
  }

  es$frames = frames[1:frame.ind]

  es

}

set.story.next.frame = function(es, frame.ind = es$cur$frame.ind, update.es=TRUE) {
  restore.point("set.story.next.frame")
  frame.ind = min(frame.ind+1, length(es$frames))
  if (update.es) {
    set.story.frame(frame.ind=frame.ind, es=es)
  }
  return(list(frame.ind=frame.ind, end= frame.ind==length(es$frames) ))
}


set.story.prev.frame = function(es, frame.ind = es$cur$frame.ind, update.es=TRUE) {
  restore.point("set.story.prev.frame")
  frame.ind = max(frame.ind-1, 1)
  if (update.es) {
    set.story.frame(frame.ind=frame.ind, es=es)
  }
  return(list(frame.ind=frame.ind, start=frame.ind==1))
}


set.story.forward.frame = function(es, frame.ind = es$cur$frame.ind, update.es=TRUE) {
  restore.point("set.story.forward.frame")
  frame.ind = min(frame.ind+1, length(es$frames))
  if (update.es) {
    set.story.frame(frame.ind=frame.ind, es=es)
  }
  return(list(frame.ind=frame.ind, end= frame.ind==length(es$frames) ))
}



set.story.frame = function(frame.ind,es, solved=FALSE, stage=NULL) {
  restore.point("set.story.frame")

  es$prev = prev = es$cur

  cur = list()

  if (is.null(stage)) {
    if (!solved) {
      cur$stage = "start"
    } else {
      cur$stage = "complete"
    }
  }

  cur$frame.ind = frame.ind
  cur$frame = es$frames[[cur$frame.ind]]
  cur$solved = solved

  cur$wait.for.answer  = !(length(cur$frame$task)==0 | cur$solved)

  cur$changed.frame = !identical(cur$frame.ind, prev$frame.ind)
  cur$changed.stage = !identical(cur$stage, prev$stage)

  cur$values = current.story.values(es=es,cur=cur)
  cur$changed.values = !identical(prev$values, cur$values)

  es$cur = cur
}
