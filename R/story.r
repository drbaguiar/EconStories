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

set.story.cur = function(es, cur=es$cur) {
  es$cur = cur
}



init.free.story = function(es) {
  init.free.story.parts(es=es)
  set.story.part(part.ind=1, es=es)
}


init.free.story.parts = function(es) {
  restore.point("init.free.story.parts")

  prev.part = list(shown=NULL)
  part.ind = 0
  part.names = names(es$parts)
  parts = vector("list",length(es$parts))

  i = 1
  hvals = list(section=NULL)

  while(TRUE) {
    if (i>length(es$parts)) break
    name = part.names[i]

    if (str.starts.with(name,"Section ")) {
      i = i +1
      hvals$section = str.right.of(name, "Section ")
      next
    }

    part = es$parts[[i]]
    part.ind = part.ind+1

    for (var in names(hvals)) {
      if (is.null(part[[var]])) {
        if (!is.null(hvals[[var]])) {
          part[[var]] = hvals[[var]]
        } else {
          part[[var]] = prev.part[[var]]
        }
      }
    }
    hvals = lapply(hvals, function(val) NULL)

    if (!is.null(part$append)) {
      if (part$append[[1]]=="all") {
        part$append = c("show","tell")
      }
    }

    part =init.story.part(part=part, prev.part = prev.part, es=es)

    parts[[part.ind]] = part
    names(parts)[part.ind] = name
    prev.part = part
    i = i+1
  }

  es$parts = parts[1:part.ind]

  es

}

set.story.next.part = function(es, part.ind = es$cur$part.ind, update.es=TRUE) {
  restore.point("set.story.next.part")
  part.ind = min(part.ind+1, length(es$parts))
  if (update.es) {
    set.story.part(part.ind=part.ind, es=es)
  }
  return(list(part.ind=part.ind, end= part.ind==length(es$parts) ))
}


set.story.prev.part = function(es, part.ind = es$cur$part.ind, update.es=TRUE) {
  restore.point("set.story.prev.part")
  part.ind = max(part.ind-1, 1)
  if (update.es) {
    set.story.part(part.ind=part.ind, es=es)
  }
  return(list(part.ind=part.ind, start=part.ind==1))
}


set.story.forward.part = function(es, part.ind = es$cur$part.ind, update.es=TRUE) {
  restore.point("set.story.forward.part")
  part.ind = min(part.ind+1, length(es$parts))
  if (update.es) {
    set.story.part(part.ind=part.ind, es=es)
  }
  return(list(part.ind=part.ind, end= part.ind==length(es$parts) ))
}



set.story.part = function(part.ind,es, solved=FALSE, stage=NULL) {
  restore.point("set.story.part")

  es$prev = prev = es$cur

  cur = list()

  if (is.null(stage)) {
    if (!solved) {
      cur$stage = "start"
    } else {
      cur$stage = "complete"
    }
  }

  cur$part.ind = part.ind
  cur$part = es$parts[[cur$part.ind]]
  cur$solved = solved

  cur$wait.for.answer  = !(length(cur$part$task)==0 | cur$solved)

  cur$changed.part = !identical(cur$part.ind, prev$part.ind)
  cur$changed.stage = !identical(cur$stage, prev$stage)

  cur$values = current.story.values(es=es,cur=cur)
  cur$changed.values = !identical(prev$values, cur$values)

  es$cur = cur
}
