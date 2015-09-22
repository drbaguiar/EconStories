
examples.dynry = function() {
  set.restore.point.options(display.restore.point = !TRUE)

  setwd("D:/libraries/EconStories/EconStories")
  ES = initEconStories()
  es = load.story("SimpleLabor3EqStory")
  init.story(es)

  step.num = 0
  step.num = step.num +1
  set.dynry.step(t=2, step.num=step.num, es=es)
  plot.part.pane(es=es)

  #dyplot.timelines(em$sim,cols = c(em$var.names,"A"),em = em)

}


init.dynry = function(es, em=es$em, sim=NULL) {
  restore.point("init.dynry")


  # Initialize and simulate model

  if (!is.null(em)) {
    init.model(em)
    if (is.null(es$scenario))
      es$scenario = em$scenarios[[es$scenarioId]]
    init.model.scen(em,scen = es$scenario)
    simulate.model(em,scen = es$scenario,init.scen = FALSE)
    es$sim = em$sim
  } else {
    es$sim = sim
  }

  es$T = NROW(es$sim)
  es$tol = 0.07


  es$curves = init.curves(es$curves)
  panes = lapply(es$panes, init.story.pane, es=es)
  es$panes = c(lapply(em$panes, as.environment), panes)

  init.dynry.parts(es)

  set.dynry.step(t=1,es=es)
}

dynry.tparts = function(es,t=es$cur$t) {
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  es$tparts.df[row,]
}

dynry.next = function(es, t=es$cur$t, step.num=es$cur$step.num, update.es=TRUE) {
  restore.point("dynry.next")

  tparts = dynry.tparts(es,t)

  if (step.num < tparts$num.parts) {
    step.num = step.num+1
  } else {
    if (t<es$T) {
      t = t+1
      step.num = 1
    } else {
      return(list(t=t, step.num=1, end=TRUE))
    }
  }

  if (update.es) {
    set.dynry.step(t=t, step.num=step.num, solved=FALSE, es=es)
  }
  return(list(t=t, step.num=step.num, end=FALSE))
}


dynry.prev = function(es, t=es$cur$t, step.num=es$cur$step.num, update.es=TRUE) {
  restore.point("dynry.prev")

  start = FALSE
  if (step.num > 1) {
    step.num = step.num-1
  } else {
    if (t>1) {
      t = t-1
      tparts = dynry.tparts(es,t)
      step.num = tparts$num.parts
    } else {
      t = 1
      step.num = 1
      start = TRUE
    }
  }

  if (update.es) {
    set.dynry.step(t=t, step.num=step.num, solved=TRUE, es=es)
  }

  return(list(t=t, step.num=step.num, start=start))
}


dynry.forward = function(es, t = es$cur$t, step.num = es$cur$step.num, update.es=TRUE) {
  restore.point("dynry.forward")

  tparts = dynry.tparts(es,t)
  if (tparts$row == NROW(es$tparts.df)) {
    t = es$T
    step.num = tparts$num.parts
    solved = TRUE
  } else {
    row = tparts$row+1
    t = es$tparts.df$start.t[[row]]
    step.num = 1
    solved = FALSE
  }
  if (update.es) {
    set.dynry.step(t=t, step.num=step.num, solved=TRUE, es=es)
  }

  return(list(t=t, step.num = step.num))
}


init.dynry.parts = function(es) {
  restore.point("init.dynry.parts")
  #period = es$periods[[2]]
  prev.part = list(t=0, shown=NULL)
  step.num = 0
  part.ind = 0
  part.names = names(es$parts)
  parts = vector("list",length(es$parts))

  i = 1
  hvals = list(t=1,section=NULL)

  while(TRUE) {
    if (i>length(es$parts)) break
    name = part.names[i]

    if (str.starts.with(name,"Period ")) {
      i = i+1
      hvals$t = as.numeric(str.right.of(name, "Period "))
      next
    }

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
    same.t = part$t == prev.part$t
    if (same.t) {
      step.num = step.num +1
    } else {
      step.num = 1
    }

    # If is part of the same period,
    # by default copy shown curves
    if (is.null(part$append)) {
      if (same.t)
        part$append = c("show")
    } else if (part$append[1]=="all") {
      part$append = c("show","tell")
    }


    part =init.story.part(part=part, prev.part = prev.part, es=es)

    parts[[part.ind]] = part
    names(parts)[part.ind] = name
    prev.part = part
    i = i+1
  }

  es$parts = parts[1:part.ind]

  t.vec = sapply(es$parts, function(part) part$t)

  start.t = unique(t.vec)
  end.t = c(start.t[-1]-1, Inf)
  parts = lapply(start.t, function(t) which(t.vec==t))
  num.parts = sapply(parts, function(ps) length(ps))

  es$tparts.df = data_frame(row=seq_along(start.t),start.t = start.t, end.t = end.t, parts = parts, num.parts=num.parts)

  es
}


set.dynry.step = function(t, step.num=1, solved=FALSE, stage=NULL, es) {
  restore.point("set.dynry.step")

  es$prev = prev = es$cur

  cur = list()

  if (is.null(stage)) {
    if (!solved) {
      cur$stage = "start"
    } else {
      cur$stage = "complete"
    }
  }

  cur$t = t
  cur$sim.row = t
  cur$step.num = step.num

  cur$part.ind =get.dynry.part.ind(es=es, t=t, step.num=step.num)
  cur$part = es$parts[[cur$part.ind]]
  cur$solved = solved

  cur$wait.for.answer  = !(length(cur$part$task)==0 | cur$solved)

  cur$changed.part = !identical(cur$part.ind, prev$part.ind)
  cur$changed.t = !identical(cur$t, prev$t)
  cur$changed.stage = !identical(cur$stage, prev$stage)

  cur$values = current.story.values(es=es,cur=cur)
  cur$changed.values = !identical(prev$values, cur$values)


  es$cur = cur
}

get.dynry.part.ind = function(es, t=es$cur$t, step.num = es$cur$step.num) {
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  es$tparts.df$part[[row]][step.num]
}


get.dynry.part = function(es, t=es$cur$t, step.num = es$cur$step.num) {
  restore.point("get.dynry.part")
  row = which(es$tparts.df$start.t <= t & t <= es$tparts.df$end.t)
  part.ind = es$tparts.df$parts[[row]][step.num]
  es$parts[[part.ind]]
}

dynry.step.dyplot = function(es, t, step, solved=FALSE, previous.steps=TRUE, pane.names = names(es$em$panes), vars = names(es$em$vars)) {
  restore.point("dynry.step.dyplot")

  em = es$em
  sim = em$sim
  sim = sim[,c("t",vars)]

  #if (t==1) return()

  if (t<NROW(sim)) {
    sim[(t+1):NROW(sim),vars]=NA
  }
  symbols = get.dynry.step.symbols(es=es,t=t,step.num=step, solved=solved, previous.steps=previous.steps)

  t.vars = intersect(symbols, vars)
  hidden.vars = setdiff(vars, t.vars)
  if (length(hidden.vars)>0) {
    sim[t,hidden.vars] = NA
  }
  sim
  #cat("\nRefresh timeline: t=",t, " vars = ", paste0(t.vars, collapse=","))
  dyplot.timelines(sim,cols = vars,em = em)

}

get.task.pane = function(es, task) {
  if (!is.null(task$pane)) return(task$pane)
  return(NULL)
}

