
examples.dynry = function() {
  set.restore.point.options(display.restore.point = !TRUE)

  setwd("D:/libraries/EconStories/EconStories")
  ES = initEconStories()
  es = load.story("SimpleLabor3EqStory")
  init.story(es)

  step.num = 0
  step.num = step.num +1
  set.dynry.step(t=2, step.num=step.num, es=es)
  plot.frame.pane(es=es)

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

  init.dynry.frames(es)

  set.dynry.step(t=1,es=es)
}

dynry.tframes = function(es,t=es$cur$t) {
  row = which(es$tframes.df$start.t <= t & t <= es$tframes.df$end.t)
  es$tframes.df[row,]
}

set.dynry.next.frame = function(es, t=es$cur$t, step.num=es$cur$step.num, update.es=TRUE) {
  restore.point("set.dynry.next.frame")

  tframes = dynry.tframes(es,t)

  if (step.num < tframes$num.frames) {
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


set.dynry.prev.frame = function(es, t=es$cur$t, step.num=es$cur$step.num, update.es=TRUE) {
  restore.point("set.dynry.prev.frame")

  start = FALSE
  if (step.num > 1) {
    step.num = step.num-1
  } else {
    if (t>1) {
      t = t-1
      tframes = dynry.tframes(es,t)
      step.num = tframes$num.frames
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


set.dynry.forward.frame = function(es, t = es$cur$t, step.num = es$cur$step.num, update.es=TRUE) {
  restore.point("set.dynry.forward.frame")

  tframes = dynry.tframes(es,t)
  if (tframes$row == NROW(es$tframes.df)) {
    t = es$T
    step.num = tframes$num.frames
    solved = TRUE
  } else {
    row = tframes$row+1
    t = es$tframes.df$start.t[[row]]
    step.num = 1
    solved = FALSE
  }
  if (update.es) {
    set.dynry.step(t=t, step.num=step.num, solved=TRUE, es=es)
  }

  return(list(t=t, step.num = step.num))
}


init.dynry.frames = function(es) {
  restore.point("init.dynry.frames")
  #period = es$periods[[2]]
  prev.frame = list(t=0, shown=NULL)
  step.num = 0
  frame.ind = 0
  frame.names = names(es$frames)
  frames = vector("list",length(es$frames))

  i = 1
  hvals = list(t=1,section=NULL)

  while(TRUE) {
    if (i>length(es$frames)) break
    name = frame.names[i]

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
    same.t = frame$t == prev.frame$t
    if (same.t) {
      step.num = step.num +1
    } else {
      step.num = 1
    }

    # If is frame of the same period,
    # by default copy shown curves
    if (is.null(frame$append)) {
      if (same.t)
        frame$append = c("show")
    } else if (frame$append[1]=="all") {
      frame$append = c("show","tell")
    }


    frame =init.story.frame(frame=frame, prev.frame = prev.frame, es=es)

    frames[[frame.ind]] = frame
    names(frames)[frame.ind] = name
    prev.frame = frame
    i = i+1
  }

  es$frames = frames[1:frame.ind]

  t.vec = sapply(es$frames, function(frame) frame$t)

  start.t = unique(t.vec)
  end.t = c(start.t[-1]-1, Inf)
  frames = lapply(start.t, function(t) which(t.vec==t))
  num.frames = sapply(frames, function(ps) length(ps))

  es$tframes.df = data_frame(row=seq_along(start.t),start.t = start.t, end.t = end.t, frames = frames, num.frames=num.frames)

  es
}


set.dynry.step = function(t=cur$t, step.num=cur$step.num, solved=cur$solved, stage=cur$stage, es, cur=list(step.num=1, solved=FALSE)) {
  restore.point("set.dynry.step")

  es$prev = prev = es$cur

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

  cur$frame.ind =get.dynry.frame.ind(es=es, t=t, step.num=step.num)
  cur$frame = es$frames[[cur$frame.ind]]
  cur$solved = solved

  cur$wait.for.answer  = !(length(cur$frame$task)==0 | cur$solved)

  cur$changed.frame = !identical(cur$frame.ind, prev$frame.ind)
  cur$changed.t = !identical(cur$t, prev$t)
  cur$changed.stage = !identical(cur$stage, prev$stage)

  cur$values = current.story.values(es=es,cur=cur)
  cur$changed.values = !identical(prev$values, cur$values)


  es$cur = cur
}

get.dynry.frame.ind = function(es, t=es$cur$t, step.num = es$cur$step.num) {
  row = which(es$tframes.df$start.t <= t & t <= es$tframes.df$end.t)
  es$tframes.df$frame[[row]][step.num]
}


get.dynry.frame = function(es, t=es$cur$t, step.num = es$cur$step.num) {
  restore.point("get.dynry.frame")
  row = which(es$tframes.df$start.t <= t & t <= es$tframes.df$end.t)
  frame.ind = es$tframes.df$frames[[row]][step.num]
  es$frames[[frame.ind]]
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

