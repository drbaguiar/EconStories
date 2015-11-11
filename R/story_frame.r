# frame stages
# start, complete, hint, failure

make.lag.object = function(obj, prefix = "lag_", ...) {
  restore.point("make.lag.object")

  if (is.null(obj)) return(NULL)

  sobj = obj
  sobj$name = paste0(prefix,obj$name)

  if (sobj$type == "curve") {
    vars = find.variables(obj$eq_)
    vars = setdiff(vars, c(obj$xvar,obj$yvar))
    subst = lapply(vars, function(var) {
      as.name(paste0(prefix, var))
    })
    names(subst) = vars
    fields = c("eq_","xformula_","yformula_","impl_")

    sobj[fields] = lapply(obj[fields], function(call) {
      if (is.null(call)) return(NULL)
      substitute.call(call, subst)
    })
    sobj$eq = deparse1(sobj$eq_)
  } else if (sobj$type == "marker") {
    sobj$var = paste0(prefix,obj$var)
  }
  args = list(...)
  sobj[names(args)] = args

  sobj
}

add.pane.lag.objects = function(pane,syms, prefix = "lag_") {
  restore.point("add.pane.lag.objects")

  if (length(prefix)>1) {
    for (pf in prefix)
      pane = add.pane.lag.objects(pane=pane, syms=syms, prefix=pf)
    return(pane)
  }


  rows = str.starts.with(syms,prefix)
  lsyms = syms[rows]
  syms = str.right.of(syms[rows],prefix)

  # Only keep symbols that have been defined for the pane
  contained = syms %in% names(pane$objs)
  lsyms = lsyms[contained]
  syms = syms[contained]


#   unknown = setdiff(syms,names(pane$objs))
#   if (length(unknown)>0) {
#     warning(paste0("The symbols ", paste0(unknown, collapse=", "), " are  not specified for pane ", pane$name))
#   }

  lag.objs = lapply(syms, function(sym) {
    make.lag.object(pane$objs[[sym]], prefix=prefix, color.level=2)
  })
  names(lag.objs) = lsyms
  if (is.null(pane$lag.objs)) {
    pane$lag.objs = lag.objs
  } else {
    pane$lag.objs[lsyms] = lag.objs
  }
  pane
}

compute.frame.pane.geoms = function(frame, pane, stage="start", values=current.story.values(es), es=NULL) {
  restore.point("compute.frame.pane.geoms")

  syms = frame[[paste0(stage,".symbols")]]

  pane = add.pane.lag.objects(pane=pane, syms=syms,prefix = "lag_")


  objs = c(pane$objs, pane$lag.objs)
  # Ideally we would not need to take this intersection
  syms = intersect(names(objs), syms)

  geoms = compute.pane.geoms(pane=pane, objs = objs[syms],values = values)
  geoms
}

plot.frame.pane = function(frame=es$cur$frame, pane=es$panes[[1]], values=current.story.values(es), es=NULL) {
  restore.point("plot.frame.pane")


  geoms = compute.frame.pane.geoms(frame=frame, pane=pane, values=values, es=es)

  plot.pane(pane, geoms=geoms)
}

plot.frame.panes = function(frame=es$cur$frame,app=getApp(), es=app$es, pane.names=names(es$panes),...) {
  restore.point("plot.frame.panes")
  lapply(es$panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PlotPane")
    setPlot(id = plotId, plot.frame.pane(es=es,pane=pane,frame=frame))
  })
  setText("plotCounter",sample(1:1000,1))
}

init.story.frame = function(frame, prev.frame=NULL, es=NULL) {
  restore.point("init.story.frame")

  if (is.null(frame$layout))
    frame$layout = prev.frame$layout

  frame$shown = c(frame$show, sc("lag_", frame$lagshow))

  if ("show" %in% frame$append)
    frame$shown = unique(c(frame$show, prev.frame$shown))


  frame$shown = setdiff(frame$shown,c(frame$hid, sc("lag_", frame$laghide)))
  frame$start.symbols = frame$hint.symbols = frame$failure.symbols = frame$shown

  frame$shown = unique(c(frame$shown, story.frame.task.symbols(frame)))


  frame$complete.symbols = frame$shown

  if (!is.null(frame$quiz)) {
    frame$quiz = shinyQuiz(id="storyQuiz",qu=frame$quiz,add.handler = FALSE)
    frame$quiz_html = paste0(as.character(p(frame$quiz$ui)), collapse="\n")
    frame$quiz_solved = paste0(as.character(p(quiz.ui(frame$quiz,solution = TRUE))), collapse="\n")
    #try(Encoding(frame$quiz_html) <- "UTF-8", silent=TRUE)
  }

  #try(Encoding(frame$tell) <- "UTF-8", silent=TRUE)
  #try(Encoding(frame$ask) <- "UTF-8", silent=TRUE)
  #try(Encoding(frame$success) <- "UTF-8", silent=TRUE)


  if ("tell" %in% frame$append) {
    if (!is.null(prev.frame$tell))
      frame$tell = paste0(prev.frame$tell, "\n", frame$tell)
  }


  if (!is.null(frame$task))
      frame$task$type = get.story.frame.task.type(frame)


  frame$has.question = (!is.null(frame$task)) | (!is.null(frame$quiz))

  frame$layout = make.frame.layout(frame = frame,es=es)

  #frame$ui = story.frame.ui(frame=frame,es = es)

  frame = as.environment(frame)

  frame
}

show.story.frame = function(frame=es$cur$frame, values=NULL, stage=es$cur$stage,cur=es$cur, es=NULL) {
  restore.point("show.story.frame")


  if (is.null(cur)) {
    cur = list(values=values, stage=stage, changed.frame=TRUE, changed.values=TRUE, changed.stage = TRUE)
  }

  if (cur$changed.frame | TRUE) {
    if (is.null(frame$ui))
      frame$ui = story.frame.ui(frame = frame, es=es)
    ui = frame$ui
    setUI("storyMainUI", ui)
  }

  if (cur$changed.frame | cur$changed.value | TRUE) {
    html = compile.story.txt(c(frame$tell,frame$ask), values=cur$values,out = "html")
    if (!is.null(cur$title)) {
      html = paste0("<h4>", cur$title, "</h4>", html)
    }
    if (!is.null(frame$quiz)) {
      if (stage=="complete") {
        html = paste0(html, frame$quiz_solved)
      } else {
        html = paste0(html, frame$quiz_html)
      }
    }
    setUI(id = "tellUI",HTML(html))
  }

  if (stage == "complete") {
    html = compile.story.txt(c(frame$success),values=cur$values,out = "html")
    setUI(id = "answerUI",HTML(html))
  } else {
    setUI(id = "answerUI",HTML(""))
  }

  if (cur$changed.frame | cur$changed.stage | cur$changed.values | TRUE)
    plot.frame.panes(es=es)
}


