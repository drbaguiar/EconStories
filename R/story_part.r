# Part stages
# start, complete, hint, failure

make.lag.object = function(obj, prefix = "lag_", ...) {
  restore.point("make.lag.object")

  if (is.null(obj)) return(NULL)

  sobj = obj
  sobj$name = paste0(prefix,obj$name)

  if (sobj$type == "curve") {
    vars = find.variables(sobj$eq)
    subst = paste0(prefix, vars)
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

compute.part.pane.geoms = function(part, pane, stage="start", values=current.story.values(es), es=NULL) {
  restore.point("compute.part.pane.geoms")

  syms = part[[paste0(stage,".symbols")]]

  pane = add.pane.lag.objects(pane=pane, syms=syms,prefix = "lag_")


  objs = c(pane$objs, pane$lag.objs)
  # Ideally we would not need to take this intersection
  syms = intersect(names(objs), syms)

  geoms = compute.pane.geoms(pane=pane, objs = objs[syms],values = values)
  geoms
}

plot.part.pane = function(part=es$cur$part, pane=es$panes[[1]], values=current.story.values(es), es=NULL) {
  restore.point("plot.part.pane")


  geoms = compute.part.pane.geoms(part=part, pane=pane, values=values, es=es)

  plot.pane(pane, geoms=geoms)
}

plot.part.panes = function(part=es$cur$part,app=getApp(), es=app$es, pane.names=names(es$panes),...) {
  restore.point("plot.part.panes")
  lapply(es$panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PanePlot")
    setPlot(id = plotId, plot.part.pane(es=es,pane=pane,part=part))
  })
  setText("plotCounter",sample(1:1000,1))
}

init.story.part = function(part, prev.part=NULL, es=NULL) {
  restore.point("init.story.part")

  part$shown = c(part$show, sc("lag_", part$lagshow))

  if ("show" %in% part$append)
    part$shown = unique(c(part$show, prev.part$shown))


  part$shown = setdiff(part$shown,c(part$hid, sc("lag_", part$laghide)))
  part$start.symbols = part$hint.symbols = part$failure.symbols = part$shown

  part$shown = unique(c(part$shown, story.part.task.symbols(part)))


  part$complete.symbols = part$shown

  part$has.rhs = length(part$shown)>0

  try(Encoding(part$tell) <- "UTF-8", silent=TRUE)
  try(Encoding(part$ask) <- "UTF-8", silent=TRUE)
  try(Encoding(part$success) <- "UTF-8", silent=TRUE)


  if ("tell" %in% part$append) {
    if (!is.null(prev.part$tell))
      part$tell = paste0(prev.part$tell, "\n", part$tell)
  }


  if (!is.null(part$task))
      part$task$type = get.story.part.task.type(part)

  part$layout = make.part.layout(part = part,es=es)

  #part$ui = story.part.ui(part=part,es = es)

  part = as.environment(part)

  part
}

show.story.part = function(part=es$cur$part, values=NULL, stage="start",cur=es$cur, es=NULL) {
  restore.point("show.story.part")


  if (is.null(cur)) {
    cur = list(values=values, stage=stage, changed.part=TRUE, changed.values=TRUE, changed.stage = TRUE)
  }

  if (cur$changed.part | TRUE) {
    if (is.null(part$ui))
      part$ui = story.part.ui(part = part, es=es)
    ui = part$ui
    setUI("storyMainUI", ui)
  }

  if (cur$changed.part | cur$changed.value | TRUE) {
    html = compile.story.txt(c(part$tell,part$ask), values=cur$values,out = "html")
    if (!is.null(cur$title)) {
      html = paste0("<h4>", cur$title, "</h4>", html)
    }
    setUI(id = "tellUI",HTML(html))
  }

  if (stage == "complete") {
    html = compile.story.txt(c(part$success),values=cur$values,out = "html")
    setUI(id = "answerUI",HTML(html))
  } else {
    setUI(id = "answerUI",HTML(""))
  }

  if (cur$changed.part | cur$changed.stage | cur$changed.values | TRUE)
    plot.part.panes(es=es)
}


