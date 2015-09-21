make.lag.object = function(obj, prefix = "lag_", ...) {
  restore.point("make.lag.object")

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

compute.part.pane.geoms = function(part, pane, stage="start", params=current.story.params(es), es=NULL) {
  restore.point("compute.part.pane.geoms")

  syms = part[[paste0(stage,".symbols")]]

  pane = add.pane.lag.objects(pane=pane, syms=syms,prefix = "lag_")


  objs = c(pane$objs, pane$lag.objs)
  # Ideally we would not need to take this intersection
  syms = intersect(names(objs), syms)

  geoms = compute.pane.geoms(pane=pane, objs = objs[syms],params = params)
  geoms
}

plot.part.pane = function(part=es$part, pane=es$panes[[1]], params=current.story.params(es), es=NULL) {
  restore.point("plot.part.pane")


  geoms = compute.part.pane.geoms(part=part, pane=pane, params=params, es=es)

  plot.pane(pane, geoms=geoms)
}
