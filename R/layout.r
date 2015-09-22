default.layout = function() {
  list(
    left.cols = 4,
    left.margin = 0,
    right.margin = 0
  )
}

make.part.layout = function(part,layout=list(), fill.layouts = list(part$layout,es$layout, default.layout()),  es=NULL) {
  restore.point("make.part.layout")

  set.none.null = function(...) {
    layout <<- copy.into.null.fields(dest=layout, source=list(...))
  }

  if (isTRUE(!part$has.rhs))
    set.none.null(left.cols=10, left.margin=1, right.margin=1)


  for (playout in fill.layouts) {
    layout = copy.into.null.fields(dest=layout, source=playout )
  }
  layout
}

story.part.ui = function(part=NULL,layout = part$layout, allow.edit=es$allow.edit, es) {
  restore.point("story.part.ui")

  right.cols = 12 - layout$left.cols - layout$left.margin - layout$right.margin

  if (isTRUE(allow.edit)) {
    editBtn = actionButton("stEditBtn","Edit")
    refreshBtn = actionButton("stRefreshBtn","Refresh")
  } else {
    editBtn = refreshBtn = NULL
  }
  button.row = fluidRow(
      actionButton("stPrevBtn","<"),
      actionButton("stNextBtn",">"),
      actionButton("stForwardBtn",">>"),
      actionButton("stExitBtn","Exit"),
      editBtn, refreshBtn
  )
  if (is.null(layout$button.pos)) {
    if (layout$left.cols>=3) {
      layout$button.pos = "top-left"
    } else {
      layout$button.pos = "top"
    }
  }

  get.buttons = function(pos) {
    if (pos == layout$button.pos) return(button.row)
    return(NULL)
  }

  if (layout$left.cols >0) {
    left.col =  column(
      width = layout$left.cols, offset=layout$left.margin,
      get.buttons("top-left"),
      uiOutput("tellUI"),
      uiOutput("answerUI"),
      get.buttons("bottom-left")
    )
  } else {
    left.col = NULL
  }

  if (right.cols >0) {
    right.col =  column(
      width = right.cols,
      get.buttons("top-right"),
      story.panes.ui(es=es),
      get.buttons("bottom-right")
    )
  } else {
    right.col = NULL
  }
  ui = fluidRow(
    get.buttons("top"),
    left.col,
    right.col,
    get.buttons("bottom")
  )
  #setPlot("testPlot",{plot(runif(100),runif(100))})
  #setUI(id = "panesUI",dynry.panes.ui())


  ui
}



story.panes.ui = function(panes = es$panes, es) {
  restore.point("story.panes.ui")
  pane.names = names(panes)
  li = lapply(panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PanePlot")
    clickId = paste0(pane$name,"_PaneClick")
    changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = plotId,click = clickId, width="250px",height="250px")
  })
  names(li) = NULL
  ui = HTML(html.table(li,ncol=2))
  #ui = do.call(fluidRow, li)
  ui
}

