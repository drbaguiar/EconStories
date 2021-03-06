default.layout = function() {
  list(
    left_cols = 4,
    left_margin = 0,
    right_margin = 0,
    plot_height = "250px",
    plot_width = "250px",
    portrait_height = "100px"
  )
}

make.frame.layout = function(frame,layout=list(), fill.layouts = list(frame$layout,es$layout, default.layout()),  es) {
  restore.point("make.frame.layout")


  set.missing = function(...) {
    layout <<- copy.into.missing.fields(dest=layout, source=list(...))
  }

  for (playout in fill.layouts) {
    layout = copy.into.missing.fields(dest=layout, source=playout )
  }

  set.missing(showpanes = names(es$panes))

  layout$showpanes = as.character(layout$showpanes)

  layout$num.plots = length(layout$showpanes)
  layout$num.images = length(frame$images)
  layout$num.portraits = length(frame$portraits)

  layout$num.rhs = layout$num.plots + layout$num.images + layout$num.portraits


#  if (isTRUE(!frame$has.rhs))
#    set.none.null(left_cols=10, left_margin=1, right_margin=1)

  set.missing(pane.cols = max(1,min(2, layout$num.rhs)))


  layout
}

story.frame.ui = function(frame=NULL,layout = frame$layout, allow.edit=getES()$allow.edit, es) {
  restore.point("story.frame.ui")

  right.cols = 12 - layout$left_cols - layout$left_margin - layout$right_margin

  if (isTRUE(allow.edit)) {
    #editBtn = actionButton("stEditBtn","Edit")
    editBtn = NULL
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
    if (layout$left_cols>=3) {
      layout$button.pos = "top-left"
    } else {
      layout$button.pos = "top"
    }
  }

  get.buttons = function(pos) {
    if (pos == layout$button.pos) return(button.row)
    return(NULL)
  }

  if (layout$left_cols >0) {
    left.col =  column(
      width = layout$left_cols, offset=layout$left_margin,
      get.buttons("top-left"),
      uiOutput("tellUI"),
      uiOutput("answerUI"),
      story.portraits.ui(frame=frame, es=es),
      get.buttons("bottom-left")
    )
  } else {
    left.col = NULL
  }

  if (right.cols >0) {
    right.col =  column(
      width = right.cols,
      get.buttons("top-right"),
      story.frame.rhs.ui(frame=frame,es=es),
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

story.portraits.ui = function(frame,es) {
  layout = frame$layout

  portraits.li = lapply.with.name(frame$portraits, function(image, name) {
    file.name = image
    str = paste0(
    '<figure>
       <img src="',file.name,'" alt="', name,'" style="height:',layout$portrait_height,';width:auto;">
      <figcaption>',name,'</figcaption>
    </figure>'
    )

    HTML(paste0(str))
  })
  if (length(portraits.li)==0) return(NULL)
  portraits.li
}

story.frame.rhs.ui = function(frame, es) {
  restore.point("story.frame.rhs.ui")

  layout = frame$layout

  panes = es$panes[layout$showpanes]

  plot.li = lapply(panes, function(pane) {
    id = paste0(pane$name,"_PlotPane")
    clickId = paste0(pane$name,"_PlotPaneClick")
    changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = id,click = clickId, width=layout$plot_width,height=layout$plot_height)
  })

  images.li = lapply.with.name(frame$images, function(image, name) {
    make.frame.image.ui(frame=frame,image=image, name=name)
  })
  li = c(plot.li, images.li)

  names(li) = NULL
  if (length(li) == 0) return(NULL)
  ui = HTML(html.table(li,ncol=layout$pane.cols))
  #ui = do.call(fluidRow, li)
  ui


}

make.frame.image.ui = function(frame, image,name, caption=NULL, layout=frame$layout) {
  restore.point("make.frame.image.ui")

  file.name = get.story.image.file(image = image)

#   local = !str.starts.with(file.name,"http")
#   if (local) {
#     file.name = paste0(getES()$stories.path,"/images/",file.name)
#     id = paste0(name,"_ImagePane")
#     clickId = paste0(name,"_ImagePaneClick")
#     ui = imageOutput(outputId = id, height="auto", width="auto")
#     try(getApp()$session$output[[id]] <- renderImage(list(src=file.name, alt=name),deleteFile=FALSE))
#     #setImage(id, list(src=file.name, alt=name))
#     ui
#   } else {
#     ui = HTML(paste0('<img src="',file.name,'" alt="', name,'">'))
#
#   }

  height = "400px"
  width = "auto"
  style =  paste0('style="height: ',height,'";width: ', width,';"')
  if (!is.null(caption)) {


    ui = paste0('<figure>
       <img src="',file.name,'" alt="', name,'" ', style,'>
      <figcaption>',name,'</figcaption>
    </figure>'
    )
  } else {
    ui = HTML(paste0('<img src="',file.name,'" alt="', name,'" ', style,'>'))

  }


  ui
}

get.story.image.file = function(image) {
  restore.point("get.story.image.file")

  if (!is.list(image)) {
    file = image
  } else {
    file = image$file
  }
  if (!str.starts.with(file,"http")) {
    file = paste0("images/",file)
  }
  file
}

story.panes.ui = function(panes = es$panes, es) {
  restore.point("story.panes.ui")
  pane.names = names(panes)


  li = lapply(panes[pane.names], function(pane) {
    plotId = paste0(pane$name,"_PlotPane")
    clickId = paste0(pane$name,"_PlotPaneClick")
    changeHandler(id=clickId, shiny.pane.click, pane.name=pane$name)
    plotOutput(outputId = plotId,click = clickId, width="250px",height="250px")
  })
  names(li) = NULL
  ui = HTML(html.table(li,ncol=2))
  #ui = do.call(fluidRow, li)
  ui
}





