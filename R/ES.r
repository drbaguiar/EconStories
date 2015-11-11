..ES = new.env()

initEconStories = function(
    stories.path = paste0(getwd(),"/stories"),
    models.path = paste0(getwd(),"/models"),
    types.path = paste0(path.package("EconStories", quiet = TRUE),"/yamltypes"),
    allow.edit=TRUE
) {
  restore.point("initEconModels")
  options(stringsAsFactors = FALSE)
  ES = ..ES
  ES$types.path = types.path
  ES$stories.path = stories.path

  initEconModels(models.path=models.path)
  ES$models.path = models.path

  ES$types = load.yaml.types(types.path=types.path)
  ES$allow.edit = allow.edit
  ES
}

getES = function() {
  app = try(getApp(),silent=TRUE)
  if (!is(app,"try-error")) {
    if (!is.null(app[["..ES"]]))
      return(app[["..ES"]])
  }
  ..ES
}



# check the package for bugs
check.EconStories = function() {
  txt = NULL
  codetools::checkUsagePackage("EconStories",
    report = function(str)  txt <<- c(txt,str),
    suppressLocalUnused = TRUE
  )
  txt
  rows = str.starts.with(txt,"examples.")
  rows = rows | has.substr(txt,"no visible global function definition")
  txt = txt[!rows]
  cat(paste0(1:NROW(txt),": ", txt, collapse="\n"))
  invisible(txt)
}




load.story = function(storyId, file=paste0(storyId,".yaml"), dir=getES()$stories.path, text=NULL, ES = getES()) {
  restore.point("load.story")

  #obj = read.yaml(file = paste0(dir,"/",file))

  tt = load.struct(name="story",file = paste0(dir,"/",file),typeName = "story",text=text,types = ES$types)

  obj = tt.object(tt,1)
  frames = setdiff(names(obj),c("storyId","settings"))

  es = as.environment(c(
    list(storyId = as.character(obj$storyId)),
    obj$settings,
    list(frames = obj[frames])
  ))

  #es = as.environment(tt.object(tt,1))
  #es = as.environment(read.yaml(file=paste0(dir,"/",file)))

  es$yaml = attr(tt,"yaml")
  #Encoding(es$yaml) <- "UTF-8"

  check.story(file = file, es=es)

  es
}
