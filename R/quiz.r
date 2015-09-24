examples.quiz = function() {
    yaml = '
frames:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = parse.quiz.yaml(yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)
  
  runEventsApp(app, launch.browser=rstudio::viewer)
  
  
}

parse.quiz.yaml = function(yaml,quiz.id =paste0("quiz_",sample.int(10e10,1))) {
  restore.point("parse.quiz.yaml")
  library(YamlObjects)
  qu = read.yaml(text=yaml)
  init.quiz(qu, quiz.id)
}

init.quiz = function(qu, quiz.id=paste0("quiz_",sample.int(10e10,1))) {
  if (is.null(qu[["id"]])) {
    qu$id = quiz.id
  }
  if (is.null(qu$frames)) {
    qu$frames = list(qu)
  }
  
  qu$frames = lapply(seq_along(qu$frames), function(ind) init.quiz.frame(qu$frames[[ind]],ind,qu))
  
  qu    
  
}

init.quiz.frame = function(frame, frame.ind=1, qu=NULL) {
  restore.point("init.quiz.frame")
  
  if (!is.null(frame$choices)) {
    correct.choices = which(str.ends.with(frame$choices,"*"))
    if (is.null(frame$multiple)) {
      frame$multiple = length(correct.choices) != 1
    }
    frame$correct.choices = correct.choices
    frame$choices[correct.choices] = str.remove.ends(frame$choices[correct.choices],right=1)
    frame$answer = unlist(frame$choices[correct.choices])
    names(frame$choices) =NULL
    if (frame$multiple) {
      frame$type = "mc"
    } else {
      frame$type = "sc"
    }
  } else if (!is.null(frame$answer)) {
    if (is.numeric(frame$answer)) {
      frame$type = "numeric"
      if (is.null(frame$roundto)) frame$roundto=0
    } else {
      frame$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", frame$question, " has neither defined the field 'answer' nor the field 'choices'."))
  } 
  
  expl = frame[["expl"]]
  if (!is.null(expl))
    expl = markdownToHTML(text=expl,encoding = "UTF-8", fragment.only=TRUE)
  
  if (is.null(frame$success)) {
    frame$success = paste0("<p><b>Correct.</b>",expl,"</p>")
  } else {
    frame$success =  markdownToHTML(text=frame$success,encoding = "UTF-8", fragment.only=TRUE)
  }
  if (is.null(frame$failure)) {
    frame$failure = paste0("<p><b>Not correct.</b>",expl,"</p>")
  } else {
    frame$failure =  markdownToHTML(text=frame$failure,encoding = "UTF-8", fragment.only=TRUE)
  }
  
  
  frame$id = paste0(qu$id,"__frame", frame.ind) 
  frame$inputId = paste0(frame$id,"__answer")
  frame$checkBtnId = paste0(frame$id,"__checkBtn")
  frame$explId = paste0(frame$id,"__explainUI")
  frame$ui = quiz.frame.ui(frame)

  
  frame
}

quiz.ui = function(qu, in.well.panel=TRUE) {
  
  if (in.well.panel) {
    pli = lapply(qu$frames, function(frame) {
      wellPanel(frame$ui)
    })
  } else {
    pli = lapply(qu$frames, function(frame) {
      frame$ui
    })
  }
  pli
}

quiz.frame.ui = function(frame) {
  head = list(
    HTML(paste0("<hr>",frame$question))
  )
  if (frame$type=="numeric") {
    answer = numericInput(frame$inputId, label = "",value = NULL)  
  } else if (frame$type =="text") {
    answer = textInput(frame$inputId, label = "",value = "")  
  } else if (frame$type=="mc") {
    answer = checkboxGroupInput(frame$inputId, "",frame$choices)
  } else if (frame$type=="sc") {
    answer = radioButtons(frame$inputId, "",frame$choices, selected=NA)
  }
  
  button = bsButton(frame$checkBtnId,label = "check", size="small")
  setUI(frame$explId,NULL)
  list(head,answer,button, uiOutput(frame$explId))
  
}

add.quiz.handlers = function(qu, check.fun=NULL, set.ui=TRUE){
  restore.point("add.quiz.handlers")
  for (frame in qu$frames) {
    buttonHandler(frame$checkBtnId,fun = click.check.quiz, frame=frame, qu=qu, check.fun=check.fun, set.ui=set.ui)
  }
}

click.check.quiz = function(app=getApp(), frame, qu,check.fun=NULL, set.ui=TRUE, tol=1e-8, ...) {
  answer = getInputValue(frame$inputId)
  restore.point("click.check.quiz")
  
  if (frame$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-frame$answer)<=max(frame$roundto,tol))
  } else {
    correct = setequal(answer,frame$answer)
  }
  
  if (set.ui) {
    if (correct) {
      cat("Correct!")
      setUI(frame$explId,HTML(frame$success))
    } else {
      cat("Wrong")
      setUI(frame$explId,HTML(frame$failure))
    }
  }
  if (!is.null(check.fun)) {
    check.fun(qu=frame,answered=TRUE,correct=correct)
  }
}



init.statement = function(sta, qu.ind=1) {
  restore.point("init.statement")
  types = substring(names(sta),1,1)
  yn.inds = which(types=="y" | types=="n")
  yn.ind = sample.int(length(yn.inds),1)

  st = list(qu.ind=qu.ind,yn.ind=yn.ind, yn = sta[[yn.ind]], type=types[[yn.ind]],
    all.yn=sta[yn.inds],types=types,expl=sta$e)
  st
}

add.quiz.ui.id = function(qu, qu.ind = qu$qu.ind) {
  qu$explId = paste0("quiz_explain__", qu.ind)
  qu$checkBtnId = paste0("quiz_checkBtn__", qu.ind)
  qu$inputId = paste0("quiz_input__", qu.ind)
  qu
}

statement.ui = function(st, check.fun=NULL, choices=c("true","false")) {
  restore.point("statement.ui")
  st = add.quiz.ui.id(st)
  
  ch = list("true","false")
  names(ch) = choices
  
  input = radioButtons(st$inputId, label="", choices=ch, selected=FALSE, inline=TRUE)
  ui = list(
    HTML(paste0("<hr>",st$yn)),
    input,
    bsButton(st$checkBtnId,label = "check",size = "extra-small"),
    uiOutput(st$explId)
  )
  buttonHandler(st$checkBtnId, check.statement.handler,st=st,check.fun=check.fun)
  setUI(st$explId,NULL)
  ui
}

check.statement.handler = function(st, check.fun,...) {
  restore.point("check.statement.handler")
  
  value = getInputValue(st$inputId)
  if (!(isTRUE(value=="false") | isTRUE(value=="true") )) {
    check.fun(answered=FALSE, correct=NA,qu=st,...)
    return()
  }
  if ( (value=="true" & st$type=="y") |
       (value=="false" & st$type=="n") ) 
  {
    check.fun(answered=TRUE,correct=TRUE, qu=st,...)
  } else {
    check.fun(answered=TRUE,correct=FALSE, qu=st,...)
  }
}