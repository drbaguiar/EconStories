click.story.check.quiz = function(..., es) {
  restore.point("click.story.check.quiz")

  qu = es$cur$frame$quiz

  click.check.quiz(qu=qu,quiz.handler=story.quiz.handler, part.ind=0)
}

story.quiz.handler = function(solved, app=getApp(), es=app$es,...) {
  restore.point("story.quiz.handler")
  if (solved)
    story.tell.answer(es=es)
  cat("\nQuiz solved = ", solved)
}
