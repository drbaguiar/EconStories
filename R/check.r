check.story = function(file=NULL, es=NULL) {
  restore.point("check.story")

  if (is.null(es))
    es <- load.story(file = file)

  if (!is.null(file))
    assert(es$storyId == str.left.of(basename(file),"."))

  if (!is.null(es$scenario)) {
    assert(!is.null(es$scenario$T))
    # shocks must be frame of the scenario
    assert(is.null(es$shocks))
    # T must be frame of the scenario$T
    assert(is.null(es$T))
  }

}

