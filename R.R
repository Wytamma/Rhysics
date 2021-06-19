library(plotrix)
source('~/programming/Rhysics/Rhysics.R')

setClass("Circle", slots=list(position="vector", r="numeric"))

setGeneric("display", function(self) standardGeneric("display"))

setMethod("display", "Circle", function(self) {
  draw.circle(self@position[1], self@position[2], self@r, col = '#3498db')
})

setGeneric("update", function(self) standardGeneric("update"))

setMethod("update", "Circle", function(self) {
  stepSize = 0.2
  self@position <- self@position + c(runif(1, -stepSize, stepSize), runif(1, -stepSize, stepSize))
  return(self)
})

setup <- function() {
  par(bg='white')
  plot(-50:0, type="n")
  title('Rhyisics')
}


l <- c()
line_count = 0
char_count = 0
for (line in strsplit(R, '\n')[[1]]) {
  line_count = line_count - 1
  for (char in strsplit(line, '')[[1]]) {
    char_count = char_count + 1

    if (char == "1") {

      l <- c(l, new("Circle", position=c(char_count, line_count), r=0.7))
    }
  }
  char_count = 0

}
l <- unlist(l)
draw <- function() {
  for (i in 1:length(l)) {
    # update i.e. add vec to pos
    l[[i]] <<- update(l[[i]])
    # dysplay i.e. draw
    display(l[[i]])
  }
}

loop <- function(n) {
  plots <- c()
  for (i in 0:n) {
    flush.console()
    setup()
    draw()
    plots <- c(plots, recordPlot())
    Sys.sleep(0.1)
  }
  return(plots)
}

plots <- loop(30)
