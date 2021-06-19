

createCanvas <- function(width, height, bg = 'white', border =  TRUE) {
  width <<- width
  height <<- height
  par(bg = bg, mar = c(1, 1, 1, 1))
  plot(
    0:width,
    ylim = c(0, height),
    xlim = c(0, width),
    type = "n",
    ylab = "",
    yaxt = 'n',
    xaxt = 'n',
    xlab = "",
    asp = 1,
    frame.plot = FALSE
  )
  if (border) {
    rect(0, 0, width, height)
  }

}

ellipse <- function(xpos, ypos, a, b, col = '#3498db') {
  plotrix::draw.ellipse(xpos,
                        ypos,
                        a = a,
                        b = b,
                        col = col)
}


loop <- function(number_of_frames) {
  frames <- c()
  setup()
  for (i in 0:number_of_frames) {
    flush.console()
    draw()
    # frames <- c(frames, recordPlot())
    Sys.sleep(0.1)
  }
}


setClass("Vector",
         slots = list(x = 'numeric',
                      y = 'numeric'))

setMethod("Ops", signature(e1="Vector", e2="Vector"),
          function(e1, e2) {
            e1@x=callGeneric(e1@x, e2@x)
            e1@y=callGeneric(e1@y, e2@y)
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="Vector", e2="numeric"),
          function(e1, e2) {
            e1@x=callGeneric(e1@x, e2)
            e1@y=callGeneric(e1@y, e2)
            validObject(e1)
            return(e1)
          }
)
setMethod("Ops", signature(e1="numeric", e2="Vector"),
          function(e1, e2) {
            e2@x=callGeneric(e1, e2@x)
            e2@y=callGeneric(e1, e2@y)
            validObject(e2)
            return(e2)
          }
)



R <- "0011111111111111110000000
0011111111111111111100000
0011111111111111111100000
0011111111111111111110000
0011111100000011111110000
0011111100000011111110000
0011111100000001111110000
0011111100000011111110000
0011111111111111111100000
0011111111111111111100000
0011111111111111111000000
0011111111111111000000000
0011111111111111100000000
0011111100011111111000000
0111111100001111111000000
0111111100001111111000000
0111111100000111111100000
0011111100000011111110000
0011111100000011111110000
0011111100000001111111000
0011111100000001111111000
0011111100000000111111100"
