CreateDynamicNetwork <-
function(x,writeToFile)
 {
   if (missing(writeToFile)) {
     writeToFile <- FALSE # default = not write to file
   }
     UseMethod("CreateDynamicNetwork",x)
  }
