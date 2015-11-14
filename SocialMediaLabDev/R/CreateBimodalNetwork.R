CreateBimodalNetwork <-
function(x,writeToFile,removeTermsOrHashtags)
 {
   if (missing(writeToFile)) {
     writeToFile <- FALSE # default = not write to file
   }
   if (!missing(removeTermsOrHashtags)) {
     removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) #coerce to vector... to be sure
   }

   if (missing(removeTermsOrHashtags)) {
     removeTermsOrHashtags <- "foobar"
   }
    UseMethod("CreateBimodalNetwork",x)
  }
