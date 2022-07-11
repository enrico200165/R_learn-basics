

l1 <- list("a","b","c")
l2 <- list(1,2,3)
l3 <- list("one","two","three")

showParams <- function(...){
  args <- list(...)
  print(args)
  for(a in args) { 
    str(a) 
  }
  print("------------ ESCO ----------")
  TRUE
}
