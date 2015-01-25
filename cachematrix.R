## My attempt at the second assignment. This is more based on the 
##theoretical aspect of the assignment, ie., what kind of cube 
## is invertible and which kind cannot.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())### x is an object, a special 
          ##cube that can be inverted. 
          #The invertible cube meets the following criteria: 
          #1. all rows with at least one element different from zero 
          #above any row of zeros, 2. leading coefficient of a row 
          #must always be located by necessity to the right of 
          #the leading coefficient of the row above it. All rows must have a 
          #leading coefficient different from zero.This will make 
          #sure that the cube can be changed into a 
          ##triangular shape and then later inverted back
          # to a matrix. Gaussian Elimination Triangular Solver method. 
          #See Jorge Soriano Pinedo's project "Matrix Inversion Speed Up 
          #With CUDA" IIT Chicago, 2011. {
                        mcm   <- NULL
                        set <- function (y)#Here is where I would normally put the code in R 
                         #to test for a valid cube. 
                {
                     x <<- y          ##This is where lexical scoping is happening
                     mcm <<- NULL
               }
      get     <- function() x
      setcache <- function(cache) mcm <<- cache
      getcache <- function() mcm
      list(set = set, get = get,
     setcache = setcache,
     getcache = getcache)
      }
              cachsolve <- function(x, ...) 
                {
              mcm <- x$getcache()
       if(is.mull(mcm))
         {
               message("getting cached data")
          return(mcm)
      }
     {
            mcm <- solve(mcm) %*% mcm # Note that solve uses per cent asterisk and per cent
       }data <- x$get()
           mcm<- cache(data, ...)
           set$cache(mcm)
           mcm
      }