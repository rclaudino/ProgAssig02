# function          <-  makeCacheMatrix() 
# goal              <-  set a matrix and cache the solved result
# author            <-  rclaudino, 2014-12-20
# email             <-  rclaudino@fmh.ulisboa.pt
# faculty-en        <-  c("human kinetics faculty", "lisbon university")
# faculty-pt        <-  c("faculdade de motricidade humana", "universidade de lisboa")
# acronym           <-  c("FMH", "UL")
# city              <-  c("lisbon", "lisboa")
# country           <-  c("Portugal", "pt-PT")
# whit the help of: <-  c("Pavel Kirjanas","& other", "Forums", "Programming Assignment 2 (peer assessment): Lexical Scoping") 
# &                 <-  c("Making sense of Assignment 2")
# class makeCacheMatrix properties and methods specification:
#   variables (more accurately, class/object properties):
#       mtx         <- an input matrix property
#       sm          <- property to store the solved matrix
#   functions (more accurately, class/object methods):   
#       set         <- method to assign and save a new matrix_ 
#                      It also resets the solved matrix to NULL, when a new object is generated 
#       get         <- method to return the original matrix values
#       setSolve    <- method accessed by cachSolve() function_
#                      to calculate and store/cache the first matrix solved values by superassignement 
#       getSolve    <- method accessed by cacheSolve() function_
#                      to return the cached matrix solved values on the following accessess 
#       list        <- list of the internal methods accessed whenever class makeCacheMatrix provides a new object
#
makeCacheMatrix <- function(mtx = matrix()) 
    {
        sm <- NULL
        
        set <- function(y) 
            {
                mtx <<- y
                sm <<- NULL
            }

        get <- function() 
            {
                mtx
            }
        
        setSolve <- function(solve) 
            {
                sm <<- solve
            }
        
        getSolve <- function() 
            {
                sm
            }

        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
    }

# function              <- cacheSolve()
# goal                  <- return a matrix that is the inverse of 'x'
# function cacheSolve properties and methods specification:
#   variables (more accurately, class/object properties):
#       mtx             <- represents an object created by makeCacheMatrix
#       sm              <- property to store the solved matrix 
#       data            <- represents the matrix data values
#   functions (more accurately, class/object methods):   
#       mtx$getSolve()  <- accesses the getSolve method from the object 'x'
#       mtx$get()       <- accesses the get method from the object 'mtx' if mtx$getSolve returned NULL
#       mtx$setSolve()  <- store the calculated solved matrix values in the object 'mtx'
#
cacheSolve <- function(mtx) 
    {
        sm <- mtx$getSolve()
        
        if(!is.null(sm)) 
            {
                message("Cached inverse matrix")
                return(sm)
            }
        
        data <- mtx$get()
        
        sm <- solve(data)
        
        mtx$setSolve(sm)
        
        sm
    }
