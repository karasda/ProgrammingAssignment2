# This piece of program contains functions which create possibility
# to speed up your computing by caching time demanding inversion of given matrix.

# This function constructs an "object" containing two private variables x, inv.
# The former is matrix and the latter is its cached inverse (or NULL,
# if the inverse has not been computed yet). Furthermore, it contains public 
# getter and setter for those variables.
# Return value is list of those getters and setters.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL # variable for storing inverse/ or NULL
      
      # setter for stored matrix (set new matrix and erase stored inverse)
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
            
      # getter for stored matrix
      get <- function()
            return(x)
      
      # setter for inverse, operator <-- set variable in the parent environment
      set_inv <- function(NewInv)
            inv <<- NewInv
      
      # getter for stored inverse
      get_inv <- function()
            return(inv)
      
      return(list( set = set, get = get,
                   set_inv = set_inv, get_inv = get_inv))
            
}


# This function checks, whether an inverse of matrix x is cached.
# If it is true, then it returns cached inverse,
# otherwise it compute inverse returns it and store it.
cacheSolve <- function(x, ...) {
      
      inv <- x$get_inv() # gets information about cached inverse
      
      # is inverse cached?
      if(!is.null(inv)){
            print("Getting cached inverse matrix.")
            return(inv)
      }else {
            inv <- solve(x$get(), ...) # compute inverse
            x$set_inv(inv) # store inverse
            return(inv) # return inverse
      }
}

