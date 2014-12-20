makeCacheMatrix <- function(x = matrix()) {  # x is a matrix

        m <- NULL           # it is reset to NULL
                            # everytime makeCacheMatrix() is called
                            
        set <- function(y) { 

                x <<- y      

                m <<- NULL   #m is resetted to NULL
        }
        get <- function() x  # original matrix is returned

        setsolve <- function(solve){ # called by cacheSolve() function

                m <<- solve  # sets the value of m to inverse 
        }                                         
        getsolve <- function() m # cached value is returned to cacheSolve

        list(set = set, get = get, # object of type list is returned 
             setsolve = setsolve,  
             getsolve = getsolve)
}


cacheSolve<- function(x, ...) { # x is been taken from 
					  # makeCacheMatrix()
        m <- x$getsolve()   

        if(!is.null(m)) {   # if inverse was calculated
                            # for a particular matrix

                message("getting cached data") 
                
                return(m)    # inverse is returned              
        }
        data <- x$get()      # if m is NULL 

        m <- solve(data, ...) 

        x$setsolve(m)  # calculated value is stored in object x

        m        # inverse is returned 
}
