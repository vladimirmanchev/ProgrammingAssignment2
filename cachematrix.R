## Here we create two functions that only work together to calculate the inverse of a matrix. The
## first one - makeCacheMatrix takes as an argument a matrix and creates a special object that
## contains the matrix and a list of function that can be used to modify the object that was
## created. The second function takes as an argument the special object created by the first and
## finds the inverse matrix of the one contained in the special object and saves that inverse
## matrix in the cache. However it first checks whether the inverse matrix has not been calculated
## before.


## makeCacheMatrix creates a special object(anyNameofMatrix) that has as output a list containing
## functions.This special object also contains the matrix that was used as an argument to create 
## it.

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL ##initializes invx, serves to clear the cache when a new matrix is used
        set <- function(y) {  # set function allows for input of a new matrix without the need 
                              # to run the makeCacheMarix function. 
                
                x <<- y   # to input new matrix you call the set function by name using 
                          # anyNameofMatrix$set(newMatrix) where anyNameofMatrix contains 
                          # the initial output of makeCacheMarix and newMatrix is the new matrix   
                
                invx <<- NULL # since a new matrix has been inputted, 
                              # invx need to be reinitialized (the cache cleared)
        
        }
        get <- function() x  # Returns the current martix used by anyNameofMatrix. 
                             # Can also serve to print the current matrix the anyNameofMatrix 
                             # uses. Useful if a new matrix has been inputted using the set 
                             # function.
        
        setinverce <- function(inverce) invx <<- inverce # serves to cache the invx once 
                                                         # calculated.
        
        getinverce <- function() invx # Returns the value of invx (either NULL or the cached 
                                      # value if such is available).
        list(set = set, get = get,    # Creates a name list for the functions above so 
                                      # that they can be called using $ extract operator.
             setinverce = setinverce,
             getinverce = getinverce)
}

## cacheSolve takes the special object(anyNameofMatrix) that makeCacheMatrix has created and 
## finds the inverse of the matrix contained in that special object 

cacheSolve <- function(x, ...) {
        invx <- x$getinverce()  # Calls getinverce function from the anyNameofMatrix which 
                                # assigns the current value of invx(NULL by default since that 
                                # was the value) set when the special object was created
        if(!is.null(invx)) {  # Checks the value of invx in case there is a cached value for it
                              # if the value of invx is different than NULL the following code is
                              # executed.
                message("getting cached data")
                return(invx)
        }
        data <- x$get() # If the value of invx in NULL the function assigns the matrix contained 
                        # in the anyNameofMatrix.
        invx <- solve(data, ...) # The inverse matrix is calculated and assigned to invx.
        x$setinverce(invx) # The setinverce function is called. This assigns the new value of
                           # invx in the parent environment (the one defined by makeCacheMatrix).
                           # This is where the value of invx is cached
        invx #returns invx
}
