# This functions allows you to calculate the inverse of a matrix.
# If the calculation has already been done, then the result is
# of the second function is going to be the first operation, 
# that was stored in the cache.

## Write a short comment describing this function

cacheMatrix <- function(firstMatrix = matrix()) {

    solvedM <- NULL
    
    setValues <- function(x){
        
        solvedM <<- NULL
        firstMatrix <<- x
        
    }
    
    storeMatrix <- function(x){
        
        solvedM <<- x
        
    }
    
    obtainCalculation <- function() solvedM
    
    originalMatrix <- function() firstMatrix
    
    list(Store = storeMatrix, Obtain = obtainCalculation, Original = originalMatrix, Set = setValues)
    
}


## Write a short comment describing this function

matrixSolve <- function(par1, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    checkStatus <- par1$Obtain()
    
    if (!is.null(checkStatus)){
        
        print("The matrix inverse has already been calculated. Getting cached data from 'Obtain calculation' function.")
        solvedMatrix <- par1$Obtain()
        return(solvedMatrix)
        
    }
    
    original <- par1$Original()
    tryCatch ({
        
        print("The matrix inverse hasn't been calculated. Doing calculation now...");
        original <- solve(original); 
        par1$Store(original); 
        return(original) }, 
        error = function(e){
            
            "Please check the matrix you are trying to solve"
            
            }
    )
    
}
