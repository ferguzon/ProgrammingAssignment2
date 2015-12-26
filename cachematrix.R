# THIS FILE  ALLOWS YOU TO CALCULATE THE INVERSE OF A MATRIX IF IT HASN'T
# BEEN DONE YET. ELSE, IT WILL MAKE THE CALCULATION.



# Generally speaking, there are two functions within this file ('cacheMatrix
# and 'matrixSolve').
# The first one, called "cacheMatrix" has four functions in it that help you
# store and retrieve the value of the original matrix and the inverse matrix. 
# Each of the functions is described in the next lines. The second function, 
# called "matrixSolve", is the one that does the heavy job. 
# It asks "cacheMatrix" if the calculation has already been done and, if the
# process hasn't been made, it calculates the inverse of the matrix

cacheMatrix <- function(firstMatrix = matrix()) {

#   SolvedM allows us to store information related to the inverse of a matrix. If the 
#   calculation hasn't been made, then its value is 'NULL'. Else, it has the inverse
#   value of the matrix.
    
    solvedM <- NULL
    
#   setValues function: when we first run the whole program, we have to assign the 
#   cacheMatrix function to an R object through the console (for example, 
#   "object1 <- cacheMatrix()"). Then, in console, with "object1$setValues(matrixName)" 
#   we pass the matrix we want to solve. In this case, "matrixName" is a R object 
#   previously created through the console. After doing this, the function will assign 
#   "firstMatrix" the value of the matrix you are passing through "matrixName". 
#   The function will assing a "Null" value to "SolvedM" because it's the
#   first time we are using the function, therefore, there is no inverse matrix 
#   calculation yet.
    
    setValues <- function(x){
        
        solvedM <<- NULL
        firstMatrix <<- x
        
    }
    
#   storeMatrix: this is a function called by the 'matrixSolve' function. After 
#   'matrixSolve' calculates the inverse of the matrix for the first time, 
#   it calls 'storeMatrix' in order to save the result into 'solvedM' (recall
#   that 'solvedM' is an object of the 'cacheMatrix' function). Therefore, for future 
#   requests to calculate the matrix inverse, 'solvedM' is going to have the
#   value and, thus, there is going to be no need to calculate the inverse again.
    
    storeMatrix <- function(x){
        
        solvedM <<- x
        
    }

#   obtainCalculation: this function just returns the 'solvedM' object.
#   'obtainCalculation' is called by the 'matrixSolve' function ('par1$obtain')
#   to see if the matrix inverse has already been calculated. 
    
    obtainCalculation <- function() solvedM

#   originalMatrix: returns the original matrix (the one used to calculate 
#   its inverse)
    
    originalMatrix <- function() firstMatrix
    
    list(Store = storeMatrix, Obtain = obtainCalculation, Original = originalMatrix, Set = setValues)
    
}

# 'matrixSolve' asks "cacheMatrix" if the calculation has already been done and,
# if the process hasn't been made, it calculates the inverse of the matrix

matrixSolve <- function(par1, ...) {

#If 'solvedM' is NULL
#   then it 'matrixSolve' will do the computation for the first time. If it's not
#   Null, then 'matrixSolve' will ask 
    
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
