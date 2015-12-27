
#############################################################################
# THIS FILE  ALLOWS YOU TO CALCULATE THE INVERSE OF A MATRIX IF IT HASN'T   #
# BEEN DONE YET. ELSE, IT WILL MAKE THE CALCULATION. HAPPY NEW YEAR! :)     #
#############################################################################


# Generally speaking, there are two functions within this file ('makeCacheMatrix
# and 'cacheSolve').

# The first one, called "makeCacheMatrix" has four functions in it that help you
# store and retrieve the value of the original matrix and the inverse matrix 
# and it need one argument, 'firstMatrix' which is the original matrix to be
# solved. Each of its functions is described in the next lines. 

# The second function, called "cacheSolve", is the one that does the heavy job. 
# It asks "makeCacheMatrix" if the calculation has already been done and, if the
# process hasn't been made, it calculates the inverse of the matrix

makeCacheMatrix <- function(firstMatrix = matrix()) { # beginning of 'makeCacheMatrix' function

#   'SolvedM' allows us to store information related to the inverse of a matrix. If the 
#   calculation hasn't been made, then its value is 'NULL'. Else, it has the inverse
#   value of the matrix.
    
    solvedM <- NULL
    
#   'setValues' function: when we first run the whole program, we have to assign the 
#   makeCacheMatrix function to an R object through the console (for example, 
#   "object1 <- makeCacheMatrix()"). Then, in console, with "object1$setValues(matrixName)" 
#   we pass the matrix we want to solve. In this case, "matrixName" is an R object 
#   previously created. After doing this, the function will assign parameter
#   "firstMatrix" the value of the matrix you are passing through "matrixName". 
#   The function will assing a "Null" value to "SolvedM" because it's the
#   first time we are using the function, therefore there is no inverse matrix 
#   calculation yet.
    
    setValues <- function(x){
        
        solvedM <<- NULL
        firstMatrix <<- x
        
    }
    
#   'storeMatrix': this is a function called by the 'cacheSolve' function. After 
#   'cacheSolve' calculates the inverse of the matrix for the first time, 
#   it calls 'storeMatrix' in order to save the result into 'solvedM' (recall
#   that 'solvedM' is an object of the 'makeCacheMatrix' function). Therefore, for future 
#   requests to calculate the matrix inverse, 'solvedM' is going to have the
#   value and, thus, there is going to be no need to calculate the inverse again.
    
    storeMatrix <- function(x){
        
        solvedM <<- x
        
    }

#   'obtainCalculation': this function just returns the 'solvedM' object.
#   'obtainCalculation' is called by the 'cacheSolve' function ('par1$obtain')
#   to see if the matrix inverse has already been calculated. 
    
    obtainCalculation <- function() solvedM

#   'originalMatrix': returns the original matrix (the one used to calculate 
#   its inverse)
    
    originalMatrix <- function() firstMatrix
    
    list(Store = storeMatrix, Obtain = obtainCalculation, Original = originalMatrix, Set = setValues)
    
} # end of 'makeCacheMatrix' function


# The next function, 'cacheSolve' asks "makeCacheMatrix" if the calculation has
# already been done and, if the process hasn't been made, it calculates the 
# inverse of the matrix. It needs one argument or parameter ('par1'), that should
# be the object we created by assigning 'makeCacheMatrix()' to it (in the first
# example it was "object1 <- makeCacheMatrix()". This was explained when the function
# 'setValues' of the 'makeCacheMatrix' function was explained.

cacheSolve <- function(par1, ...) {  # beginning of 'cacheSolve' function

#   With 'par1$Obtain()' we get the status of the 'solvedM' object, located in
#   the 'makeCacheMatrix() function. 

        checkStatus <- par1$Obtain()
    
#   If 'checkStatus" gets a not 'Null' value, then 'cacheSolve' will ask 
#   'makeCacheMatrix' for the inverse value of the matrix with 'solvedMatrix <- par1$Obtain()' 
#   (that is because if 'checkStatus' has a different value than 'Null' 
#   it means the calculation has already been done).
        
    if (!is.null(checkStatus)){
        
        print("The matrix inverse has already been calculated. Getting cached data from 'Obtain calculation' function.")
        solvedMatrix <- par1$Obtain()
        return(solvedMatrix)
        
    }
    
#   If 'solvedM' is 'NULL' (thus 'checkStatus' has a 'Null' value) then 
#   it 'cacheSolve' will do the computation for the first time.
    
#   With the next line, we retrieve the original matrix that should be
#   used to calculate its inverse
        
    original <- par1$Original()
    
#   'tryCatch' function lets us handle an error in case there is one. I put this 
#   here to make sure that if something is not working good with the assigned 
#   matrix, then the program doesn't suddenly stops running but instead warns 
#   the user that something is not right. One example of this is would be if the
#   user doesn't creates a matrix to calculate its inverse. Then, since R is
#   not going to have a matrix when it tries to do 'solve(original)' an error
#   will occur and the program will suddenly stop.
    
#   As a summary, the next piece of code calculates the inverse of a matrix and 
#   assigns it to 'original' object, with 'original <- solve(original)'. 
#   Then it stores the result in the 'makeCacheMatrix' function with 'par1$Store(original)' 
#   and finally returns 'original' to show it to the user. If an
#   error occurs the function will print a message asking the user to check the matrix that
#   is trying to solve.
    
    tryCatch ({
        
        print("The matrix inverse hasn't been calculated. Doing calculation now...");
        original <- solve(original); 
        par1$Store(original); 
        return(original) }, 
        error = function(e){
            
            "Please check the matrix you are trying to solve"
            
            }
    )
    
} # end of 'cacheSolve' function
