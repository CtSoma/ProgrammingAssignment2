## Program: cachematrix.R
## purpose: Peer Program Assignment for week 3 R Programming
## Date   : Aug 23rd, 2014
## Description:
## The makeCacheMatrix takes in a Square matrix as input
## input of the form f <-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## The number of numeric inputs has to be square value like 4, 9, 16, 25, 36, 49, 64
##  and equally distributed between columns and rows.
## Also the determinant of the eventual matrix arranged Column wise should not equate to Zero.
## Matrices with determinant of Zero do not have an inverse.
## CacheSolve calculates the inverse of the the given matrix and keeps in memory.
## If the matrix remains same, the inverse will be retrieved from memory, else it will be calculated.
##
## make Cache Matrix is similar to make vector function, it has four main functions
## set, get, SetInverse, getInverse,
## these can be called as f$set(matrix(c(3,4,1,2),2,2)), f$get(),
## f$setInverse(matrix(c(0,1,1,0),2,2)), f$getInverse()

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL  ## initial Null for the Inverse Matrix,
                           ##this is effective when the main function is called.
        set <- function(y) { ## set -> this function sets the value of input matrix
                        x <<- y  ## superassigment of the Input Matrix to X
                invMatrix <<- NULL
                ## initiating the global variable to Null
                ## this null is effective when the Set matrix is called.
        }
        ## get -> this function gets the value of input matrix
        get <- function() { x }

        setInverse <- function(solveMat){
                ## setInverse --> this functions helps to set the value
                ## of Inverse matrix externally, does not validate
                invMatrix <<- solveMat  ## super Assignment of invMatrix vlaue
                }
        ## getInverse --> this function gets the solved inverse matrix,
        ## if the matrix is unsolvable it will return a null
        getInverse <- function() {invMatrix}

        ## This list the function calls available under the main function.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve has two functionalities
## 1. Solve for the inverse of the matrix if
##    the given matrix is a square matrix and a solvable matrix
## 2. if the input Matrix is unchanged retrive the result from cache
## this function can be called as cacheSolve(f) where f is a square matrix
## created by the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                     ## checks if the current invMatrix is not null
                message("getting cached data")        ##prints the message if its getting from Cache
                return(invMatrix)                     ## returns the value and terminates the program
        }
        data <- x$get()                               ## gets the input matrix and assigns to data
        dimenSion <- dim(data)                        ## gets the dimensions of input matrix

       if(dimenSion[1] == dimenSion[2]) {             ## this check is done to check if the given
                                                      ## matrix is a square matrix
                if(det(data) != 0 ) {                 ## this check is done to check if the
                                                      ## determinant is not 0
                        invMatrix <- solve(data, ...) ## calculate inverse
                        x$setInverse(invMatrix)
                        return(invMatrix)             ## displays the solved matrix and terminates function
                                    }
                        else                          ## when determinant is Zero
        {message("determinant of given Matrix is 0, this matrix will not have Inverse") }
       }
        else {                                        ## when the dimension of the input matrix do not match
                message("Given Matrix is not a Square Matrix")
             }
        data ## display the given matrix in case of Errors
}
