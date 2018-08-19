## Both of these function actually tries to cache the inverse of a MATRIX

## In this first function it has some nested functions that manipulating matrix
## to get it's inverse in cache function.

#Function declared to hold a matrix data
makeCacheMatrix <- function(x = matrix()) 
{
        s <- NULL #inittalising the matrix_inverse unit to NULL
        set <- function(y)
        {
                x<<-y #Able to change the values of matrix through it's environment 
                s<<-NULL
        }
        get <- function() x # Gets the matrix
        setinv <- function(solve) s <<- solve # Calculates the inverse of non-singular matrix via the solve function
        getinv <- function() s # Gets the inverse of matrix
        # passing values of function makeCacheMatrix        
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}


## This function takes the input matrix after function makeCacheMatrix
## It's checking whether it havig a inverse_matrix to display it, and if
## it doesn't contain any matrix it tries to get it through GET function 
## and solves it inverse to display it.

cacheSolve <- function(x, ...) 
{
        s <- x$getinv() # Getting inverse matrix
        if(!is.null(s)) # A condition to check, does 1st function has thrown any matrix or not
        {
                message("getting cached data")
                return(s) # If 's' is not null and it does contain an inverse matrix, then it will simply retun it
        }
        mat_data <- x$get() # If 's' is null it get a matrix through GET function
        s <- solve(mat_data, ...) # solve to get it inverse of that function
        x$setinv(s) # And will set this to 'x' as it's inverse 
        s
        ## Returning a matrix that is 's', the inverse of 'x'
}

