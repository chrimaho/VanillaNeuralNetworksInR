str_Format <- function(string, ...) {
    #' @title String Formatter
    #' @description Take an input string, and substitute in-string variables.
    #' @note This is similar to the Python `string.foramt()` method.
    #' @param string string. The string to be re-formatted. Note, each of the named arguments must be surrounded in curly brackets.
    #' @param ... variables. A list of variables. Note, these can either be named or not; but they must all be named, or all be blank, because it cannot handle a mixture. Each of these arguments must align to the variables in curly brackets from the `string` argument. These will be combined in to a list.
    #' @return A formatted string
    #' @example str_Format("Sammy the {animal} {verb} a {noun}.", animal="shark", verb="made", noun="house")
    #' @example str_Format("Sammy the {} {} a {}.", "shark", "made", "house")
    #' @example "Sammy the {animal} {verb} a {noun}" %>% str_Format(animal="shark", verb="made", noun="house")
    #' @references https://stackoverflow.com/questions/44763056/is-there-an-r-equivalent-of-pythons-string-format-function#answer-44763659
    #' @author chrimaho
    #' @export
    
    # Import packages ----
    require(stringr)
    require(magrittr)
    require(dplyr)
    require(assertthat)
    require(dynutils)
    require(english)
    
    # Validations ----
    assert_that(is.string(string))
    assert_that(c("stringr", "magrittr", "dplyr", "assertthat", "dynutils", "english") %all_in% .packages(), msg="The packages must be mounted.")
    
    # Set Up ----
    num_variables <- str_count(string, coll("{}"))
    vars <- list(...)
    
    # Handle if vars are not named ----
    if (num_variables>0) {
        
        # Add number in between each curly bracket
        for (i in 1:num_variables) {
            string %<>% str_replace(coll("{}"), paste0("{",as.english(i),"}"))
        }
        
        # Name the vars as numbers
        vars %<>% set_names(as.english(1:num_variables))
        
    }
    
    # Make environment ----
    envir <- as.environment(vars)
    parent.env(envir) <- .GlobalEnv
    
    # Perform substitution
    string %<>% str_replace_all("\\{", "${")
    str_return <- str_interp(string=string, env=envir)
    
    # Return ----
    return(str_return)
    
}


get_CountOfElementsWithCondition <- function(list_of_elements, condition=NULL) {
    #' @title Get Count of Elements With Condition
    #' @description Get the count of the number of elements in a list that meet a specified condition.
    #' @note The `condition` must be a hidden function. Also note that the `warnings` are suppressed when running the `condition` function.
    #' @param list_of_elements vector. The list of elements to check.
    #' @param condition function. The condition for checking. Must be a hidden function.
    #' @return An integer.
    #' @example get_CountOfElementsWithCondition(c("i", "1", "2", "3", "o"), function(x){IsWhole(as.numeric(x))}) ## returns 3
    #' @example get_CountOfElementsWithCondition(c("i", "1", "2", "3", "o"))                                      ## returns 5
    #' @example get_CountOfElementsWithCondition(c("i", "1", "2", "3", "o"), 2)                                   ## throws error
    #' @references https://thispointer.com/python-count-elements-in-a-list-that-satisfy-certain-conditions/#crayon-5ea195c434f39109077492-1
    #' @author chrimaho
    #' @export
    
    # Import packages ----
    require(assertthat)
    
    # Validations ----
    assert_that(is_vector(list_of_elements))
    assert_that(or(is_function(condition), is.null(condition)), msg="'condition' must either be a function or the value 'NULL'.")
    
    # Do work ----
    if (!is.null(condition)) {
        count <- sum(suppressWarnings(condition(list_of_elements)), na.rm=TRUE)
    } else {
        count <- length(list_of_elements)
    }
    
    # Return ----
    return(count)
}

get_ObjectAttributes <- function(object, name) {
    #' @title Get Attributes
    #' @description Extract and print the key attributes of an object, including `name`, `size`, `class`, `type`, `mode`, `dims`.
    #' @note In order to do pretty-print, parse the result of this function in to the `cat()` function.
    #' @param object any. The object to be checked.
    #' @param name string. The name of the object.
    #' @return A string that contains all the relevant information.
    #' @example 
    #' @author chrimaho
    #' @export
    
    # Import packages
    require(assertthat)
    require(magrittr)
    
    # Get attributes
    if (missing(name)) {name <- deparse(substitute(object))}
    name %<>% paste("Name :", .)
    size  <- object %>% object.size() %>% format(units="auto") %>% paste("Size :", .)
    class <- object %>% class()       %>% paste("Clas :", .)
    type  <- object %>% typeof()      %>% paste("Type :", .)
    mode  <- object %>% mode()        %>% paste("Mode :", .)
    dims  <- object %>% 
        { if(class(.) %in% c("matrix","data.frame","list","array")) {
            dim(.) %>% paste(collapse="x")
        } else {
            length(.)
        } 
        } %>%  
        paste("Dims :", .)
    
    # Print attributes
    output <- paste(
        name,
        size,
        class,
        type,
        mode,
        dims,
        sep="\n - "
    )
    
    # Return
    return(output)
}

# Print result
get_ObjectAttributes(cifa$images) %>% cat()


?str_Format
