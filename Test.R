get_CountOfElementsWithCondition <- function(list_of_elements, condition=NA) {
    
    assert_that(is_vector(list_of_elements))
    
    if (is_function(condition)) {
        # old_w <- getOption("warn")
        # options(warn=-1)
        count <- sum(suppressWarnings(condition(list_of_elements)), na.rm=TRUE)
        # count <- sum(suppressWarnings({condition(list_of_elements)}), na.rm=TRUE)
        # count <- sum(suppressMessages({condition(list_of_elements)}), na.rm=TRUE)
        # count <- sum(suppressMessages({suppressWarnings({condition(list_of_elements)})}), na.rm=TRUE)
        # options(warn=old_w)
    } else {
        count <- length(list_of_elements)
    }
    return(count)
}
get_CountOfElementsWithCondition(names(network_model), function(x){IsWhole(as.numeric(x))})
