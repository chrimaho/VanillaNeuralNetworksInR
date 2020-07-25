#==============================================================================#
#                                                                              #
#    Title      : Functions for Creating a Vanilla Neural Network              #
#    Purpose    : These functions will be imported from within the notebook    #
#                 by calling the `source()` function.                          #
#    Notes      : They are different because these functions contain more      #
#                 commentary and comments and validations than what is         #
#                 contained in the notebook.                                   #
#    Author     : chrimaho                                                     #
#    Created    : 25/Jul/2020                                                  #
#    References : All references are contained within the specific functions.  #
#    Sources    : Sources                                                      #
#    Edited     : 25/Jul/2020 - Initial creation                               #
#                                                                              #
#==============================================================================#


#------------------------------------------------------------------------------#
#                                                                              #
#    Generic Functions                                                      ####
#                                                                              #
#------------------------------------------------------------------------------#

str_Format <- function(string, ...) {
    #' @title String Formatter
    #' @description Take an input string, and substitute in-string variables.
    #' @note This is similar to the Python `string.foramt()` method.
    #' @param string `string`. The string to be re-formatted. Note, each of the named arguments must be surrounded in curly brackets.
    #' @param ... `variables`. A list of variables. Note, these can either be named or not; but they must all be named, or all be blank, because it cannot handle a mixture. Each of these arguments must align to the variables in curly brackets from the `string` argument. These will be combined in to a list.
    #' @return A formatted string
    #' @seealso https://stackoverflow.com/questions/44763056/is-there-an-r-equivalent-of-pythons-string-format-function#answer-44763659
    #' @author chrimaho
    #' @examples 
    #' str_Format(
    #'     "Sammy the {animal} {verb} a {noun}.", 
    #'     animal="shark", 
    #'     verb="made", 
    #'     noun="house"
    #' )
    #' 
    #' str_Format(
    #'     "Sammy the {} {} a {}.", 
    #'     "shark", 
    #'     "made", 
    #'     "house"
    #' )
    #' 
    #' "Sammy the {animal} {verb} a {noun}" %>% 
    #'     str_Format(animal="shark", verb="made", noun="house")
    #' 
    #' "Sammy the {} {} a {}" %>%
    #'     str_Format("shark", "made", "house")
    
    # Packages
    require(stringr)
    require(magrittr)
    require(dplyr)
    require(assertthat)
    require(dynutils)
    require(english)
    
    # Validations
    assert_that(string %>% is.string, msg="'string' must be type 'string'.")
    
    # Set Up
    num_variables <- str_count(string, coll("{}"))
    vars <- list(...)
    
    # Handle if vars are not named
    if (num_variables>0) {
        
        # Add number in between each curly bracket
        for (i in 1:num_variables) {
            string %<>% str_replace(coll("{}"), paste0("{",as.english(i),"}"))
        }
        
        # Name the vars as numbers
        vars %<>% set_names(as.english(1:num_variables))
        
    }
    
    # Make environment
    envir <- as.environment(vars)
    parent.env(envir) <- .GlobalEnv
    
    # Perform substitution
    string %<>% str_replace_all("\\{", "${")
    str_return <- str_interp(string=string, env=envir)
    
    # Return
    return(str_return)
    
}


get_Modulus <- function(value) {
    #' @title Find minimum modulus value
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param value `numeric`. The value to be modularised.
    #' @return The minimum modulus number of `value`.
    #' @seealso 
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_MaximumMudulus(
    #'     value=NA,
    #'     NA=NA
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    require(magrittr)
    
    # Validations
    assert_that(value %>% is.numeric, msg="'value' must be type 'numeric'.")
    
    # Do work
    for (num in 1:value) {
        if (num == 1) {
            mod_value <- num
        } else if (value %% num == 0) {
            mod_value %<>% c(num)
        }
    }
    
    # Return
    return(mod_value)
}


get_CountOfElementsWithCondition <- function(vector_of_elements, condition=NULL) {
    #' @title Get Count of Elements with Condition
    #' @description Get the count of the number of elements in a list that meet a specified condition.
    #' @note The `condition` must be an anonymous function. Also note that the `warnings` are suppressed when running the `condition` function.
    #' @param vector_of_elements `vector`. The list of elements to check.
    #' @param condition `function`. The condition for checking. Must be an anonymous function. Defaults to `NULL`.
    #' @return An integer.
    #' @seealso https://thispointer.com/python-count-elements-in-a-list-that-satisfy-certain-conditions/#crayon-5ea195c434f39109077492-1
    #' @author chrimaho
    #' @examples
    #' # Returns condition
    #' get_CountOfElementsWithCondition(
    #'     c("i", "1", "2", "3", "o"),
    #'     function(x) {IsWhole(as.numeric(x))}
    #' )
    #' 
    #' # Returns length
    #' get_CountOfElementsWithCondition(
    #'     c("i", "1", "2", "3", "o")
    #' )
    #' 
    #' # Returns error
    #' get_CountOfElementsWithCondition(
    #'     c("i", "1", "2", "3", "o"),
    #'     2
    #' )
    
    # Import packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(vector_of_elements %>% is_vector, msg="'vector_of_elements' must be type 'vector'.")
    assert_that(or(condition %>% is_function, condition %>% is.null), msg="'condition' must either be a function or the value 'NULL'.")
    
    # Do work
    if (!is.null(condition)) {
        count <- sum(suppressWarnings(condition(vector_of_elements)), na.rm=TRUE)
    } else {
        count <- length(vector_of_elements)
    }
    
    # Return
    return(count)
}


is.integer <- function(value) {
    #' @title Check if is integer.
    #' @description This is a wrapper around the `DescTools::IsWhole()` function.
    #' @note The `DescTools::IsWhole()` is the only package that genuinely works on integer values. Other functions such as `base::is.integer()` and `purrr::is_integer()` do not work properly...
    #' @param value `numeric`. The value to be checked.
    #' @return A logical value confirming if `value` is an integer or not.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' is.integer(
    #'     value=1
    #' )
    #' 
    #' # Fails
    #' is.integer(
    #'     value=0.5
    #' )
    
    # Packages
    require(dplyr)
    require(DescTools)
    
    # Validations
    #assert_that(value %>% is.numeric, msg="'value' must be type 'numeric'.")
    
    # Do work
    return(IsWhole(value))
}


get_TimeDifference <- function(start_time, finish_time=Sys.time()) {
    
    assert_that(start_time %>% is.time, msg="'start_time' must be type 'time'.")
    assert_that(finish_time %>% is.time, msg="'finish_time' must be type 'time'.")
    
    diff_time <- difftime(finish_time, start_time, units="auto")
    
    str_diff_time <- paste(
        diff_time %>% round(2),
        diff_time %>% attributes() %>% extract2("units")
    )
    
    return(str_diff_time)
    
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Check Data Functions                                                   ####
#                                                                              #
#------------------------------------------------------------------------------#

get_ObjectAttributes <- function(object, name=deparse(substitute(object)), print_freq=FALSE) {
    #' @title Get Object Attributes
    #' @description Extract and print the key attributes of an object, including `name`, `size`, `class`, `type`, `mode`, `dims`.
    #' @note In order to do pretty-print, parse the result of this function in to the `cat()` function.
    #' @param object `any`. The object to be checked.
    #' @param name `string`. The name of the object. Defaults to the name of `object` in the parent environment.
    #' @return A string that contains all the relevant information.
    #' @author chrimaho
    #' @examples 
    #' vec <- vector(1:20)
    #' mat <- matrix(1:20, ncol=4)
    #' 
    #' get_ObjectAttributes(
    #'     object=vec
    #' )
    #' 
    #' get_ObjectAttributes(
    #'     object=mat,
    #'     name="My Matrix"
    #' )
    
    # Import packages
    require(assertthat)
    require(magrittr)
    require(dplyr)
    
    # Validations
    assert_that(name %>% is.string, msg="'name' must be type 'string'.")
    assert_that(print_freq %>% is.flag, msg="'print_freq' must be type 'flag'.")
    
    # Get attributes
    if (missing(name)) {name <- deparse(substitute(object))}
    name %<>% paste("Name :", .)
    size  <- object %>% object.size() %>% format(units="auto") %>% paste("Size :", .)
    class <- object %>% class()       %>% paste(collapse=",") %>%  paste("Clas :", .)
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
    if (print_freq) {
        assert_that(object %>% dim %>% length <= 2, msg="If 'print_freq' is TRUE, then the dimensions of 'object' must be less than or equal to '2'.")
        freq <- object %>% 
            table(dnn="label") %>% 
            as.data.frame() %>% 
            as.matrix(rownames.force=TRUE) %>% 
            noquote %>% 
            capture.output %>% 
            paste0(collapse="\n   ") %>% 
            paste("Freq :\n  ", .)
    } else {
        freq <- NULL
    }
    
    # Combine attributes
    output <- paste(
        name,
        size,
        class,
        type,
        mode,
        dims,
        sep="\n - "
    )
    
    # Add Freq
    if (print_freq) {
        output %<>% paste(freq, sep="\n - ")
    }
    
    # Return
    return(output %>% paste0("\n"))
}


set_MakeImage <- function(image, index=1) {
    #' @title Convert Image from Array to RGB Image
    #' @description Slice a given 4-D array at a given index, and convert that to an RGB image.
    #' @note `image` must be a 3-D array, the same as returned from the function `tensorflow::tf$keras$datasets$cifar10$load_data()[[2]][[1]][1,,,]`.
    #' @param image `array`. A 3-D array of image, where the dimensions are: `width`x`height`x`colours`.
    #' @param index `integer`. The index of the image to be returned. Default value `1`.
    #' @return An `rgb()` matrix, the result of running the `grDevices::rgb()` function.
    #' @seealso https://stackoverflow.com/questions/32113942/importing-cifar-10-data-set-to-r#answer-39672323
    #' @author chrimaho
    #' @examples
    #' set_MakeImage(
    #'     image=tensorflow::tf$keras$datasets$cifar10$load_data()[[2]][[1]][1,,,],
    #'     index=1
    #' )
    
    # Packages
    require(dplyr)
    require(grDevices)
    require(assertthat)
    
    # Validations
    assert_that(image %>% is.array, msg="'image' must be type 'array'.")
    assert_that(image %>% dim %>% length == 3, msg="'image' must have 4 dimensions.")
    assert_that(index %>% is.integer, msg="'index' must be type 'integer'.")
    
    # Extract elements
    image.r <- image[,,1]
    image.g <- image[,,2]
    image.b <- image[,,3]
    
    # Make rgb
    image.rgb <- rgb(image.r, image.g, image.b, maxColorValue=255)
    
    # Fix dimensions
    dim(image.rgb) <- dim(image.r)
    
    # Return
    return(image.rgb)
}


plt_PlotImage <- function(images, classes, index=1) {
    #' @title Plot Image from a given 4-D block of images
    #' @description Plot a specific image from a given 4-D block of images.
    #' @note `images` must be a 4-D array, the same as returned from the function `tensorflow::tf$keras$datasets$cifar10$load_data()[[2]][[1]]`.
    #' @param images `array`. A 4-D array of image, where the dimensions are: `images`x`width`x`height`x`colours`.
    #' @param classes `array`. A 2-D array of integers, where the dimensions are: `images`x`class`. The first dimension must be the same length as `dim(images)[1]`, but the second dimension can only be length=`1`. In other words, it's a single-column matrix.
    #' @param index `integer`. The index of the image to be plotted. Default value `1`.
    #' @return Nothing is returned.
    #' @seealso https://stackoverflow.com/questions/12918367/how-to-plot-with-a-png-as-background#answer-12918368
    #' @author chrimaho
    #' @examples
    #' data <- tensorflow::tf$keras$datasets$cifar10$load_data()
    #' plt_PlotImage(
    #'     images=data[[2]][[1]],
    #'     classes=data[[2]][[2]],
    #'     index=1
    #' )
    
    # Libraries
    require(dplyr)
    require(grDevices)
    require(assertthat)
    require(grid)
    
    # Validations
    assert_that(images %>% is.array)
    assert_that(images %>% dim %>% length == 4, msg="'images' must have 4 dimensions.")
    assert_that(index %>% is.integer)
    
    # Slice images
    image <- images[index,,,]
    image %<>% set_MakeImage(index)
    lbl <- classes[index] %>% as.character() %>% ClassList[[.]]
    
    # Plot image
    plot.new()
    lim <- par()
    rasterImage(image, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4], interpolate=FALSE)
    title(lbl, font.main=2)
    
    # Return
    return(invisible(NULL))
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Instantiation and Initialisation Functions                             ####
#                                                                              #
#------------------------------------------------------------------------------#

set_InstantiateNetwork <- function(input=50, hidden=c(30,20,10), output=1) {
    #' @title Instantiate the Network
    #' @description Instantiate the network.
    #' @note This is just to set up an empty network. Which is effectively a list of lists.
    #' @param input `integer`. The number of nodes in the `input` layer of the network. Default value `50`.
    #' @param hidden `vector`. The number of nodes in each `hidden` layer of the network. Default value `c(30,20,10)`.
    #' @param output `integer`. The number of nodes in the `output` layer of the network. Default value `1`.
    #' @return An instantiated network. A `list` of `lists`.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_InstantiateNetwork(
    #'     input=50,
    #'     hidden=c(30,20,10),
    #'     output=1
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(input %>% is.integer, msg="'input' must be type 'integer'.")
    assert_that(hidden %>% is.vector, msg="'hidden' must be type 'vector'.")
    assert_that(hidden %>% is.integer %>% all, msg="All elements of 'hidden' must be integers.")
    assert_that((hidden > 0) %>% all, msg="All elements of 'hidden' must be greater than '0'.")
    assert_that(output %>% is.integer, msg="'input' must be type 'integer'.")
    
    # Set up
    model = list()
    names = c(
        "input",
        1:length(hidden),
        "output"
    )
    
    # Loop
    for (layer in names) {
        
        # Make layer
        model[[layer]] <- list(
            "nodz"      = "",  #<-- Number of nodes in this layer.
            "inpt"      = "",  #<-- Input matrix. Aka 'A_prev'. This is a duplicate of the activation of the previous layer, so for large networks this needs to be taken in to consideration.
            "wgts"      = "",  #<-- Weights matrix. Aka 'W'.
            "bias"      = "",  #<-- Bias vector. Aka 'b'.
            "linr"      = "",  #<-- Linear matrix. Aka 'Z'. This is the result of the linear algebra between inpt, wgts and bias.
            "acti"      = "",  #<-- Activation matrix. Aka 'A'. The result of applying an activation function to the linr matrix.
            "acti_func" = "",  #<-- The activation function used.
            "cost"      = "",  #<-- The overall cost of the model. This is a single value (the overall cost of the model), but is copied to each layer of the model.
            "back_cost" = "",  #<-- Gradient of the cost vector. Aka 'dA_cost'.
            "back_acti" = "",  #<-- Gradient of the Activation matrix. Aka 'dA'. The result of differentiation after having applied back propagation. with a given cost function.
            "back_linr" = "",  #<-- Gradient of the Linear algebra matrix. Aka 'dZ'. The result of backwards linear differentiation back propagation.
            "back_wgts" = "",  #<-- Gradient of the Weights matrix. Aka 'dW'. Also the result of back-prop.
            "back_bias" = ""   #<-- Gradient of the Bias vector. Aka 'db'. Also the result of back-prop.
        )
        
        # Set nodes
        if (layer=="input") {
            model[[layer]][["nodz"]] = input
        } else if (layer=="output") {
            model[[layer]][["nodz"]] = output
        } else {
            layer_index = layer %>% as.numeric()
            model[[layer]][["nodz"]] = hidden[layer_index]
        }
        
    }
    
    # Return
    return(model)
}


let_InitialiseXavier <- function(nodes_in, nodes_out, order=6) {
    #' @title Xavier Initialisation
    #' @description Initialise the weights based on the Xavier algorithm.
    #' @param nodes_in `integer`. The number of nodes coming in to this layer (ie. the number of nodes in the previous layer).
    #' @param nodes_out `integer`. The number of nodes outgoing from this layer (ie. the number of nodes in this layer).
    #' @param order `integer`. The order of magnitude for this equation. Default value `6`.
    #' @return A floating point number.
    #' @seealso https://towardsdatascience.com/weight-initialization-in-neural-networks-a-journey-from-the-basics-to-kaiming-954fb9b47c79
    #' @seealso http://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_InitialiseXavier(
    #'     nodes_in=30,
    #'     nodes_out=20,
    #'     order=6
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(nodes_in %>% is.integer, msg="'nodes_in' must be type 'integer'.")
    assert_that(nodes_out %>% is.integer, msg="'nodes_out' must be type 'integer'.")
    assert_that(order %>% is.integer, msg="'nodes_out' must be type 'integer'.")
    
    # Do work
    numer <- sqrt(order)
    denom <- sqrt(nodes_in + nodes_out)
    output <- numer/denom
    
    # Return
    return(output)
}


let_InitialiseHe <- function(nodes_in, nodes_out, order=2) {
    #' @title He Initialisation
    #' @description Initialise the weights based on the He initialisation algorithm.
    #' @param nodes_in `integer`. The number of nodes coming in to this layer (ie. number of nodes in previous layer).
    #' @param nodes_out `integer`. The number of nodes outgoing from this layer (ie. the number of nodes in this layer). !NOTE! This value is not used in this function, but is included to make the arguments of this function consistent with those of the other initialisation functions.
    #' @param order `integer`. The order of magnitude for this equation. Default value `2`.
    #' @return A floating point number.
    #' @seealso https://towardsdatascience.com/weight-initialization-in-neural-networks-a-journey-from-the-basics-to-kaiming-954fb9b47c79
    #' @seealso http://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_InitialiseHe(
    #'     nodes_in=30,
    #'     order=2
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(nodes_in %>% is.integer, msg="'nodes_in' must be type 'integer'.")
    assert_that(order %>% is.integer, msg="'order' must be type 'integer'.")
    
    # Do work
    numer <- order
    denom <- nodes_in
    output <- sqrt(numer/denom)
    
    # Return
    return(output)
}


set_InitialiseLayer <- function(network_model, layer_index, initialisation_algorithm=NA, initialisation_order=6) {
    #' @title Initialise layer within network.
    #' @description Set the weights and biases of a given layer.
    #' @note If `initialisation_algorithm` is `NA`, then the weights initialisation is only a random normal number, the result of running `rnorm()` as follows: `matrix(data=rnorm(nodes_in * nodes_out), nrow=nodes_in, ncol=nodes_out)`
    #' @param network_model `list`. The model to be updated. The result of having run the `set_InstantiateNetwork()` function.
    #' @param layer_index `integer`. The index of the layer to be initialised.
    #' @param initialisation_algorithm `string`. The algorithm to be used for initialisation (eg. `xavier` or `he`). Default value `NA`.
    #' @param initialisation_order `integer`. The order of magnitude for the initialisation (can be either an integer, or set to the number of layers defined). Default value `6`.
    #' @return The `network_model`, with the relevant layer having been initialised.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_InitialiseLayer(
    #'     network_model=network_model,
    #'     layer_index=1,
    #'     initialisation_algorithm="xavier",
    #'     initialisation_order="6"
    #' )
    
    # Packages
    require(assertthat)
    require(stringr)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(layer_index %>% is.integer, msg="'layer_index' must be type 'integer'.")
    assert_that(initialisation_algorithm %in% c("xavier", "he", NA), msg="'initialisation_algorithm' must be one of 'xavier', 'he', or 'NA'.")
    assert_that(initialisation_order %>% is.integer, msg="'layer_index' must be type 'integer'.")
    
    # Get layer names
    layer_prev <- names(network_model)[layer_index-1]
    layer <- names(network_model)[layer_index]
    
    # Get number of nodes
    if (layer_index == 1) {
        nodes_in <- 0 #The first layer is the 'input' layer; therefore, there are 0 nodes feeding in to it.
    } else {
        nodes_in <- network_model %>% extract2(layer_prev) %>% extract2("nodz")
    }
    nodes_out <- network_model %>% extract2(layer) %>% extract2("nodz")
    
    # Set the seed of reproducibility
    set.seed(1234)
    
    # Initialise weight matrix
    w_matrix <- matrix(
        data=rnorm(nodes_in * nodes_out), 
        nrow=nodes_in,
        ncol=nodes_out
    )
    
    # Get initialisation algorithm
    if (!is.na(initialisation_algorithm)) {
        algorithm <- paste0("let_Initialise", str_to_title(initialisation_algorithm))
    }
    
    # Scale weights
    if (layer_index != 1) {
        if (is.na(initialisation_algorithm)) {
            w_matrix <- w_matrix
        } else {
            w_matrix <- w_matrix * get(algorithm)(nodes_in=nodes_in, nodes_out=nodes_out, order=initialisation_order)
        }
    }
    
    # Initialise bias matrix
    b_matrix <- matrix(
        data=network_model %>% extract2(layer) %>% extract2("nodz") %>% replicate(0),
        nrow=network_model %>% extract2(layer) %>% extract2("nodz"),
        ncol=1
    )
    
    # Place data back in to the model
    network_model[[layer]][["wgts"]] <- w_matrix
    network_model[[layer]][["bias"]] <- b_matrix
    
    # Return
    return(network_model)
}


set_InitialiseModel <- function(network_model, initialisation_algorithm="xavier", initialisation_order="layers") {
    #' @title Initialise Model
    #' @description Initialise the model with all the necessary weights and biases.
    #' @note Add a note for the developer.
    #' @param network_model `list`. The model to be initialised.
    #' @param initialisation_algorithm `string`. The initialisation algorithm to use for the weights. If `NA`, then will use the values from an `rnorm()` function. Default value `"xavier"`.
    #' @param initialisation_order `integer` or `string`. The order of magnitude for the initialisation algorithm. Should be a number (ideally less than `10`), or the word `layers`, which will automatically calculate the order based in the number of hidden layers in the model. Default value `"layers"`.
    #' @return The `network_model`, with all layers having been initialised.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_InitialiseModel(
    #'     network_model,
    #'     initialisation_algorithm="xavier",
    #'     initialisation_order="layers"
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(or(is.string(initialisation_algorithm), is.na(initialisation_algorithm)), msg="'initialisation_algorithm' must be type 'string' or value 'NA'.")
    assert_that(initialisation_algorithm %in% c("xavier","he",NA), msg="'initialisation_algorithm' must be one of 'xavier', 'he', or 'NA'.")
    assert_that(or(is.integer(initialisation_order), is.string(initialisation_order)), msg="'initialisation_order' must be type 'integer' or 'string'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Redefine 'initialisation_order'
    if (initialisation_order == "layers") {
        initialisation_order <- get_CountOfElementsWithCondition(names(network_model), function(x){IsWhole(as.numeric(x))})
    }
    
    # Initialise each layer
    for (layer_index in 1:length(names(network_model))) {
        network_model <- set_InitialiseLayer(
            network_model=network_model, 
            layer_index=layer_index, 
            initialisation_algorithm=initialisation_algorithm,
            initialisation_order=initialisation_order
        )
    }
    
    # Return
    return(network_model)
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Forward Propagation Functions                                          ####
#                                                                              #
#------------------------------------------------------------------------------#

set_LinearForward <- function(inpt, wgts, bias) {
    #' @title Linear Forward Algebra
    #' @description Perform linear algebra on a layer during forward propagation.
    #' @note Just like finding a gradient (`y=mx+b`), the output of this function takes `inpt * wgts + bias`.
    #' @note To perform the matrix addition part, the `base::sweep()` function is used, which sweeps a vector linearly across a matrix. In this case we are using the second dimension for this, meaning to say that the bias is swept across each row of the `linr` matrix. And as each row is each node, then this is how the bias is being added.
    #' @param inpt `matrix`. The input matrix for this layer. This is also the 'activation' from the previous layer. First dimension: Number of nodes in the previous layer. Second dimension: number of images being parsed in to the model.
    #' @param wgts `matrix`. The weights matrix for this layer. The length of the first dimension of `wgts` MUST equal the length of the second dimension of the `inpt` matrix. First dimension: number of images being parsed in to the model. Second dimension: number of nodes in this layer. 
    #' @param bias `vector` or single-column `matrix`. The bias for this layer. The length of this element MUST be equal to the second dimension of the `wgts` matrix. In other words, the length of the `bias` element is the number of nodes in this layer.
    #' @return A `matrix` after performing linear algebra.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_LinearForward(
    #'     inpt,
    #'     wgts,
    #'     bias
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(inpt %>% is.matrix, msg="'inpt' must be type 'matrix'.")
    assert_that(wgts %>% is.matrix, msg="'wgts' must be type 'matrix'.")
    assert_that(bias %>% is.matrix, msg="'bias' must be type 'matrix'.")
    assert_that(wgts %>% dim %>% extract(1) == inpt %>% dim %>% extract(2), msg="The length of the first dimension of 'wgts' MUST equal the length of the second dimension of 'inpt'.")
    assert_that(bias %>% length == wgts %>% dim %>% extract(2), msg="The length of `bias` MUST equal the length of the second dimension of `wgts`.")
    
    # Perform matrix multiplication
    linr <- inpt %*% wgts
    
    # Add bias
    linr <- sweep(linr, 2, bias, "+")
    
    # Return
    return(linr)
}


let_ActivateSigmoid <- function(linr) {
    #' @title Sigmoid Activation
    #' @description Activate a matrix using the Sigmoid algorithm.
    #' @note The `linr` is the result of running the `set_LinearForward()` function.
    #' @param linr `matrix`. The matrix to be activated.
    #' @return An activated matrix.
    #' @seealso https://kite.com/python/answers/how-to-calculate-a-logistic-sigmoid-function-in-python
    #' @seealso https://www.geeksforgeeks.org/implement-sigmoid-function-using-numpy/
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_ActivateSigmoid(
    #'     linr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(linr %>% is.matrix, msg="'linr' must be type 'matrix'.")
    
    # Do work
    acti <- 1/(1+exp(-linr))
    
    # Return
    return(acti)
}


let_ActivateRelu <- function(linr) {
    #' @title ReLU Activation
    #' @description Activate a matrix using the Relu algorithm.
    #' @note The `linr` is the result of running the `set_LinearForward()` function.
    #' @param linr `matrix`. The matrix to be activated.
    #' @return An activated matrix.
    #' @seealso https://medium.com/ai%C2%B3-theory-practice-business/a-beginners-guide-to-numpy-with-sigmoid-relu-and-softmax-activation-functions-25b840a9a272
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_ActivateRelu(
    #'     linr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(linr %>% is.matrix, msg="'linr' must be type 'matrix'.")
    
    # Do work
    acti <- sapply(linr, max, 0) %>% 
        structure(dim=dim(linr))
    
    # Return
    return(acti)
}


let_ActivateSoftmax <- function(linr) {
    #' @title Softmax Activation
    #' @description Activate a matrix using the Softmax algorithm.
    #' @note The `linr` is the result of running the `set_LinearForward()` function.
    #' @param linr `matrix`. The matrix to be activated.
    #' @return An activated matrix.
    #' @seealso https://medium.com/ai%C2%B3-theory-practice-business/a-beginners-guide-to-numpy-with-sigmoid-relu-and-softmax-activation-functions-25b840a9a272
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_ActivateSoftmax(
    #'     linr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(linr %>% is.matrix, msg="'linr' must be type 'matrix'.")
    
    # Do work
    expo <- exp(z)
    expo_sum <- sum(exp(z))
    acti <- expo/expo_sum
    
    # Return
    return(acti)
}


let_ActivateSwish <- function(linr, beta=0.1) {
    #' @title Swish Activation
    #' @description Activate a matrix using the Swish algorithm.
    #' @note The `linr` is the result of running the `set_LinearForward()` function.
    #' @param linr `matrix`. The matrix to be activated.
    #' @param beta `numeric`. The beta amount to be used.
    #' @return An activated matrix.
    #' @seealso https://arxiv.org/pdf/1710.05941.pdf
    #' @seealso https://www.bignerdranch.com/blog/implementing-swish-activation-function-in-keras/
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_ActivateSwish(
    #'     linr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(linr %>% is.matrix, msg="'linr' must be type 'matrix'.")
    
    # Do work
    acti <- linr * (beta * linr)
    
    # Return
    return(acti)
}


set_ForwardProp <- function(network_model, data_in, activation_hidden="relu", activation_final="sigmoid") {
    #' @title Run Forward Propagation
    #' @description Run forward propagation over a model (`network_model`) with a given input data set (`data_in`) and using different activations for the hidden and output layers (`activation_hidden` & `activation_final`).
    #' @note Add a note for the developer.
    #' @param network_model `list`. The model to be used. Note, must be instantiated and initialised, after having run the `set_InitialiseModel()` function.
    #' @param data_in `array`. A 4-D array containing the images for propagation. Note, the dimensions must be: `images`x`width`x`height`x`colour`.
    #' @param activation_hidden `string`. The activation algorithm to use for the hidden layers. Must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'. Default value `relu`.
    #' @param activation_final `string`. The activation algorithm to use for the final layer. Must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'. Default value `sigmoid`.
    #' @return The same `network_model`, after having completed forward propagation.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_ForwardProp(
    #'     network_model,
    #'     data_in,
    #'     activation_hidden,
    #'     activation_final
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(data_in %>% is.array, msg="'data_in' must be type 'array'.")
    assert_that(is.string(activation_hidden), msg="'activation_hidden' must be type 'string'.")
    assert_that(is.string(activation_final), msg="'activation_final' must be type 'string'.")
    assert_that(activation_hidden %in% c("sigmoid","relu","softmax","swish"), msg="'activation_hidden' must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'.")
    assert_that(activation_final %in% c("sigmoid","relu","softmax","swish"), msg="'activation_final' must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Do work
    for (index in 1:length(names(network_model))) {
        
        # Define layer name
        layr <- names(network_model)[index]
        
        if (layr=="input") {
            
            # Pass-thru for 'input' layer
            network_model[[layr]][["inpt"]] <- data_in
            network_model[[layr]][["acti"]] <- data_in
            
        } else {
            
            # Extract data
            prev <- names(network_model)[index-1]
            inpt <- network_model[[prev]][["acti"]]
            wgts <- network_model[[layr]][["wgts"]]
            bias <- network_model[[layr]][["bias"]]
            
            # Calculate
            linr <- set_LinearForward(inpt, wgts, bias)
            
            # Activate
            if (layr=="output") {
                acti <- get(paste0("let_Activate",str_to_title(activation_final)))(linr)
                network_model[[layr]][["acti_func"]] <- activation_final
            } else {
                acti <- get(paste0("let_Activate",str_to_title(activation_hidden)))(linr)
                network_model[[layr]][["acti_func"]] <- activation_hidden
            }
            
            # Apply back to our model
            network_model[[layr]][["inpt"]] <- inpt
            network_model[[layr]][["linr"]] <- linr
            network_model[[layr]][["acti"]] <- acti
            
        }
        
    }
    
    # Return
    return(network_model)
}


get_ComputeCost <- function(pred, true, epsi=1e-10) {
    #' @title Compute the Cost
    #' @description Compute the cost of a given network.
    #' @note Uses a tiny epsilon value in order to account for perfect predictions.
    #' @param pred `matrix`. The matrix of values to use for the prediction.
    #' @param true `matrix`. The matrix of values to use for the truth.
    #' @param epsi `number`. A very small epsilon value, in order to adjust for perfect predictions.
    #' @return A single floating point number.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_ComputeCost(
    #'     pred,
    #'     true,
    #'     epsi
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(pred %>% {is.matrix(.) | is.vector(.)}, msg="'pred' must be type 'matrix' or type 'vector'.")
    assert_that(true %>% {is.matrix(.) | is.vector(.)}, msg="'true' must be type 'matrix' or type 'vector'.")
    assert_that(length(pred) == length(true), msg="The length of 'pred' must equal the length of 'true'.")
    assert_that(epsi %>% is.number, msg="'epsi' must be type 'number'.")
    assert_that(epsi < 0.0001, msg="'epsi' should be a very small epsilon value.")
    
    # Get number of samples
    samp <- length(true)
    
    # Instantiate totals
    total_cost <- 0
    
    # Loop for each prediction
    for (i in 1:samp) {
        
        # Adjust for perfect predictions.
        if (pred[i]==1) {pred[i] %<>% subtract(epsi)}
        if (pred[i]==0) {pred[i] %<>% add(epsi)}
        
        # Calculate totals
        total_cost <- total_cost - ((true[i] * log(pred[i]) + (1-true[i]) * log(1-pred[i])))
        
    }
    
    # Take an average
    cost <- (1/samp) * total_cost
    
    # Return
    return(cost)
}


set_ApplyCost <- function(network_model, cost) {
    #' @title Apply Cost to model
    #' @description Apply the calculated cost to each layer of a given `network_model`.
    #' @note It simply applies the same value to the `cost` section of each layer.
    #' @param network_model `list`. The network model that should have the cost applied to it. It must be an instantiated and initialised model, which has already had the `set_ForwardProp` function applied to it. Default value `NA`.
    #' @param cost `number`. The cost that has been calculated for the model. It is the result of running the `get_ComputeCost()` function. Default value `NA`.
    #' @return The `network_model`, with the cost value applied to each layer.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_ApplyCost(
    #'     network_model=NA,
    #'     cost=NA
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(cost %>% is.number, msg="'cost' must be type 'number'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Apply back to the model
    for (layer in network_model %>% names) {
        network_model[[layer]][["cost"]] <- cost
    }
    
    # Return
    return(network_model)
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Backward Propagation Functions                                         ####
#                                                                              #
#------------------------------------------------------------------------------#

get_DifferentiateCost <- function(pred=NA, true=NA) {
    #' @title Differentiate Cost Value
    #' @description Differentiate the Cost value.
    #' @note Simple differentiation function.
    #' @param pred `matrix`. The matrix of predicted values.
    #' @param true `matrix`. The matrix of true values.
    #' @return A floating point vlue.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_DifferentiateCost(
    #'     pred,
    #'     true
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(pred %>% {is.matrix(.) | is.vector(.)}, msg="'pred' must be type 'matrix' or type 'vector'.")
    assert_that(true %>% {is.matrix(.) | is.vector(.)}, msg="'true' must be type 'matrix' or type 'vector'.")
    
    # Do work
    diff_cost <- -(divide_by(true, pred) - divide_by(1-true, 1-pred))
    
    # Return
    return(diff_cost)
}


set_ApplyDifferentiateCost <- function(network_model, cost_differential) {
    #' @title Apply Cost Differential to the Network
    #' @description Apply the calculated Cost Differential function to the given network.
    #' @note Applies the same value to each layer of the network. The `cost_differential` is transposed for the `output` layer by running the `t()` function, which is necessary for the back-propagation parts.
    #' @param network_model `list`. The model that the cost differential should be applied to. Must be a model that has been instantiated and initialised, and has had the `set_ForwardProp()` function run over it.
    #' @param cost_differential `number`. The differentiated cost for the given model. The result of running the `get_DifferentiateCost()` function.
    #' @return The `network_model`, after having the `cost_differential` applied to each layer..
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_ApplyDifferentiateCost(
    #'     network_model,
    #'     cost_differential
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(cost_differential %>% is.numeric, msg="'cost_differential' must be type 'numeric'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Do work
    for (layer in names(network_model)) {
        network_model[[layer]][["back_cost"]] <- cost_differential
        if (layer=="output") {
            network_model[[layer]][["back_acti"]] <- network_model[[layer]][["back_cost"]] %>% t()
        }
    }
    
    # Return
    return(network_model)
}


get_DifferentiateLinear <- function(back_linr_curr, acti_prev, wgts, bias) {
    #' @title Differentiate the linear algebra part
    #' @description For a given layer, differentiate the linear algebra parts.
    #' @note Do the weights of current layer first, then bias of the current layer, then activation of the the previous layer.
    #' @param back_linr_curr `matrix`. The differentiated linear matrix of the next layer.
    #' @param acti_prev `matrix`. The activate matrix from the previous layer.
    #' @param wgts `matrix`. The weights matrix of the current layer.
    #' @param bias `matrix`. The bias matrix of the current layer.
    #' @return A list of three matrices. 1) `diff_acti_prev`: The differentiated activation matrix of the previous layer; 2) `diff_wgts`: The differentiated weights matrix of the current layer; 3) `diff_bias`: The differentiated bias matrix of the current layer.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_DifferentiateLinear(
    #'     back_linr_curr,
    #'     acti_prev,
    #'     wgts,
    #'     bias
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(back_linr_curr %>% is.matrix, msg="'back_linr_curr' must be type 'matrix'.")
    assert_that(acti_prev %>% is.matrix, msg="'acti_prev' must be type 'matrix'.")
    assert_that(wgts %>% is.matrix, msg="'wgts' must be type 'matrix'.")
    assert_that(bias %>% is.matrix, msg="'bias' must be type 'matrix'.")
    
    # get number of samples
    samp <- acti_prev %>% dim %>% extract(2)
    
    # Check
    # print(dim(back_linr_curr))
    # print(dim(acti_prev))
    # print(dim(wgts))
    
    # Differentiate weights
    diff_wgts <- 1/samp * (back_linr_curr %*% acti_prev)
    
    # Differentiate bias
    diff_bias <- 1/samp * rowSums(back_linr_curr, dims=1)
    
    # Differentiate activation
    diff_acti_prev <- wgts %*% back_linr_curr
    
    # Consolidate in to one list
    list_linr <- list(
        diff_acti_prev, 
        diff_wgts, 
        diff_bias
    )
    
    # Return
    return(list_linr)
}


let_BackwardActivateRelu <- function(diff_acti_curr, linr_curr) {
    #' @title Get Backwards ReLU Activation
    #' @description Get the differentiated ReLU activation .
    #' @note Simply reversing the work of the `let_ActivateRelu()` function.
    #' @param diff_acti_curr `matrix`. The differentiated activation matrix of the current layer.
    #' @param linr_curr `matrix`. The linear algebra matrix in the current layer.
    #' @return The differentiated linear algebra matrix of the current layer.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_BackwardActivateRelu(
    #'     diff_acti_curr,
    #'     linr_curr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(diff_acti_curr %>% is.matrix, msg="'diff_acti_curr' must be type 'matrix'.")
    assert_that(linr_curr %>% is.matrix, msg="'linr_curr' must be type 'matrix'.")
    
    # Do work
    diff_linr_curr <- diff_acti_curr
    diff_linr_curr[linr_curr<=0] <- 0
    
    # Return
    return(diff_linr_curr)
}


let_BackwardActivateSigmoid <- function(diff_acti_curr, linr_curr) {
    #' @title Get Backwards Sigmoid Activation
    #' @description Get the differentiated Sigmoid activation .
    #' @note Simply reversing the work of the `let_ActivateSigmoid()` function. Need to transpose a couple of times in order to ensure that the matrices are all aligned correctly.
    #' @param diff_acti_curr `matrix`. The differentiated activation matrix of the current layer.
    #' @param linr_curr `matrix`. The linear algebra matrix in the current layer.
    #' @return The differentiated linear algebra matrix of the current layer.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_BackwardActivateRelu(
    #'     diff_acti_curr,
    #'     linr_curr
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(diff_acti_curr %>% is.matrix, msg="'diff_acti_curr' must be type 'matrix'.")
    assert_that(linr_curr %>% is.matrix, msg="'linr_curr' must be type 'matrix'.")
    
    # Do work
    temp <- 1/(1+exp(-linr_curr))
    diff_linr_curr <- t(diff_acti_curr) * temp * (1-temp)
    
    # Return
    return(t(diff_linr_curr))
}


set_BackwardProp <- function(network_model) {
    #' @title Run Back Propagation
    #' @description Apply Back Propagation over a given model.
    #' @note Skips the `input` layer, because that doesn't need to be back-propagated. Also, it runs through each layer in reverse; in the same way that the `set_ForwardProp()` function works from start to end, the `set_BackwardProp()` function works from end to start.
    #' @param network_model `list`. The model to run back-propagation over.
    #' @return The same `network_model`, but having had back-propagation applied to it.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_BackwardProp(
    #'     network_model
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Loop through each layer in reverse order
    for (layr_indx in network_model %>% names %>% length %>% 1:. %>% rev) {
        
        # Get the layer name
        layr_curr <- network_model %>% names %>% extract(layr_indx)
        
        # Skip the 'input' layer
        if (layr_curr == "input") next
        
        # Get the previous layer name
        layr_prev <- network_model %>% names %>% extract(layr_indx-1)
        
        # Set up the variables
        linr_curr <- network_model[[layr_curr]][["linr"]]
        wgts_curr <- network_model[[layr_curr]][["wgts"]]
        bias_curr <- network_model[[layr_curr]][["bias"]]
        acti_prev <- network_model[[layr_prev]][["acti"]]
        acti_func_back <- network_model[[layr_curr]][["acti_func"]] %>% str_to_title %>% paste0("let_BackwardActivate", .)
        diff_acti_curr <- network_model[[layr_curr]][["back_acti"]]
        diff_linr_curr <- matrix()
        diff_acti_prev <- matrix()
        diff_wgts_curr <- matrix()
        diff_bias_curr <- matrix()
        
        # Differentiate activation
        diff_linr_curr <- get(acti_func_back)(diff_acti_curr, linr_curr)
        
        # Differentiate linear
        list_linr <- get_DifferentiateLinear(
            back_linr_curr=diff_linr_curr,
            acti_prev=acti_prev,
            wgts=wgts_curr,
            bias=bias_curr
        )
        diff_acti_prev <- list_linr[[1]]
        diff_wgts_curr <- list_linr[[2]]
        diff_bias_curr <- list_linr[[3]]
        
        # Apply back to model
        network_model[[layr_prev]][["back_acti"]] <- diff_acti_prev
        network_model[[layr_curr]][["back_linr"]] <- diff_linr_curr
        network_model[[layr_curr]][["back_wgts"]] <- diff_wgts_curr
        network_model[[layr_curr]][["back_bias"]] <- diff_bias_curr
        
    }
    
    return(network_model)
}


set_UpdateModel <- function(network_model, learning_rate=0.001) {
    #' @title Update the Model
    #' @description Update the `network_model` by taking a number of 'steps', which is effectively updating the weights and biases by multiplying the matrices by a given learning rate (`learning_rate`).
    #' @note The `grad_step_weights` element is transposed to ensure it is in the correct orientation.
    #' @param network_model `list`. The network model to be updated.
    #' @param learning_rate `number`. The learning rate that the parameters should be updated by. It should be a small decimal number. Default value `0.001`.
    #' @return The same `network_model`, after having the parameters (`wgts` & `bias`) updated.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' set_UpdateModel(
    #'     network_model,
    #'     learning_rate=0.001
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(learning_rate %>% is.number, msg="'learning_rate' must be type 'number'.")
    assert_that(between(learning_rate, 0, 1), msg="'learning_rate' must be between '0' and '1'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Do work
    for (index in network_model %>% names %>% length %>% 1:.) {
        
        # Get layer name
        layr <- network_model %>% names %>% extract(index)
        
        # Skip 'input' layer
        if (layr=="input") next
        
        # Define gradient steps
        grad_step_wgts <- -1 * (learning_rate * network_model[[layr]][["back_wgts"]])
        grad_step_bias <- -1 * (learning_rate * network_model[[layr]][["back_bias"]])
        
        # Take steps
        network_model[[layr]][["wgts"]] <- network_model[[layr]][["wgts"]] + t(grad_step_wgts)
        network_model[[layr]][["bias"]] <- network_model[[layr]][["bias"]] + grad_step_bias
        
    }
    
    # Return
    return(network_model)
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Training Functions                                                     ####
#                                                                              #
#------------------------------------------------------------------------------#

plt_PlotLearningCurve <- function(model_cost, 
                                  input_nodes, hidden_nodes, output_nodes, 
                                  initialisation_algorithm, initialisation_order, 
                                  activation_hidden, activation_final, 
                                  epochs, learning_rate
) {
    #' @title Plot Model Learning Curve
    #' @description Plot model learning curve.
    #' @note Add a note for the developer.
    #' @param model_cost `numeric` `vector`. The learning curve for the model.
    #' @param input_nodes `number`. The number of nodes in the input layer.
    #' @param hidden_nodes `numeric` `vector`.
    #' @param output_nodes `number`.
    #' @param initialisation_algorithm `string`.
    #' @param initialisation_order `string`.
    #' @param activation_hidden `string`.
    #' @param activation_final `string`.
    #' @param epochs `number`.
    #' @param learning_rate `number`.
    #' @return Nothing is being returned.
    #' @seealso 
    #' @author chrimaho
    #' @examples
    #' # Works
    #' plt_PlotLearningCurve(
    #'     model_cost,
    #'     input_nodes
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that(model_cost %>% {is.vector(.) & is.numeric(.)}, msg="'model_cost' must be type 'vector' and 'numeric'.")
    assert_that(input_nodes %>% is.number, msg="'input_nodes' must be type 'number'.")
    assert_that(hidden_nodes %>% {is.vector(.) & is.numeric(.)}, msg="'hidden_nodes' must be type 'vector' and 'number'.")
    assert_that(output_nodes %>% is.number, msg="'output_nodes' must be type 'number'.")
    assert_that(initialisation_algorithm %>% is.string, msg="'initialisation_algorithm' must be type 'string'.")
    assert_that(initialisation_order %>% {is.string(.) | is.number(.)}, msg="'initialisation_order' must be type 'number'.")
    assert_that(activation_hidden %>% is.string, msg="'activation_hidden' must be type 'string'.")
    assert_that(activation_final %>% is.string, msg="'activation_final' must be type 'string'.")
    assert_that(epochs %>% is.number, msg="'epochs' must be type 'number'.")
    assert_that(learning_rate %>% is.number, msg="'learning_rate' must be type 'number'.")
    
    # Do work
    plot <- model_cost %>% 
        data.frame(cost=.) %>% 
        rowid_to_column("epoch") %>% 
        ggplot() +
        geom_line(aes(epoch,cost)) +
        coord_cartesian(ylim=c(0,1)) +
        labs(
            title=paste0("Learning Curve for Neural Network"),
            subtitle=paste0(
                "Inpt: '", input_nodes, "',  Hidd: '", paste0(hidden_nodes, collapse=","), "',  Outp: '", output_nodes, "'\n",
                "Init Alg: '", initialisation_algorithm, "',  Init Ord: '", initialisation_order, "'\n",
                "Acti Hid: '", activation_hidden, "',  Acti Out: '", activation_final, "'\n",
                "Epochs: '", epochs, "',  Lrn Rate: '", learning_rate, "'\n"
            ),
            x="Epoch",
            y="Cost"
        )
    
    # Return
    return(plot)
}


get_BatchIndexes <- function(vector, batches=get_Modulus(length(vector))[4], batch=1, seed=sample(1:100,1)) {
    #' @title Get the batches for a given vector
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param vector `vector`. The vector to be batched up.
    #' @param batches `integer`. The number of batches required. Default value `get_Modulus(dim(vector)[1])[4]`.
    #' @param batch `number`. The batch number required. Default value `1`.
    #' @return A vector with the same length as `vector` comprising of logical values where `TRUE` is the index for the given `batch` and `FALSE` is everything else.
    #' @seealso 
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_VectorBatches(
    #'     vector=NA,
    #'     batches=get_Modulus(dim(vector)[1])[7]
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    require(groupdata2)
    
    # Validations
    assert_that(vector %>% is.vector, msg="'vector' must be type 'vector'.")
    assert_that(batches %>% is.integer, msg="'batches' must be type 'integer'.")
    assert_that(batch %>% is.integer, msg="'batch' must be type 'integer'.")
    assert_that(batch <= batches, msg="The required 'batch' must be less than, or equal to, the number of 'batches'.")
    
    set.seed(seed)
    
    # Do work and return
    vector %>% 
        data.frame(dat=.) %>% 
        fold(batches) %>% 
        ungroup() %>% 
        rename("folds"=2) %>%
        mutate(bat=ifelse(folds==batch,TRUE,FALSE)) %>% 
        select(bat) %>% 
        pull %>% 
        return()
}


let_TrainModel <- function(x_train, y_train,
                           input_nodes=dim(x_train)[2], hidden_nodes=c(100, 50, 10), output_nodes=1,
                           initialisation_algorithm="xavier", initialisation_order="layers",
                           activation_hidden="relu", activation_final="sigmoid",
                           batches=get_Modulus(dim(x_train)[1])[4], epochs=500, learning_rate=0.001,
                           verbosity=NA, print_learning_curve=TRUE
) {
    #' @title Train network model
    #' @description Parse in the relevant parameters, and then instantiate, initialise, forward propagate, assess, differentiate, backward propagate and update model. Then repeat this process `epoch` number of times.
    #' @note The model will be re-created every time this function is run.
    #' @param x_train `array`. The 4-D array of images for training.
    #' @param y_train `array`. The 2-D array of labels for each image.
    #' @param input_nodes `integer`.
    #' @param hidden_nodes `vector` of `integer`s.
    #' @param output_nodes `integer`.
    #' @param initialisation_algorithm `string` or `NA`.
    #' @param initialisation_order `integer` or `string`.
    #' @param activation_hidden `string`.
    #' @param activation_final `string`.
    #' @param batches `integer`.
    #' @param epochs `integer`.
    #' @param learning_rate `number`.
    #' @param verbosity `integer` or `NA`.
    #' @param print_learning_curve `logical`.
    #' @return A list containing the `results`, and the final trained `network_model`.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' let_TrainModel(
    #'     x_train,
    #'     y_train
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    require(magrittr)
    
    # Validations
    assert_that(x_train %>% is.array, msg="'x_train' must be type 'array'.")
    assert_that(y_train %>% is.array, msg="'y_train' must be type 'array'.")
    assert_that(input_nodes %>% is.integer, msg="'input_nodes' must be type 'integer'.")
    assert_that(hidden_nodes %>% is.vector, msg="'hidden_nodes' must be type 'vector'.")
    assert_that(hidden_nodes %>% is.integer %>% all, msg="All elements of 'hidden_nodes' must be integers.")
    assert_that((hidden_nodes > 0) %>% all, msg="All elements of 'hidden_nodes' must be greater than '0'.")
    assert_that(output_nodes %>% is.integer, msg="'input' must be type 'integer'.")
    assert_that(initialisation_algorithm %>% {is.string(.) | is.na(.)}, msg="'initialisation_algorithm' must be type 'string' or value 'NA'.")
    assert_that(initialisation_algorithm %in% c("xavier","he",NA), msg="'initialisation_algorithm' must be one of 'xavier', 'he', or 'NA'.")
    assert_that(initialisation_order %>% {is.integer(.) | is.string(.)}, msg="'initialisation_order' must be type 'integer' or 'string'.")
    assert_that(is.string(activation_hidden), msg="'activation_hidden' must be type 'string'.")
    assert_that(is.string(activation_final), msg="'activation_final' must be type 'string'.")
    assert_that(activation_hidden %in% c("sigmoid","relu","softmax","swish"), msg="'activation_hidden' must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'.")
    assert_that(batches %>% is.integer, msg="'batches' must be type 'integer'.")
    assert_that(dim(x_train)[1] %% batches == 0, msg="The number of images in 'x_train' should be equally divisible by 'batches'. Try another value for 'batches' instead. Suggested: {}." %>% str_Format(get_Modulus(dim(x_train)[1])[4]))
    assert_that(epochs %>% is.integer, msg="'epochs' must be type 'integer'.")
    assert_that(learning_rate %>% is.number, msg="'learning_rate' must be type 'number'.")
    assert_that(between(learning_rate, 0, 1), msg="'learning_rate' must be between '0' and '1'.")
    assert_that(activation_final %in% c("sigmoid","relu","softmax","swish"), msg="'activation_final' must be one of: 'sigmoid', 'relu', 'softmax', or 'swish'.")
    assert_that(verbosity %>% {is.integer(.) | is.na(.)}, msg="'verbosity' must be type 'integer' or value 'NA'.")
    assert_that(print_learning_curve %>% is.logical, msg="'print_learning_curve' must be type 'logical'.")
    
    # Begin the timer
    time_begin <- Sys.time()
    
    # Set return values
    output <- list(
        network_model=network_model,
        results=list(
            cost=vector()
            # Open to add future results features, such as accuracy or specificity.
        )
    )
    
    # Instantiate
    network_model <- set_InstantiateNetwork(
        input=input_nodes,
        hidden=hidden_nodes, 
        output=output_nodes
    )
    
    # Initialise
    network_model <- set_InitialiseModel(
        network_model=network_model, 
        initialisation_algorithm=initialisation_algorithm, 
        initialisation_order=initialisation_order
    )
    
    # Loop each epoch
    for (epoch in 1:epochs) {
        
        # Loop each batch
        for (batch in 1:batches) {
            
            # Set indices
            batch_indexes <- get_BatchIndexes(
                vector=1:dim(x_train)[1], 
                batches=batches, 
                batch=batch 
                # seed=1234
            )
            
            # Set data
            x_train_batch <- x_train[batch_indexes,]
            y_train_batch <- y_train[batch_indexes]
            
            # Forward Prop
            network_model <- set_ForwardProp(
                network_model=network_model, 
                data_in=x_train_batch, 
                activation_hidden=activation_hidden, 
                activation_final=activation_final
            )
            
            # Get cost
            cost <- get_ComputeCost(
                pred=network_model[["output"]][["acti"]], 
                true=y_train_batch, 
                epsi=1e-10
            )
            
            # Apply cost
            network_model <- set_ApplyCost(
                network_model=network_model, 
                cost=cost
            )
            
            # Differentiate cost
            network_model <- set_ApplyDifferentiateCost(
                network_model=network_model, 
                get_DifferentiateCost(network_model[["output"]][["acti"]], y_train_batch)
            )
            
            # Backprop
            network_model <- set_BackwardProp(network_model)
            
            # Update parameters
            network_model <- set_UpdateModel(
                network_model=network_model, 
                learning_rate=learning_rate
            )
            
        }
        
        # Save cost
        output[["results"]][["cost"]] %<>% c(cost)
        
        # Print update
        if (!is.na(verbosity)) {
            if (epoch %% verbosity == 0) {
                if (epoch == verbosity) {
                    cat("Learning rate: {}\n" %>% str_Format(learning_rate))
                }
                cat("Epoch {}, Cost: {}, Time: {}\n" %>% str_Format(epoch, cost, get_TimeDifference(time_begin)))
            }
        }
        
    }
    
    # Re-apply back to the output list
    output[["network_model"]] <- network_model
    
    # Print the results
    if (print_learning_curve == TRUE) {
        
        tryCatch(
            expr={
                output[["results"]][["cost"]] %>% 
                    plt_PlotLearningCurve(
                        input_nodes=input_nodes, hidden_nodes=hidden_nodes, output_nodes=output_nodes,
                        initialisation_algorithm=initialisation_algorithm, initialisation_order=initialisation_order,
                        activation_hidden=activation_hidden, activation_final=activation_final,
                        epochs=epochs, learning_rate=learning_rate
                    ) %>% 
                    print()
            },
            warning=function(message){
                writeLines("A Warning occurred:")
                writeLines(message)
                return(invisible(NA))
            },
            error=function(message){
                writeLines("An Error occurred:")
                writeLines(message)
                return(invisible(NA))
            },
            finally={
                #Do nothing...
            }
        )
        
    }
    
    # Return
    return(output)
}


#------------------------------------------------------------------------------#
#                                                                              #
#    Predicting and Assessment Functions                                    ####
#                                                                              #
#------------------------------------------------------------------------------#

get_Prediction <- function(x_test, y_test, network_model, threshold=0.5) {
    #' @title Get Prediction from Model
    #' @description Use the `network_model` to forward-propagate `x_test` to create a set of predictions. Then, compare these predictions with `y_test`.
    #' @note Add a note for the developer.
    #' @param x_test `array`. A 4-D array of images.
    #' @param y_test `array`. A 2-D array of labels.
    #' @param network_model `list`. The trained network_model.
    #' @param threshold `number`. A single value between `0` & `1` that is the threshold for the predicted probabilities.
    #' @return A `data.frame` with three columns: 1) `probs`: The Probability value of each of the labels, and is the same length as `y_test`; 2) `truth`: The true labels, the exact same vector as `y_test`; 3) `class`: The class of values, defined at a given cutoff value.
    #' @author chrimaho
    #' @examples
    #' # Works
    #' get_Prediction(
    #'     x_test=NA,
    #'     y_test=NA
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    require(magrittr)
    
    # Validations
    assert_that(x_test %>% is.array, msg="'x_test' must be type 'array'.")
    assert_that(y_test %>% is.array, msg="'y_test' must be type 'array'.")
    assert_that(threshold %>% is.number, msg="'threshold' must be type 'number'.")
    assert_that(between(threshold, 0, 1), msg="'threshold must be between '0' and '1'.")
    assert_that(network_model %>% is.list, msg="'network_model' must be type 'list'.")
    assert_that(network_model %>% names %>% extract(1) == "input", msg="The first layer of 'network_model' must be 'input'.")
    assert_that(network_model %>% names %>% rev %>% extract(1) == "output", msg="The last layer of 'network_model' must be 'output'.")
    for (name in network_model %>% names) {
        if (!name %in% c("input","output")) {
            assert_that(name %>% as.numeric %>% is.integer, msg="Each hidden layer in 'network_model' must be an integer value.")
        }
    }
    
    # Create prediction
    predic <- set_ForwardProp(
        network_model=network_model, 
        data_in=x_test, 
        activation_hidden="relu", 
        activation_final="sigmoid"
    )
    
    # Extract probabilities
    probas <- predic[["output"]][["acti"]]
    
    # Define results
    result <- data.frame(
        probs=probas,
        truth=y_test
    )
    
    # Add class
    result %<>% 
        mutate(class=ifelse(probas>threshold, 1, 0))
    
    # Return
    return(result)
}


plt_ConfusionMatrix <- function(confusion_matrix) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param confusion_matrix `caret::confusionMatrix()`. The confusion matrix, as produced by `cared::confusionMatrix()`.
    #' @return A ggplot object.
    #' @seealso https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot
    #' @author chrimaho
    #' @examples
    #' # Works
    #' plt_ConfusionMatrix(
    #'     confusion_matrix=NA,
    #'     title=NA
    #' )
    
    # Packages
    require(assertthat)
    require(dplyr)
    
    # Validations
    assert_that((confusion_matrix %>% class) == "confusionMatrix", msg="'confusion_matrix' must be class 'confusionMatrix()', as generated by the 'caret::confusionMatrix()' function.")
    
    # Do work
    plot <- confusion_matrix %>% 
        extract("table") %>% 
        as.data.frame() %>% 
        rename_all(str_remove_all, "table.") %>% 
        rename("Prediction"=1, "Reference"=2) %>% 
        mutate(goodbad = ifelse(Prediction == Reference, "good", "bad")) %>%
        group_by(Reference) %>% 
        mutate(prop = Freq/sum(Freq)) %>% 
        ungroup() %>% 
        {
            ggplot(., aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
                geom_tile() +
                geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
                scale_fill_manual(values = c(good = "green", bad = "red")) +
                scale_x_discrete(limits=levels(.$Reference), position="top") +
                scale_y_discrete(limits=rev(levels(.$Prediction))) +
                labs(
                    title="Confusion Matrix",
                    subtitle=paste0("For: '", .$Prediction[1], "' vs '", .$Prediction[2], "'")
                )
        }
    
    # Return
    return(plot)
}
