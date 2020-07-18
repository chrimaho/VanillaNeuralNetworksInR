str_Format <- function(string, ...) {
    #' @title String Formatter
    #' @description Take an input string, and substitute in-string variables.
    #' @note This is similar to the Python `string.foramt()` method.
    #' @param string `string`. The string to be re-formatted. Note, each of the named arguments must be surrounded in curly brackets.
    #' @param ... `variables`. A list of variables. Note, these can either be named or not; but they must all be named, or all be blank, because it cannot handle a mixture. Each of these arguments must align to the variables in curly brackets from the `string` argument. These will be combined in to a list.
    #' @return A formatted string
    #' @examples 
    #'     str_Format(
    #'         "Sammy the {animal} {verb} a {noun}.", 
    #'         animal="shark", 
    #'         verb="made", 
    #'         noun="house"
    #'     )
    #'     
    #'     str_Format(
    #'         "Sammy the {} {} a {}.", 
    #'         "shark", 
    #'         "made", 
    #'         "house"
    #'     )
    #'     
    #'     "Sammy the {animal} {verb} a {noun}" %>% 
    #'         str_Format(animal="shark", verb="made", noun="house")
    #'     
    #'     "Sammy the {} {} a {}" %>%
    #'         str_Format("shark", "made", "house")
    #' @seealso https://stackoverflow.com/questions/44763056/is-there-an-r-equivalent-of-pythons-string-format-function#answer-44763659
    #' @author chrimaho
    
    # Import packages
    require(stringr)
    require(magrittr)
    require(dplyr)
    require(assertthat)
    require(dynutils)
    require(english)
    
    # Validations
    assert_that(is.string(string))
    assert_that(c("stringr", "magrittr", "dplyr", "assertthat", "dynutils", "english") %all_in% .packages(), msg="The packages must be mounted.")
    
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


get_CountOfElementsWithCondition <- function(list_of_elements, condition=NULL) {
    #' @title Get Count of Elements with Condition
    #' @description Get the count of the number of elements in a list that meet a specified condition.
    #' @note The `condition` must be a hidden function. Also note that the `warnings` are suppressed when running the `condition` function.
    #' @param list_of_elements `vector`. The list of elements to check.
    #' @param condition `function`. The condition for checking. Must be a hidden function.
    #' @return An integer.
    #' @examples
    #'     # Returns condition
    #'     get_CountOfElementsWithCondition(
    #'         c("i", "1", "2", "3", "o"),
    #'         function(x) {IsWhole(as.numeric(x))}
    #'     )
    #'     
    #'     # Returns length
    #'     get_CountOfElementsWithCondition(
    #'         c("i", "1", "2", "3", "o")
    #'     )
    #'     
    #'     # Returns error
    #'     get_CountOfElementsWithCondition(
    #'         c("i", "1", "2", "3", "o"),
    #'         2
    #'     )
    #' @seealso https://thispointer.com/python-count-elements-in-a-list-that-satisfy-certain-conditions/#crayon-5ea195c434f39109077492-1
    #' @author chrimaho
    
    # Import packages
    require(assertthat)
    
    # Validations
    assert_that(is_vector(list_of_elements))
    assert_that(or(is_function(condition), is.null(condition)), msg="'condition' must either be a function or the value 'NULL'.")
    
    # Do work
    if (!is.null(condition)) {
        count <- sum(suppressWarnings(condition(list_of_elements)), na.rm=TRUE)
    } else {
        count <- length(list_of_elements)
    }
    
    # Return
    return(count)
}

get_ObjectAttributes <- function(object, name) {
    #' @title Get Attributes
    #' @description Extract and print the key attributes of an object, including `name`, `size`, `class`, `type`, `mode`, `dims`.
    #' @note In order to do pretty-print, parse the result of this function in to the `cat()` function.
    #' @param object any. The object to be checked.
    #' @param name string. The name of the object.
    #' @return A string that contains all the relevant information.
    #' @author chrimaho
    
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

set_MakeImage <- function(images, index=1) {
    #' @title Slice Array
    #' @description Slice the images at a given index
    #' @note Add a note for the developer.
    #' @seealso https://stackoverflow.com/questions/32113942/importing-cifar-10-data-set-to-r#answer-39672323
    #' @param images array. The array to be sliced.
    #' @param index int. The integer to be sliced
    #' @return An `rgb()` object.
    #' @author chrimaho
    
    # Libraries
    require(dplyr)
    require(grDevices)
    require(assertthat)
    
    # Validations
    assert_that(is.array(images))
    assert_that(is.count(index))
    assert_that(images %>% dim %>% length == 4, msg="'images' must have 4 dimensions.")
    
    # Extract elements
    img <- images[index,,,]
    img.r <- img[,,1]
    img.g <- img[,,2]
    img.b <- img[,,3]
    
    # Make rgb
    img.rgb <- rgb(img.r, img.g, img.b, maxColorValue=255)
    
    # Fix dimensions
    dim(img.rgb) <- dim(img.r)
    
    # Return
    return(img.rgb)
}

plt_PlotImage <- function(images, classes, index=1) {
    #' @title Plot Array
    #' @description Plot a given image.
    #' @note Add a note for the developer.
    #' @seealso https://stackoverflow.com/questions/12918367/how-to-plot-with-a-png-as-background#answer-12918368
    #' @param images array. The image to be plotted.
    #' @param index int. The index of the image to be plotted.
    #' @return Nothing is returned.
    #' @author chrimaho
    
    # Libraries
    require(dplyr)
    require(grDevices)
    require(assertthat)
    require(grid)
    
    # Validations
    assert_that(is.array(images))
    assert_that(is.count(index))
    assert_that(images %>% dim %>% length == 4, msg="'images' must have 4 dimensions.")
    
    # Slice images
    img <- set_MakeImage(images, index)
    lbl <- classes[index] %>% as.character() %>% ClassList[[.]]
    
    # Plot image
    plot.new()
    lim <- par()
    rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4], interpolate=FALSE)
    title(lbl, font.main=2)
    
    # Return
    return(invisible(NULL))
}

InstantiateNetwork <- function(input=50, hidden=c(30,20,10), output=10) {
    #' @title Add function title
    #' @description Add function description.
    #' @note Add a note for the developer.
    #' @param input integer. What is Input1?
    #' @param hidden vector of integers. What is Input1?
    #' @param output integer. What is Input1?
    #' @return What is being returned?
    #' @author chrimaho
    
    # Validations
    assert_that(IsWhole(input))
    assert_that(is_vector(hidden))
    assert_that(all(IsWhole(hidden)))
    assert_that(IsWhole(output))
    
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
            "nodz"      = ""  #<-- Number of nodes in this layer.
            ,"inpt"      = ""  #<-- Input matrix. Aka 'A_prev'. This is a duplicate of the activation of the previous layer, so for large networks this needs to be taken in to consideration.
            ,"wgts"      = ""  #<-- Weights matrix. Aka 'W'.
            ,"bias"      = ""  #<-- Bias vector. Aka 'b'.
            ,"linr"      = ""  #<-- Linear matrix. Aka 'Z'. This is the result of the linear algebra between inpt, wgts and bias.
            ,"acti"      = ""  #<-- Activation matrix. Aka 'A'. The result of applying an activation function to the linr matrix.
            ,"acti_func" = ""  #<-- The activation function used.
            ,"cost"      = ""  #<-- The overall cost of the model. This is a single value (the overall cost of the model), but is copied to each layer of the model.
            ,"back_cost" = ""  #<-- Gradient of the cost vector. Aka 'dA_cost'.
            ,"back_acti" = ""  #<-- Gradient of the Activation matrix. Aka 'dA'. The result of differentiation after having applied back propagation. with a given cost function.
            ,"back_linr" = ""  #<-- Gradient of the Linear algebra matrix. Aka 'dZ'. The result of backwards linear differentiation back propagation.
            ,"back_wgts" = ""  #<-- Gradient of the Weights matrix. Aka 'dW'. Also the result of back-prop.
            ,"back_bias" = ""  #<-- Gradient of the Bias vector. Aka 'db'. Also the result of back-prop.
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

InitialiserXavier <- function(nodes_in, nodes_out, order=6) {
    #' @title Xavier Initialisation
    #' @description Initialise the weights based on Xavier initialisation
    #' @note Add a note for the developer.
    #' @references https://towardsdatascience.com/weight-initialization-in-neural-networks-a-journey-from-the-basics-to-kaiming-954fb9b47c79
    #' @references http://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf
    #' @param nodes_in integer. The number of nodes coming in to this layer (ie. number of nodes in previous layer).
    #' @param nodes_out integer. The number of nodes outgoing from this layer (ie. number of nodes in this layer).
    #' @param order integer. The order of magnitude for this equation. Defaults to `6`.
    #' @return A floating point number.
    #' @author chrimaho
    
    # Validations
    assert_that(IsWhole(nodes_in))
    assert_that(IsWhole(nodes_out))
    assert_that(IsWhole(order))
    
    # Do work
    numer <- sqrt(order)
    denom <- sqrt(nodes_in + nodes_out)
    output <- numer/denom
    
    # Return
    return(output)
}

InitialiserHe <- function(nodes_in, order=2) {
    #' @title He Initialisation
    #' @description Initialise the weights based on Xavier initialisation
    #' @note Add a note for the developer.
    #' @references https://towardsdatascience.com/weight-initialization-in-neural-networks-a-journey-from-the-basics-to-kaiming-954fb9b47c79
    #' @references http://proceedings.mlr.press/v9/glorot10a/glorot10a.pdf
    #' @param nodes_in integer. The number of nodes coming in to this layer (ie. number of nodes in previous layer).
    #' @param order integer. The order of magnitude for this equation. Defaults to `2`.
    #' @return A floating point number.
    #' @author chrimaho
    
    # Validations
    assert_that(IsWhole(nodes_in))
    assert_that(IsWhole(order))
    
    # Do work
    numer <- order
    denom <- nodes_in
    output <- sqrt(numer/denom)
    
    # Return
    return(output)
}

InitialiseLayer <- function(network_model, layer_index, initialisation_algorithm=NA, initialisation_order=2) {
    #' @title Initialise Layer
    #' @description Update the weights and biases of a given layer
    #' @note Defaults the initialisation algorithm to the Xavier Initialisation.
    #' @param network_model list. The model to be updated.
    #' @param layer_index integer. The layer to be updated.
    #' @param initialisation_algorithm string. The algorithm to be used for initialisation (eg. `xavier` or `he`).
    #' @param initialisation_order integer. The order of magnitude for the initialisation (can be either an integer, or set to the number of layers defined).
    #' @return The updated model, with the relevant layer updated.
    #' @author chrimaho
    
    # Validations
    assert_that(is_list(network_model))
    assert_that(IsWhole(layer_index))
    assert_that(initialisation_algorithm %in% c("xavier", "he", NA), msg="'initialisation_algorithm' must be one of 'xavier', 'he', or 'NA'.")
    assert_that(IsWhole(initialisation_order))
    
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
    
    # Initialise weight matrix
    w_matrix <- matrix(
        data=rnorm(nodes_in * nodes_out), 
        nrow=nodes_in,
        ncol=nodes_out
    )
    
    # Scale weights
    if (layer_index != 1) {
        if (initialisation_algorithm == "xavier") {
            w_matrix <- w_matrix * InitialiserXavier(nodes_in, nodes_out, order=initialisation_order)
        } else if (initialisation_algorithm == "he") {
            w_matrix <- w_matrix * InitialiserHe(nodes_in, order=initialisation_order)
        } else {
            w_matrix <- w_matrix
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

InitialiseModel <- function(network_model, initialisation_algorithm="xavier", initialisation_order="layers") {
    #' @title Initialise Model
    #' @description Initialise each layer in the model.
    #' @note 
    #' @param network_model list. The model to be initialised.
    #' @param initialisation_algorithm string. The initialisation algorithm to be used.
    #' @param initialisation_order string or integer. The order of magnitude for the initialisation (either integer or set to the number of layers defined) 
    #' @return The initialised model.
    #' @author chrimaho
    
    # Validations
    assert_that(is_list(network_model))
    assert_that(initialisation_algorithm %in% c("xavier", "he", NA), msg="'initialisation_algorithm' must be one of 'xavier', 'he', or 'NA'.")
    assert_that(or(IsWhole(initialisation_order), is.string(initialisation_order)), msg="'initialisation_order' must be either type 'integer' or 'string'.")
    
    # Redefine 'initialisation_order'
    if (initialisation_order == "layers") {
        initialisation_order <- get_CountOfElementsWithCondition(names(network_model), function(x){IsWhole(as.numeric(x))})
    }
    
    # Initialise each layer
    for (layer_index in 1:length(names(network_model))) {
        network_model <- InitialiseLayer(
            network_model=network_model, 
            layer_index=layer_index, 
            initialisation_algorithm=initialisation_algorithm,
            initialisation_order=initialisation_order
        )
    }
    
    # Return
    return(network_model)
}

sigmoid <- function(z) {
    # References:
    # https://kite.com/python/answers/how-to-calculate-a-logistic-sigmoid-function-in-python
    # https://www.geeksforgeeks.org/implement-sigmoid-function-using-numpy/
    a <- 1/(1+exp(-z))
    return(a)
}

relu <- function(z) {
    # References:
    # https://medium.com/ai%C2%B3-theory-practice-business/a-beginners-guide-to-numpy-with-sigmoid-relu-and-softmax-activation-functions-25b840a9a272
    a <- sapply(z, max, 0) %>% 
        structure(dim=dim(z))
    return(a)
}

softmax <- function(z) {
    # Reference: 
    # https://medium.com/ai%C2%B3-theory-practice-business/a-beginners-guide-to-numpy-with-sigmoid-relu-and-softmax-activation-functions-25b840a9a272
    expo <- exp(z)
    expo_sum <- sum(exp(z))
    a <- expo/expo_sum
    return(a)
}

swish <- function(z, beta=0.1) {
    # References: 
    # https://arxiv.org/pdf/1710.05941.pdf
    # https://www.bignerdranch.com/blog/implementing-swish-activation-function-in-keras/
    a <- z * (beta*z)
    return(a)
}

ForwardProp <- function(network_model, data_in, activation_hidden="relu", activation_final="sigmoid") {
    
    # Validations
    assert_that(is.array(data_in))
    assert_that(is.list(network_model))
    assert_that(names(network_model)[1]=="input")
    assert_that(rev(names(network_model))[1]=="output")
    assert_that(is.string(activation_hidden))
    assert_that(is.string(activation_final))
    assert_that(activation_hidden %in% c("sigmoid","relu","softmax","swish"))
    assert_that(activation_final %in% c("sigmoid","relu","softmax","swish"))
    for (name in names(network_model)) {
        if (!name %in% c("input","output")) {
            assert_that(IsWhole(as.numeric(name)))
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
            linr <- LinearForward(inpt, wgts, bias)
            
            # Activate
            if (layr=="output") {
                acti <- get(activation_final)(linr)
                network_model[[layr]][["acti_func"]] <- activation_final
            } else {
                acti <- get(activation_hidden)(linr)
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

ComputeCost <- function(pred, true, epis=1e-10) {
    
    # Validations
    assert_that(is.matrix(pred))
    assert_that(is.matrix(true))
    assert_that(is.number(epis))
    assert_that(epis < 0.0001, msg="'epis' should be a very small epsilom value.")
    
    # Get number of samples
    samp <- length(true)
    
    # Instantiate totals
    total_cost <- 0
    
    # Loop for each prediction
    for (i in 1:samp) {
        
        # Adjust for perfect predictions.
        pred <<- pred
        i <<- i
        if (pred[i]==1) {pred[i] <- pred[i]-epis} #pred[i] %<>% subtract(epis)
        if (pred[i]==0) {pred[i] <- pred[i]+epis} #pred[i] %<>% add(epis)
        
        # Calculate totals
        total_cost <- total_cost - ((true[i] * log(pred[i]) + (1-true[i]) * log(1-pred[i])))
        
    }
    
    # Take an average
    cost <- (1/samp) * total_cost
    
    # Return
    return(cost)
    
}

ApplyCost <- function(network_model, cost) {
    
    # Apply back to the model
    for (layer in names(network_model)) {
        network_model[[layer]][["cost"]] <- cost
    }
    
    return(network_model)
}

DifferentiateCost <- function(pred, true) {
    diff_cost <- -(divide_by(true, pred) - divide_by(1-true, 1-pred))
    return(diff_cost)
}

ApplyDifferentiateCost <- function(network_model, cost_differential) {
    for (layer in names(network_model)) {
        network_model[[layer]][["back_cost"]] <- cost_differential
        if (layer=="output") {
            network_model[[layer]][["back_acti"]] <- network_model[[layer]][["back_cost"]] %>% t()
        }
    }
    return(network_model)
}

DifferentiateLinear <- function(back_linr_curr, acti_prev, wgts, bias) {
    
    samp <- dim(acti_prev)[2]
    
    # print(dim(back_linr_curr))
    # print(dim(acti_prev))
    # print(dim(wgts))
    
    diff_wgts <- 1/samp * (back_linr_curr %*% acti_prev)
    diff_bias <- 1/samp * rowSums(back_linr_curr, dims=1)
    diff_acti_prev <- wgts %*% back_linr_curr
    
    list_linr <- list(
        diff_acti_prev, 
        diff_wgts, 
        diff_bias
    )
    
    return(list_linr)
    
}

relu_backward <- function(diff_acti_curr, linr_curr) {
    diff_linr_curr <- diff_acti_curr
    diff_linr_curr[linr_curr<=0] <- 0
    return(diff_linr_curr)
}

sigmoid_backward <- function(diff_acti_curr, linr_curr) {
    temp <- 1/(1+exp(-linr_curr))
    diff_linr_curr <- t(diff_acti_curr) * temp * (1-temp)
    return(t(diff_linr_curr))
}

BackwardProp <- function(network_model) {
    
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
        acti_func_back <- network_model[[layr_curr]][["acti_func"]] %>% paste0("_backward")
        diff_acti_curr <- network_model[[layr_curr]][["back_acti"]]
        diff_linr_curr <- matrix()
        diff_acti_prev <- matrix()
        diff_wgts_curr <- matrix()
        diff_bias_curr <- matrix()
        
        # Differentiate activation
        diff_linr_curr <- get(acti_func_back)(diff_acti_curr, linr_curr)
        
        # Differentiate linear
        list_linr <- DifferentiateLinear(
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

UpdateModel <- function(network_model, learning_rate) {
    
    for (index in 1:length(names(network_model))) {
        
        # Get layer name
        layr <- names(network_model)[index]
        
        # Skip 'input' layer
        if (layr=="input") next
        
        # Define gradient steps
        grad_step_wgts <- -1 * (learning_rate * network_model[[layr]][["back_wgts"]])
        grad_step_bias <- -1 * (learning_rate * network_model[[layr]][["back_bias"]])
        
        # Take steps
        network_model[[layr]][["wgts"]] <- network_model[[layr]][["wgts"]] + t(grad_step_wgts)
        network_model[[layr]][["bias"]] <- network_model[[layr]][["bias"]] + grad_step_bias
        
    }
    
    return(network_model)
    
}

TrainModel <- function(x_train, y_train,
                       input_nodes=dim(x_train)[2], hidden_nodes=c(100, 50, 10), output_nodes=1,
                       initialisation_algorithm="xavier", initialisation_order="layers",
                       epochs=500, learning_rate=0.001,
                       activation_hidden="relu", activation_final="sigmoid",
                       verbosity=NA
) {
    
    # Set return values
    output <- list(
        network_model=network_model,
        results=list(
            cost=vector()
            # Open to add future results features, such as accuracy or specificity.
        )
    )
    
    # Instantiate
    network_model <- InstantiateNetwork(
        input=input_nodes,
        hidden=hidden_nodes, 
        output=output_nodes
    )
    
    # Initialise
    network_model <- InitialiseModel(
        network_model=network_model, 
        initialisation_algorithm=initialisation_algorithm, 
        initialisation_order=initialisation_order
    )
    
    # Loop each epoch
    for (epoch in 1:epochs) {
        
        # Forward Prop
        network_model <- ForwardProp(
            network_model=network_model, 
            data_in=x_train, 
            activation_hidden=activation_hidden, 
            activation_final=activation_final
        )
        
        # Get cost
        cost <- ComputeCost(network_model[["output"]][["acti"]], y_train, 1e-10)
        
        # Apply cost
        network_model <- ApplyCost(
            network_model=network_model, 
            cost=cost
        )
        
        # Print cost
        if (!is.na(verbosity)) {
            if (epoch %% verbosity == 0) {
                print("With learning rate {}, at epoch {}, the cost is: {}" %>% str_Format(learning_rate, epoch, cost))
            }
        }
        
        # Save cost
        output[["results"]][["cost"]] %<>% c(cost)
        
        # Differentiate cost
        network_model <- ApplyDifferentiateCost(
            network_model=network_model, 
            DifferentiateCost(network_model[["output"]][["acti"]], y_train)
        )
        
        # Backprop
        network_model <- BackwardProp(network_model)
        
        # Update parameters
        network_model <- UpdateModel(
            network_model=network_model, 
            learning_rate=learning_rate
        )
        
    }
    
    output[["network_model"]] <- network_model
    
    return(output)
}

get_Prediction <- function(x_test, y_test, network_model) {
    
    predic <- ForwardProp(
        network_model=network_model, 
        data_in=x_test, 
        activation_hidden="relu", 
        activation_final="sigmoid"
    )
    
    probas <- predic[["output"]][["acti"]]
    
    result <- data.frame(
        probs=probas,
        truth=y_test
    )
    result %<>% mutate(class=ifelse(probas>0.5, 1, 0))
    
    return(result)
    
}

