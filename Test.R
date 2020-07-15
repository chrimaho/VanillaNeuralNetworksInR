LinearForward <- function(inpt, wgts, bias) {
    
    #inpt <- inpt %>% t()
    linr <- inpt %*% wgts
    linr <- sweep(linr, 2, bias, "+")
    return(linr)
    
}

ForwardProp <- function(data_in, network_model, activation_hidden="relu", activation_final="sigmoid") {
    
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

network_model <- ForwardProp(trn_img, network_model, "relu", "sigmoid")
