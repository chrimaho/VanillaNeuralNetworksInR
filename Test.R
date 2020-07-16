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
