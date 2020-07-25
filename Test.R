trn_img %>% array(dim=c(
    dim(.) %>% extract(1), 
    dim(.) %>% extract(2:4) %>% prod() 
))

