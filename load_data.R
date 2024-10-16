pacman::p_load(quantmod, magrittr, data.table, lubridate, ggplot2 , readxl)


## Get RTX data
quantmod::getSymbols("RTX")
RTX_OHCL = as.data.table(RTX)
rm(RTX)

## Get 10Qs 

path = "/Users/j9m3/Documents/Personal/Trading/RTX10KQ"
files = list.files(path)
A <- readxl::read_xls(
  path = paste0(path , "/" , files[1]  ), 
  sheet = 4
  ) %>% as.data.table()


xl_list <- lapply(files , function(x){
  # Load page
  y <- readxl::read_xls(path = paste0(path , "/" , x  ) ,
                        sheet = "condensed consolidated bal") %>% as.data.table()

  # Take non logical cols
  cols <- names(which(sapply(y, is.character)))
  
  y <- y[ , .SD , .SDcols =  cols ] %>% na.omit()
  
  return(y)
})


xl_table <- data.table::rbindlist(xl_list, 
                                  fill = T)

dt_cast

list.files(path)
