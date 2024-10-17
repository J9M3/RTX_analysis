pacman::p_load(quantmod, magrittr, data.table, lubridate, ggplot2 , readxl , stringr , lubridate)


## Get RTX data
quantmod::getSymbols("RTX")
RTX_OHCL = as.data.table(RTX)
RTX_OHCL <- RTX_OHCL[index >= as.Date("2019-12-01") , ]
OHCL_cols <- names(RTX_OHCL)
rm(RTX)

## Get 10Qs 

path = "/Users/j9m3/Documents/Personal/Trading/RTX10KQ"
files = list.files(path)


xl_list <- lapply(files , function(x){
  # Load page
  y <- readxl::read_xls(path = paste0(path , "/" , x  ) ,
                        sheet = "condensed consolidated bal") %>% as.data.table()

  # Take non logical cols
  cols <- names(which(sapply(y, is.character)))
  
  y <- y[ , .SD , .SDcols =  cols ] %>% na.omit()
  data.table::setnames(y , old = names(y) , new = c("lead" , "c1" , "c2") )
  y <- y[ lead != "Assets related to discontinued operations"
          & lead != "Future income tax benefits", ]
  
  return(y)
})


xl_table <- do.call(cbind , xl_list) %>% as.data.table()


names(xl_table) <- paste0(names(xl_table) , 1:39)
xl_table <- cbind(xl_table[, 1] , xl_table[, .SD , .SDcols = names(xl_table)[grepl("c" , names(xl_table))]])
xl_table[1,1] <- "Date"

xl_table <- data.table::transpose(xl_table )

tmp_names <- xl_table[1,] %>%
  as.character() %>%
  stringr::str_replace_all(. , "," , "_") %>% 
  stringr::str_replace_all(. , " " , "_") %>% 
  stringr::str_replace_all(. , "`" , "_") %>% 
  stringr::str_replace_all(. , "-" , "_")

names(xl_table) <- tmp_names
xl_table <- xl_table[-1,]


cols2convert <- names(xl_table)[-1]
xl_table[, (cols2convert) :=lapply(.SD ,  as.numeric) , .SDcols = cols2convert]
xl_table[, Date := lubridate::mdy(Date)]
xl_table <- xl_table[order(Date)]
xl_table <- unique(xl_table)

earning_dates <- xl_table$Date

xl_with_price = data.table::merge.data.table(x = xl_table ,
                                             y = RTX_OHCL ,
                                             by.x = "Date",
                                             by.y = "index", 
                                             all.y = T, 
                                             all.x = T)


xl_with_price[ , (OHCL_cols[-1]) :=data.table::nafill(.SD , type = "locf" ), .SDcols = OHCL_cols[-1]]

xl_with_price[Date %in% earning_dates , ] %>% View
