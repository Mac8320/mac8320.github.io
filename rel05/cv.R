# Calculando o coeficiente de variação no leem

cv <- function (x, rounding = 2, na.rm = FALSE, details = FALSE, grouped = TRUE)
{
  if (class(x) != "leem") 
    stop("Use the 'new_leem()' function to create an object of class leem!", 
         call. = FALSE)
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      cvar <- round(100 * sd(x = x$estat$raw_data, na.rm = na.rm)/mean(x = x$estat$raw_data, na.rm = na.rm), digits = rounding)
      resume <- list(cv = cvar, table = x$tabela, rawdata = x$estat$raw_data)
      
      if (details) {
        return(resume)
      }
      else {
        return(cvar)
      }
    }
    else {
      stop("Measure not used for this data type!", 
           call. = FALSE, domain = "R-leem")
    }
  }
  
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
  cvar <- 100*(sdev(x)/mean(x))
  resume <- list(cv = cvar, table = x$tabela, rawdata = x$estat$raw_data)
  
  if (details) {
    return(resume)
  }
  else {
    return(cvar)
  }
    } else {
      
      cvar <- round(100*sd(x = x$estat$raw_data, na.rm = na.rm)/mean(x = x$estat$raw_data, na.rm = na.rm),
                       digits = rounding)
      resume <- list(sdeviation = cvar, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(cvar)
      }
    }
  }
}

set.seed(10)
x <- rnorm(36, 100, 50)
x <- new_leem(x, variable = 2)
cv(x)
