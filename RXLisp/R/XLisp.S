.XLispInit <-
function(args=.XLispInitArgs, registerEvent = TRUE)
{
 .Call("RXLisp_init", as.character(args), as.logical(registerEvent))
 .XLisp("load", system.file("xlisp", "ObjectManager.lsp", package="RXLisp"))
}


.XLisp <-
function(fun, ..., .convert = TRUE, upper = TRUE)
{
  fun <- as.character(fun)
  if(upper)
    fun <- toupper(fun)

  args <- list(...)

  argNames <- names(args) != ""
  if(any(argNames)) {
     # combine the argument names with the arguments
     # to create, e.g.
     #  (arg1, arg2, name1, arg3, arg4, name2, arg5)
     # from input
     #  (arg1, arg2, name1=arg3, arg4, name2=arg5)
     # There is a way to vectorize this but this is
     # trivial and understandable.
    tmp <- list(sum(argNames) + length(args))
    pos <- 1
    for(i in 1:length(args)) {
     if(argNames[i]) {
       tmp[[pos]] <- paste(":", names(args)[i], sep="")
       pos <- pos + 1
     }
     tmp[[pos]] <- args[[i]]
     pos <- pos + 1     
    }

    args <- tmp
  }

  args <- lapply(args,
                   function(x) if(is.character(x) && length(x) == 1 && substr(x, 1, 1) == ":")
                                    toupper(x)
                               else
                                    x)

  .Call("RXLisp_call", fun, args, as.logical(.convert))
}


  
