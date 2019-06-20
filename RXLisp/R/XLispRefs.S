"$.XLispReference" <-
function(x, name)
{
  function(...) {
   .XLisp("SEND", x, paste(":", name, sep=""), ...)
  }
}


"[[.XLispReference" <-
function(x, i, ...)
{
  .XLisp("SEND", x, paste(":", as.character(i), sep=""), ...)
}

if(FALSE) {
"[[<-.XLispReference" <-
function(x, i, ..., value)
{
  .XLisp("SEND", x, ":slot-value", as.character(i),  value)
}
}
