parseEval <-
function(cmd)
{
 e <- parse(text = cmd)
 eval(e, env = globalenv())
}
