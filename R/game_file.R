#' Get raw game text data from a quackle or cross-tables game number or link or filepath.
#'
#' @param x a quackle or cross-tables game number or link, or file path
#' @return data.frame game summary
#' @examples
#' game_file(28283)
#' game_file("http://www.cross-tables.com/annotated/selfgcg/281/anno28152.gcg")
#' game_file("http://www.cross-tables.com/annotated.php?u=28190#0#")
#' @export


game_file <- function(v){
  #insert error
  if(!grepl("cross", v) & !grepl("gcg", v) & !is.numeric(v)) { stop ("Game must be a x-tables game number or link or file path of a saved quackle file") }
  if (grepl("gcg", v)) {v1 <- readLines(v)}
  if (grepl("annotated.php", v)) {
    v0 <- sub('\\#.*', '', sub('.*\\=', '', v))
    v1 <- readLines(paste0("http://www.cross-tables.com/annotated/selfgcg/",substr(v0,1,3),"/anno",v0,".gcg"))
    }
  if (is.numeric(v)) {v1 <- readLines(paste0("http://www.cross-tables.com/annotated/selfgcg/",substr(v,1,3),"/anno",v,".gcg"))}

  return(v1)
}

