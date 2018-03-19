#' Get tile distribtuion of a game
#'
#' @param x a dataframe produced by game() function
#' @return table of tile distributions played
#' @examples
#' tile_dist(game(game_file(28283)))
#' @export


tile_dist <- function(df){
  df1  <- df[!df$type=="phony",]
  df1.sp <- split(df1$play, df1$player)
  df1.sp1 <- lapply(df1.sp, function(x) paste(x, collapse = ''))
  df1.sp2 <- lapply(df1.sp1,function(f) gsub("[[:punct:]]", "", f))
  blanks <- lapply(df1.sp2, function(f) gsub("[^a-z]","",f))
  tiledist <- lapply(df1.sp2, function(f) table(factor(unlist(strsplit(gsub("[a-z]","",f),"")),levels=LETTERS[1:26])))
  b=lapply(blanks,nchar)
  cc <- t(mapply(function(x,y)  cbind(t(as.matrix(x)),y), tiledist, b))
  colnames(cc)<-c(LETTERS[1:26],"?")
  return(cc)
}
