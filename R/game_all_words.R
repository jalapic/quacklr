#' Get all words on board in a game
#'
#' @param x a dataframe produced by game() function
#' @return a vector of words on the board in a game
#' @examples
#' game_words(game(game_file(28283)))
#' @export



game_all_words <- function(df){
  out<-board_words(game_board(df, "clean"))
  out<-sort(out)
  return(out)
}


board_words <- function(m){

  words_in_mat <- function(z){
    gtx <-  strsplit(paste(z, collapse='' ), "\\.")
    gtx1 <- gtx[[1]][which(unlist(lapply(gtx[[1]], nchar))>1)]
    return(gtx1)
  }

  m <- board_dots(m)
  allwords <- c(unlist(apply(m,1,words_in_mat)),unlist(apply(m,2,words_in_mat)))
  allwords <-  as.vector(toupper(allwords))
  return(allwords)
}

