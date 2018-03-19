#' Get all words played in a game
#'
#' @param x a dataframe produced by game() function
#' @return a vector of words played by each player
#' @examples
#' game_words(game(game_file(28283)))
#' @export



game_words<- function(x){
  q<-get_pos(x)
  v<-get_tiles(x)
  out=NULL
  for(i in 1:length(q)){
    ot<-v[match(names(q[i][[1]]),names(v))]
    out[[i]]<-paste(ot,collapse="")
  }
  out <- out[nchar(out)>0]
  out<-sort(out)
  return(out)
}
