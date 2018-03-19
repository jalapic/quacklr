#' Get summary statistics of a game in a dataframe
#'
#' @param x a dataframe produced by game() function
#' @return data.frame game summary statistics
#' @examples
#' game_summary(game(game_file(28283)))
#' @export



game_summary<-function(df){

  phonies <- table(factor(df[df$type=="phony",]$player,levels=unique(df$player)))
  challenged <- table(factor(df[df$type=="challenged",]$player,levels=unique(df$player)))
  pass <- table(factor(df[df$type=="pass",]$player,levels=unique(df$player)))
  exchange <- table(factor(df[df$type=="exchange",]$player,levels=unique(df$player)))
  df1  <- df[!df$type=="phony",]
  df1.sp <- split(df1$play, df1$player)
  discon.tiles <-  lapply(df1.sp, function(f) nchar(gsub("[A-Za-z]", "", paste(gsub("^\\.*", "", gsub("\\.*$", "", f)),collapse=""))))
  df1.ex <- split(df1$exchange, df1$player)
  tiles.exchanged<-lapply(df1.ex, function(x) nchar(paste(x, collapse = '')))
  df1.sp1 <- lapply(df1.sp, function(x) paste(x, collapse = ''))
  connect.tiles <- lapply(df1.sp1, function(f) nchar(gsub("[A-Za-z]", "", f)))
  join.tiles <- mapply(function(x,y) x-y, connect.tiles, discon.tiles) #total tiles joined to - number of dots minus disconncted
  df1.sp2 <- lapply(df1.sp1,function(f) gsub("[[:punct:]]", "", f))
  tiles.played <- lapply(df1.sp2, nchar)
  blanks <- lapply(df1.sp2, function(f) gsub("[^a-z]","",f))
  tiledist <- lapply(df1.sp2, function(f) table(factor(unlist(strsplit(gsub("[a-z]","",f),"")),levels=LETTERS[1:26])))
  powertiles <- mapply(function(x,y) sum(x[c("J","Q","X","Z","S")])+nchar(y),x=tiledist,y=blanks)
  finalscore <- aggregate(df$total, by = list(df$player), max)
  df.0<-df[df$type!="add_tiles",]
  turns.taken <- aggregate(df.0$turn, by = list(df.0$player), max)
  df1.sp0 <- split(df1$position, df1$player)
  horizontals <- lapply(df1.sp0, function(f) nchar(paste(gsub("[A-Z]", "", substr(f,1,1)),collapse="")))
  verticals <- lapply(df1.sp0, function(f) nchar(paste(gsub("[0-9]", "", substr(f,1,1)),collapse="")))

  df.sum <- as.data.frame(
    rbind(
      do.call('cbind',discon.tiles),
      join.tiles,
      do.call('cbind', blanks),
      do.call('cbind', lapply(blanks,nchar)),
      phonies,
      challenged,
      pass,
      exchange,
      do.call('cbind',tiles.played),
      do.call('cbind',tiles.exchanged),
      do.call('cbind',horizontals),
      do.call('cbind',verticals),
      t(finalscore)[2,],
      t(turns.taken)[2,],
      powertiles,
      c("first", "second")
    ),row.names = F,stringsAsFactors = F
  )

  df.sum[17,]<-round(as.numeric(df.sum[13,])/as.numeric(df.sum[14,]),2)

  df.sum$summary <- c("disconnected", "joined", "blanks", "total_blanks",
                      "phonies", "challenges_won", "passes", "exchanges",
                      "tiles_played", "tiles_exchanged", "horizontal_plays",
                      "vertical_plays", "final_score", "turns_taken",
                      "power_tiles",  "order","points_turn")


  df.sum <- df.sum[c(13,14,17,16,15,4,3,9,8,10,5,6,7,11,12,1,2),c(3,1,2)]
  rownames(df.sum) <- NULL
  return(df.sum)
}
