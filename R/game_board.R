#' Get final game board of a game
#'
#' @param x a dataframe produced by game() function
#' @return matrix of game board
#' @examples
#' game_board(game(game_file(28283)))
#' game_board(game(game_file(28283)), type="clean")
#' game_board(game(game_file(28283)), type="dots")
#' @export





game_board <- function(x, type="pretty"){

  v <- get_tiles(x)

  dfy<-data.frame(lett = v,
                  row = gsub("[^0-9]", "", names(v)),
                  col = gsub("[0-9]", "", names(v)),
                  pos=names(v),
                  stringsAsFactors = F
  )

  # add in empty
  eg<-expand.grid(row=1:15,col=LETTERS[1:15])
  dfy1<-rbind(dfy,data.frame(lett="",row=eg[,1],col=eg[,2],pos=paste0(eg[,1],eg[,2])))
  dfy2<-do.call(rbind, lapply(split(dfy1, dfy1$pos), head, 1))

  mat<-reshape2::acast(dfy2, row~col, value.var='lett', fill="")
  mat<-mat[order(as.numeric(rownames(mat))),]

  if(type=="pretty"){return(board_pretty(mat))}
  if(type=="clean"){return(board_clean(mat))}
  if(type=="dots"){return(board_dots(mat))}

}




get_pos <- function(df){

  n<-nchar(df$play)-1
  pos.num<-as.numeric(gsub("[[:alpha:]]", "", df$position))
  pos.let<-gsub("[0-9]", "", df$position)
  q <- strsplit(df$play, "")

  #hz <- which(!is.na(as.numeric(substr(df$position,1,1))))
  hz <- which(nchar(gsub("[^0-9]", "", substr(df$position,1,1)))==1)
  vz <- which(nchar(gsub("[^A-Z]", "", substr(df$position,1,1)))==1)

if(length(hz)>0){
  #horizontal plays
  for(i in 1:length(hz)){
    mn<-match(pos.let[hz[i]],LETTERS[1:15])
    poses <- paste0(pos.num[hz[i]],LETTERS[mn:(mn+n[hz[i]])])
    names(q[hz[i]][[1]])<-poses
  }
}

if(length(vz)>0){
    #vertical plays
  for(i in 1:length(vz)){
    poses <- paste0(pos.num[vz[i]]:(pos.num[vz[i]]+n[vz[i]]),pos.let[vz[i]])
    names(q[[vz[i]]])<-poses
  }
}

  #need to remove phonies
  ph<-which(df$type=="phony")

  if(length(ph)>0){
    for(i in 1:length(ph)){
      q[[ph[i]]]<-NULL
    }
  }

  return(q)
}



get_tiles <- function(df){
  v = unlist(get_pos(df))
  v = v[grepl("[A-Za-z]",v)]
  return(v)
}

get_words<- function(df){
  q<-get_pos(df)
  v<-get_tiles(df)
  out=NULL
  for(i in 1:length(q)){
    ot<-v[match(names(q[i][[1]]),names(v))]
    out[[i]]<-paste(ot,collapse="")
  }
  return(out)
}


board_clean <- function(m){
  gsub("\\.", "", gsub('"',"",gsub("=|'|-|","",m)))
}

board_dots <- function(m){
  m1=board_pretty(m)
  m2=board_clean(m1)
  m3=gsub(" ", ".", m2)
  noquote(ifelse(m3=="", ".", m3))
}
board_pretty <- function(m){
  colnames(m)<-LETTERS[1:15]
  rownames(m)<-1:15
  m<-noquote(m)
  return(m)
}


