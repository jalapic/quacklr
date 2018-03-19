#' Get game summary in a dataframe
#'
#' @param x a quackle or cross-tables game number or link, or file path
#' @return data.frame game summary
#' @examples
#' game(game_file(28283))
#' game(game_file("http://www.cross-tables.com/annotated/selfgcg/281/anno28152.gcg"))
#' game(game_file("http://www.cross-tables.com/annotated.php?u=28190#0#"))
#' @export


game <- function(x){

  x0 <- lapply(x, function(f) grepl("#", substr(f,1,1)))
  x1  <- x[x0==F] #remove notes-or any element beginning with "#"
  #x1<-x[3:length(x)] #remove first two lines as are players names.


  x2 <- strsplit(x1, " ")
  #remove any remaining elements of list where its first element does not contain >
  x2tf <- lapply(x2, function(x) grepl(">", x[1]))
  x2 <- x2[which(x2tf==T)]

  x3  <- lapply(x2, function(f) if(length(f)==6) f else add_el(f)  )
  x4 <- do.call('rbind', x3) #join together
  x4[,4][is.na(x4[,4])]<-"" #replace NA with ""

  df <- as.data.frame(x4,stringsAsFactors = F)
  df$extra<-as.numeric(c(grepl("challenge", df[,3])[-1],F))*5
  df[,6] <- as.numeric(df[,6])+df$extra#make scores numeric and add challenge score
  df <- df[!grepl("challenge", df[,3]),]#remove rows with challenge
  df[,5]<-as.numeric(df[,5])

  df$extra <- df$extra + as.numeric(c(grepl("--", df[,3])[-1],F))*df[,5]*-1
  df[,6] <- ifelse(df$extra<0, df[,6]-df[,5], df[,6]) #take points off for phony
  df[,5] <- ifelse(df$extra<0, 0, df[,5]) #make score for phony turn 0
  df <- df[!grepl("--", df[,3]),]#remove rows with phony

  df$type <-
    ifelse(df[,3]=="-", "pass",
           ifelse(grepl("-",df[,3]), "exchange",
                  ifelse(df$extra<0, "phony",
                         ifelse(df$extra>0, "challenged",
                                ifelse(grepl("\\(", df[,3]), "add_tiles","")))))

  df$tiles<-ifelse(df$type=="phony",0,nchar(gsub("\\.","", df[,4]))) #number of tiles played

  colnames(df)[1:6] <- c("player","rack","position","play","score","total")# add tiles used

  df$player <- gsub(":","", gsub(">","", df$player))
  p1name <- sub("^\\S+\\s+", '', (sub("^\\S+\\s+", '', x[grepl("#player1",x)])))
  p2name <- sub("^\\S+\\s+", '', (sub("^\\S+\\s+", '', x[grepl("#player2",x)])))
  p1namex <- gsub(paste0(" ", p1name), "", sub("^\\S+\\s+", '', x[grepl("#player1",x)]))
  p2namex <- gsub(paste0(" ", p2name), "", sub("^\\S+\\s+", '', x[grepl("#player2",x)]))

  namesdf<-data.frame(player=c(p1namex,p2namex),name=c(p1name,p2name),stringsAsFactors = F)
  df$name <- namesdf$name[match(df$player, namesdf$player)]

  #add exchange column
  df$exchange <- ifelse(df$type=="exchange", gsub("-","",df$position), "")
  df$position <- ifelse(df$type=="exchange", "", df$position)
  df$position <- gsub("-","", df$position)
  df$leftover <- ifelse(grepl("\\(", df$position), gsub(".*\\((.*)\\).*", "\\1", df$position), "")
  df$position <- ifelse(grepl("\\(", df$position), "", df$position)

  ## add turn number...
  df$turn <- as.numeric(ave(df$player, df$player, FUN = seq_along))
  df<-df[c(1,13,2:7,9,8,11,12)]
  return(df)
}

add_el <- function(v){
  v2 <- numeric(length(v)+1)
  v2[4] <- NA
  v2[!is.na(v2)] <- v
  return(v2)
}

