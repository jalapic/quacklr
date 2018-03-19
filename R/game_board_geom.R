#' Get final game board of a game in ggplot2 style
#'
#' @param x a dataframe produced by game() function
#' @param fsize font size for text
#' @param asize font size for axis labels
#' @param gsize grid size
#' @param facetype font type
#' @param alp transparency of tile colors
#' @return ggplot2 plot of game board
#' @examples
#' game_board_geom(game(game_file(28283)))
#' @export


game_board_geom <- function(x, fsize=5, asize=1, gsize=.8 ,facetype="bold", alp=.4, turn=NULL){

  if(is.null(turn)){x <- x}
  if(!is.null(turn)){x <- x[1:turn,]}

  v <- get_tiles(x)

  dfy<-data.frame(lett = v,
                  row = gsub("[^0-9]", "", names(v)),
                  col = gsub("[0-9]", "", names(v)),
                  pos=names(v),
                  stringsAsFactors = F
  )

  dfy$row<-as.numeric(dfy$row)

  out <- ggboard(asizex=asize, alpx=alp, gsizex=gsize) +
          geom_text(data=dfy, size=fsize,
              fontface = facetype,
              aes(x=factor(col,levels=LETTERS[15:1]),y=row,label=lett
              ))

  return(out)
}

  get_tiles <- function(df){
    v = unlist(get_pos(df))
    v = v[grepl("[A-Za-z]",v)]
    return(v)
  }


ggboard <- function(asizex=NULL, alpx=NULL, gsizex=NULL){

    #### Basic scrabble ggplot board
  eg<-as.data.frame.matrix(expand.grid(row=1:15,col=LETTERS[1:15]))
  eg$pos <- paste0(eg$row,eg$col)
  tws<-c("1A","1H","1O","8A","8O","15A","15H","15O")
  dws<-c("2B","3C","4D","5E","2N","3M","4L","5K",
         "14B","13C","12D","11E","14N","13M","12L","11K","8H")
  dls<-c("1D","1L","8D","8L","15D","15L",
         "4A","12A","4H","12H","4O","12O",
         "3G","3I","7G","7I","9G","9I","13G","13I",
         "7C","9C","7M","9M")
  tls<-c("6B","10B","2F","6F","10F","14F",
         "2J","6J","10J","14J","6N","10N")

  eg$color<-NA
  eg$color[match(tws,eg$pos)] <- "red"
  eg$color[match(dws,eg$pos)] <- "pink"
  eg$color[match(dls,eg$pos)] <- "dodgerblue"
  eg$color[match(tls,eg$pos)] <- "purple"


  library(ggplot2)
  board<-ggplot() +
    geom_tile(data=eg,
              aes(x=col, y = row),
              fill=eg$color,
              na.rm = T,
              color="gray20",
              alpha=alpx,
              size=gsizex, stat="identity")+
    xlab("")+ylab("")+
    scale_y_reverse(breaks=1:15)+
    scale_x_discrete(position = "top")+
    theme(line = element_blank(),
          rect = element_blank(),
          axis.text = element_text(size = rel(asizex)),
          axis.title = element_blank(),
          legend.text = element_text(size = rel(0.8)),
          legend.title = element_text(hjust = 0),
          plot.margin = unit(c(0, 0, 0, 0), "lines")
          )

  return(board)
  }



