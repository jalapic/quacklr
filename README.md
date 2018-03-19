# quacklr

Extract game information from quackle game files for scrabble visualization and analysis.

### Install

`devtools::install_github("jalapic/quacklr")`


### Quick Example

` x <- game_file(27222)
  xx <- game(x)
  game_board(xx)
`
`
    A B C D E F G H I J K L M N O
    1      S E N D   Q              
    2    O M         U              
    3    F A W       A   P E R C    
    4        A B l A T I O N       C
    5  W H I L E     E   L         R
    6  O   N E E Z E     J   V     O
    7  O           S L E E K I T   R
    8  D     F O E T I D     T A M E
    9  Y       X           R I G    
    10         L         V   A      
    11       H I         A I T U    
    12     Y U P         G   o      
    13     O B               R      
    14     D     I G N E O U S      
    15       A N T I A R S     
`



You can also create gifs!
![](https://github.com/jalapic/quacklr/blob/master/img/test.gif)
