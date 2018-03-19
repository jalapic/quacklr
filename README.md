# quacklr

Extract game information from quackle game files for scrabble visualization and analysis.

Longer vignette readable [here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/jalapic/quacklr/master/misc/intro_vignette.html).  


### Install

`devtools::install_github("jalapic/quacklr")`


### Quick Example

A game between Phil Robertshaw and Austin Shin.

```
x <- game_file(27222)
  
xx <- game(x)
  
game_board(xx)

```
  

  


![](https://github.com/jalapic/quacklr/blob/master/img/game.png)

    
     
         
         
_________________________


You can create prettier boards too:

![](https://github.com/jalapic/quacklr/blob/master/img/board1.png)

     
         
         

  
__________________________



You can also create gifs!

A game between Nigel Richards and David Eldar.
  

![](https://github.com/jalapic/quacklr/blob/master/img/test.gif)
