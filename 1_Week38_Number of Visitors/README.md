![National Parks](https://user-images.githubusercontent.com/55328033/65224542-0e52c600-dacc-11e9-8f60-5a12421f2728.png)

# Average number of visitors to national parks

In this Tidytuesday i attempted at displaying the distribution for number of average visitors at national parks. Since it was my first attempt at plotting in R i tried to keep it simple. However, once i got into the graphing i found it too addicting and decided to create a nice outdoor scenery. Frankly, the idea for the background came after realizing the distribution looks like a mountain and from there I just played with the background theme.
The implementation of icons and some background is inspired by Ariane Aumaitre's [#Tidytuesday](https://github.com/aaumaitre/tidytuesday/tree/master/Amusement%20Parks).

#### Challenges i encountered:
- Finding the appropriate graph - i used geom_step but later encountered geom_path which i think is a better fit (I'll keep at as is to have something nice to look back at :)). I started by using geom_col but it wasn't neatly displayed so stuck with geom_step. to overcome the challenge of the step (it's pretty sharply cut) i filled the inside and made it transparent.
- I also wanted to show the Museums distribution inside that of the natioanl parks. however i was having trouble with *gather* function to aggregate it under number of visitors and factored by museum/parks. It might not even be the adequate function, anyway I'll try grouping next time!


#### Things that helped me:
- Icons are from www.pixabay.com and are free to use. the important note is to take **png** files that can be implemented easily (where the background is transparent).
- This is my first project i synched with GitHub. It wasn't easy but not too complicated either, figured it out with [HappygitwithR](https://happygitwithr.com/).
- I created this readme through a cool Template you can find [here](https://dillinger.io/). Hopefully next time i'll be able to do so through R Markdown :) 

*Let me know if you have any tips, suggestions and comments - would love to know!*
