![](Max_lift.jpeg)

## Power lifting

In this [#TidyTuesday](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08) I plotted the max weight lifted throughout the years, and how do those max achievements distribute across participants.


#### *New things I learned:*

* `library(ggthemr)` - I played around with the different theme templats in the package. it's a very nice go to and worth a try.

* `library(gridExtra)` - a very useful package to integrate differnet plots together. this enabled me to display both the max achievments throuout the years, and next to it a more in depth level of how those achievements distribute across participants. it was very easy to use and saves the propportions of the graph to nicely fit together.

* `dplyr::gather` - I never used this function and wanted to learn and give it a try. it helped gather the different activies to then split the distribution by.

* `legend.justification` `direction` and `spacing.x` - i learned these cool operators to play with the legend. it enabled me to place the legend in the top left corner, remove the background with the `legend.key`. really useful!
