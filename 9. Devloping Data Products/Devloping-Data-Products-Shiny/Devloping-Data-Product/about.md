  


Welcome to the “Predictor” app, that was developed for the Developing Data Products Course, from Coursera.  
This app helps predict and get confidence interval of your choice for Income depending on different variables by  
fitting a linear model on state.x77 dataset from [R].  

First you need to select a variable you wish to predict Income against. A graph will be plot using your selected  
predictor against Income. Then you can select the part of plot which you wish to fit the model on. After that a linear  
model will be fitted and a line will be drawn representing that fit.  

Also a 95% confidence interval will be drawn. The confidence interval threshold can be changed by using the slider.  
A new confidence interval will be drawn every time a new confidence interval is choosen.  

Slope and intersept of the model are shown in the side panel.

Note : All the values are in logrithmic transformation.

Source code for ui.R and server.R files are available on the [GitHub](https://github.com/im-utkarsh/Devloping-Data-Products-Shiny/tree/master/Devloping-Data-Product).