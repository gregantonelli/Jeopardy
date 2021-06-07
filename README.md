# Jeopardy

The goal of this project was answer a simple question - how likely is a winner on the TV show Jeopardy to win again? 
Thanks to the work of dedicated fans at https://j-archive.com/ I had one location from which to wrangle all of the 
necessary data. From here I was able to determine the percentage of winners who followed up with subsequent wins and run
a 2-prop test to determine if the likelihood of following a win with another win was significantly greater than chance (33%).

I found the likelihood of repeating once to be about 44% (CI: 42.1 - 45.9, p-value < 2.2e-16, n = 2627). 
The likelihood of repeating twice: about 50.7% (CI: 47.7 - 53.7, p-value < 2.2e-16, n = 1110).
The likelihood of repeating three times: about 57.7% (CI: 53.4 - 61.8, p-value < 2.2e-16, n = 554).
The likelihood of repeating four times: about 62.5% (CI: 56.9 - 67.8, p-value < 2.2e-16, n = 318).

It should be noted that the confidence interval grows for each repeat as the sample size of contestants 
at subsequent levels of repeating becomes smaller and smaller. In addition to this, the confidence 
intervals for subsequent repeats do overlap slightly.
