# Shiny-app-results
Super simple app plotting some behavioural results from my project

The ready app is published at: https://ralitsa-angel.shinyapps.io/Risk-learning-curves/


The app takes the large csv file with the data of all my participants (anonymised), and plots several results:
- the probability of each participant choosing a risky over safe stimulus across time 
- the average probability of all participants to choose risky or safe 
- the reaction times associated with the stimuli presented on the screen
- the different learning curves (performance through time) of participants with high or low anxiety levels 

The experiment is a two-armed bandit task, in which two (out of a total of 4 stimuli) are presented on the screen. The participant has the task to choose one stimulus after which a reward is delivered by this stimulus. Over the time, the participants learns about the distribution of rewards each stimulus gives. 

Two of the 4 stimuli have a high expected value (EV) of 60, the other two have an EV of 40. The average of all rewards is 50. 
From the two with high-EV, one is risky (SD of 20), the other is safe (SD of 7). The same is true for the two stimuli with EV of 40. 
The participant starts to make risky or safe choices, when two stimuli with the same mean, but different SDs are present. 
To read more details on the task: 

Moeller, M., Grohn, J., Manohar, S., & Bogacz, R. (2021). An association between prediction errors and risk-seeking: Theory and behavioral evidence. PLoS computational biology, 17(7), e1009213.
