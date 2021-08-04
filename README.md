# Car Accidents Model using R
Using fatal car accident data from the VT State Police open data, I created a generalized linear model
to predict if someone was wearing a seat belt or not when a fatal car crash occurred.

In the selected model, the statitically significant terms are slick conditions, weekends, and the interaction 
between age and weekeds. When conditions are slick, drivers and passengers who passed away in deadly accidents were more likely to be wearing
a seatbelt. On weekends, people who pass away in deadly car accidents are less likely to be wearing seatbelts.

When the conditions are slick, the odds someone in Vermont who died in a car crash was wearing a seatbelt increases e^1.06 = 2.89 times, 
holding everything else constant. During the weekend, a one year increase in age of someone who died in a car crash increases the probability 
that the person was wearing a seatbelt by (1 - e(-2.98 + .03) ) = 94.8%
