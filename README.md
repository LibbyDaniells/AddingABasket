# AddingABasket

This repository is the code used in the paper: 'Adding Baskets to an Ongoing Basket Trial with Information Borrowing: When do you Benefit?' authored by Libby Daniells, Pavel Mozgunov, Alun Bedding and Thomas Jaki. 

The files are as follows:
- EXNEX.txt and Ind.txt - the model files for the exchagneability-nonexchangeability (EXNEX) model and an independent analysis to be used in JAGS in R.
- Calibrating Under the Null - calibration and simulation for fixed scenarios under each of the 4 methods in which a traditional calibration approach under the global null scenario is implemented.
- Calibrating Across Scenarios - simulation studies where calibration of posterior probability cut-off values is conducted to control a type I error on average across several data scenarios.
    - Fixed Scenarios - calibration and simulation for fixed data scenarios under each of the 4 methods.
    - Varied Truths - calibration and simulation for a case in which response rates in existing baskets are fixed, with the resopnse on the new basket randomly generated across an interval. Code is also provided for the production of the heat-map presented in the paper.
    - Robustness To Sample Size - for each method code is provided to consider the impact of sample size in the new basket with existing baskets fixed. This is considered across fixed data scenarios.
    - 2+2 Baskets - a fixed scenario simulation study consisting of 2 existing baskets with 2 new baskets added part-way through the study.
  
