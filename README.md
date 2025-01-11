# Monte_Carlo_Methods_Project

## **Overview**
This project explores various Monte Carlo simulation techniques and their applications in statistical estimation and probability. It includes practical implementations of different algorithms such as inverse CDF sampling, acceptance-rejection methods, stratification, importance sampling, and control variates. The focus is on efficiently estimating probabilities, densities, and quantiles, along with their corresponding confidence intervals.

This work was completed as a group project with contributions from the following team members:
- Kevin Wardakhan 
- Matteo Casati 

## **Objectives**
- Implement Monte Carlo methods to simulate random variables and estimate probabilities.
- Compare the efficiency and precision of naive estimators, importance sampling, and control variates.
- Visualize results to verify the correctness and performance of implemented methods.
- Develop insights into the choice of sampling distributions and control variables to improve efficiency.

## **Key Features**
1. **Inverse CDF Sampling**
   - Uses interpolation to approximate the inverse of the CDF for generating random samples.
   - Verifies accuracy by comparing histograms of samples with theoretical densities.

2. **Acceptance-Rejection Sampling**
   - Implements the accept-reject method using a proposal density.
   - Computes the empirical and theoretical acceptance rates as functions of key parameters.

3. **Stratified Sampling**
   - Divides the sampling space into strata to reduce variance.
   - Simulates samples using stratified accept-reject methods.

4. **Importance Sampling (IS)**
   - Chooses an efficient proposal density to minimize variance.
   - Estimates rare event probabilities with a focus on sharp variations in target densities.

5. **Control Variates (CV)**
   - Utilizes correlated control variables to reduce variance.
   - Derives optimal control coefficients for improved estimations.

## **Skills Acquired**
- **Stochastic Simulation**: Understanding of Monte Carlo methods and their applications in statistical analysis.
- **Data Analysis**: Use of R to process, analyze, and visualize simulation results.
- **Variance Reduction Techniques**: Application of control variates, stratification, and importance sampling to improve simulation efficiency.
- **Problem-Solving**: Designing algorithms to tackle rare event probability estimation and other challenges.

## **Results**
- Empirical and theoretical acceptance rates for acceptance-rejection methods.
- Comparison of estimators (naive, IS, CV) in terms of variance, computational cost, and precision.
- Analysis of rare event probabilities using IS and CV methods.
- Visualizations of histograms, CDFs, and quantiles for different scenarios.

## **Technologies Used**
- **Programming Language**: R
- **Libraries**: ggplot2, gridExtra (for visualization)

## **Acknowledgments**
This project was completed as part of a coursework assignment on Monte Carlo Methods. The repository also includes details about the subject. 


#Copyright 2025 [Erwan Ouabdesselam ;  Kevin Wardakhan ;  Matteo Casatti ]

All rights reserved.

This code is provided for reference purposes only. No part of this code may be reproduced, distributed, or transmitted in any form or by any means without the prior written permission of the author.
