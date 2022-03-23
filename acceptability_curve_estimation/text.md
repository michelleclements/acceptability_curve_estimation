
<style type="text/css">
           body {          
           max-width:100%;
           padding:0;
           }
</style>
 
  
  
  
**To get started, click on the 'ACE' tab above and input your values in the sidebar**
  
   

## About

This app enables acceptability curve estimation (ACE) for up to three trials at once.  


For each trial you will need:  

1. the point estimate (mean) of the difference between treatments.
2. An estimate of variation around the point estimate - either the standard error or the lower 95% confidence interval. 

Up to three acceptability thresholds can also be specified. Different trials must have outputs on the same scale for comparisons to be valid. Acceptability values can also be seen by hovering over the lines in the graph.    

Note that confidence intervals should be symmetrical around the point estimate and not results transformed from the original output scale. 

Point estimates and variance mesures can be from frequentist or Bayesian analysis.If you would like to do Bayesian re-analysis of a frequentist trial, posterior estimates incorporating priors to enter into ACE can be obtained from: https://benjamin-andrew.shinyapps.io/bayesian_trials/  


## Replicating results from ACE paper  

Use the values below to replicate the results from 'Improving clinical trial interpretation with Acceptability Curve Estimation (ACE)' (pdf available from https://arxiv.org/ftp/arxiv/papers/2203/2203.11164.pdf).

Note that these values are from the Frequentist analysis in the original papers and so differ very slightly from the reanalysis, which used Bayesian methods. Results don't differ appreciably when degrees of freedom are inputted so I haven't included below. 



EARNEST:  
* Mean: 4.1%
* Lower 95% CI: -2.4%  


SECOND-LINE:  
* Mean: 1.8%
* Lower 95% CI: -4.7%  

Results can be entered either by ignoring the percent symbol or first dividing by 100 to get proportions.  
