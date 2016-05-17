# Bmeta
A Bayesian meta-analysis of parafoveal preview effects during reading

For anny questions or issues, please contact Martin Vasilev (mvasilev@bournemouth.ac.uk).

This file contains the most important information in order to repeat the analyses. Due to the vast amount of data and analyses (as well was my lack of experience when I started this), the scripts are spread over multiple functions and files. Here, I explain how the analysis was done.

If you are interested in the **literature search and study selection**: please see Documents/all_papers.xls. The file is heavily commented and is generally self-contained. 

## To repeat the analyses:

**Before you start**: Install JAGS: http://mcmc-jags.sourceforge.net/; then, in Rstudio, install the package 'rjags': install.packages("rjags")


* The most basic scripts are __N1.R__ (for N+1 studies) and __N2.R__ (for N+2 studies). This is where all the information extracted from the papers is coded. The scripts are a bit clumsy and take long to execute- I have provided them for completeness. However, the data which the two scripts give are already available in the folder "Data".
* __check_ES.R__ does multiple things, but generally subsets the data and prepares it for the analyses (it also does the SD imputation). If you want to repeat the funnel plots, set impute to FALSE in the beginning. Please note that it is technically not needed to run the scripts as the data is also already available.

* __pub_bias.R__ generates code, graphs etc. related to publication bias (however see the above comment about imputation).

* __get_bench.R__ does the baseline (or benchmark) anaylyses of fixation durations when parafoveal preview is allowed (i.e. valid preview).

* __N1_Bmeta_final.R__ and __N2_Bmeta_final.R__ contain the core analyses for N+1 and N+2 preview effects. Both files are generally commented. Please note that to generate the graphs, the scripts call additional functions. Due to multiple issues that I had with plotting software such as ggplot2, I generate only parts of some graphs (e.g. the N+2 forest plot). I did the annotation etc in external software (Microsoft Publisher); files are availble upon request. 

* Additional functions/ scripts:
 + __N1_probs.R__ generates N+1 probability plot
 + __CohensD__ custom-made function for checking the coded statistics for mistakes and double-checking of whether the correct values were coded.
 + __JModel.R__ generates JAGS models (as txt files) by taking some input values. This function was created to minimise the code that was repeated over and over again.
 + __sensitivity_analysis.R__ performs a sensitivity analysis using the leave-one-out method. Please note: depending on your machine, N+1 sensitivity analyses can take A LOT of time (up to 2 hrs).

