# Bank
Telemarketing (yes/no) - loan acquisition bank campaign

 This is a bank loan case with a binary (yes / no) answer option to accept or reject the loan 
      in a tele marketing campaign. The detail description of the campaign and its data source is 
      as follows:
      
 Relevant Information:

  The data is related with direct marketing campaigns of a Portuguese banking institution.<br>
  The marketing campaigns were based on phone calls. Often, more than one contact to the <br>
  same client was required, in order to access if the product (bank term deposit) would be (or not)
  subscribed.
  
  key Question
  
   The positive answers rate are 12% in the real scenario; ¿ Which variable combination will reduce <br>
   the negative answers (reduce the negative predictive value)? and which is the importance and direction (positive or negative )
   influence on the decision 
   
   THE XGBOOST ALGORITHM WILL BE USED TO ANSWER THIS QUESTION 
   
   DATA SOURCE
   
   This dataset is public available for research. The details are described in [Moro et al., 2011].
      Please include this citation if you plan to use this database: 

     [Moro et al., 2011] S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing:
     An Application of the CRISP-DM Methodology.<br> 
     In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - 
     ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>

  Available at: [pdf] http://hdl.handle.net/1822/14838 
                [bib] http://www3.dsi.uminho.pt/pcortez/bib/2011-esm-1.txt 

1. Title: Bank Marketing

2. Sources

   Created by: Paulo Cortez (Univ. Minho) and Sérgio Moro (ISCTE-IUL) @ 2012<br>
    
3. Past Usage:<br>

  The full dataset was described and analyzed in:<br> 

  S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of 
  the CRISP-DM Methodology.<br> 
  In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011,
  pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.<br><br>

   There are two datasets:
   
      1) bank-full.csv with all examples, ordered by date (from May 2008 to November 2010).
      2) DataSample.csv with 10% of the examples (4521), randomly selected from bank-full.csv.
      
      THE ORIGINAL DATA SET HAS 45,211 OBSERVATIONS AND 17 VARIABLES; 10% WILL BE UPLOADED TO <
      UNDERSTAND THE DATA STRUCTURE. File name: DataSample.csv , if the original data set is required
      write us : info@gssg.com.co
      
      NOTE: Two different lines of code will be available: a) XGBoost_2.R is the R code to run with the original
      data set: bank-full.csv ; it takes more time running! b) XGBoost_bank.R is the shiny code to runwith the DataSample.csv data set.
      To check the shiny site go to:  
      
   https://inisghtdiscovery.shinyapps.io/BankLoan/
      
