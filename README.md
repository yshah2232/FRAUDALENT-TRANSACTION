# FRAUDALENT-TRANSACTION
# R - UNSUPERVISED, SUPERVISED, SEMI-SUPERVISED MACHINE LEARNING TECHNIQUES

```
Worldwide losses from card fraud rose to $21 billion in 2015, up from about $8 billion in 2010. By 2020, that number is expected to reach $31 billion
```

## Problem:
Fraud transaction are a headache for the banks, insurance people and the stores.

## Reason:
Every year out of 12 billion transactions nearly 10 million transactions are fraud.
Fraud in USA for year 2008 was estimated at 500 million$. 

## GOAL
Using various data mining techniques we can detect the fraud transaction. Which may be helpful in reducing fraudalent transactions.

## Dataset:
We have  around 401,000 records reported by some salesperson each dateset include varibles such as : 

- ID
- PROD
- QUAT
- VAL
- INSP

### PROBLEM FACED

**UNKOWN VALUES** 
We start by addressing the problem of unknown variable values. 
There are essentially three alternatives: 
1 remove the cases
2 Fill in the unknowns using some strategy,
3 Use tools that handle these types of values.  


### FEW TRANSACTONS
There are products with very few transactions. 
This is a problem because we need to use the information on these transactions to decide if any of them are unusual.   

### DATA MINING TECHNIQUES

Using data mining to provide guidance in the task of deciding which transaction reports should be considered for inspection as a result of strong suspicion of being fraudulent. 

1 Unsupervised technique

- Box plot rule
- LOF(local outlier factor)
- Cluster based ranking


2 Supervised technique

- Class imbalance problems
- Naïve Bayes
- Ada boost.


3 Semi supervised technique


## conclusion
We have seen that these three techniques(unsupervised, supervised and semi-supervised)  can tackle this problem from different perspectives with a limited amount of resources.

Several real-world applications map into this general framework

- So we can use this techniques for, such as detecting frauds in credit card transactions, telecommunications, tax declarations.

- Using these techniques we can easily identify the fraud transactions which will in return be helpful for us in saving the money.


## Criteria evaluation
- Precision and recall
- Lift charts vs precision/recall curves
- Normalized distance


