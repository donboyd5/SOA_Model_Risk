---
title: "A Simple Pension Model"
author: "Don Boyd and Gang Chen"
date: "04 04 2024"
output:
  pdf_document: default
  html_document: null
header-includes: \usepackage{amsmath}
---

# A Simple Pension Model

To demonstrate how ESG models and parameters affect the output of pension simulation models, we construct a simple defined benefit pension model with no demographic component and simple funding rules. For the simple pension model, we assume benefit payments ($p$) to be a stable percent ($cp$) of payroll ($W$). 

## Assets

Let $r_t$ denote the rate of returns in year $t$ and $ct$ denote the contributions made in year $t$, asset $A$ in year $t+1$ is shown in the following formula:

\begin{equation}
  \tag{1}
  A_{t+1} = A_t(1 + r_t) + c_t - cp*W_t
\end{equation}

Formula (1) shows the assets of the current year are determined by the assets of the last year plus the investment returns during the year, plus the contributions, and minus the benefit payments.

## Liabilities

On the liabilities side, let $cn$ denote the normal cost rate and $d$ denote the discount rate, the liability $L$ in year $t+1$ is shown in the following formula:

\begin{equation}
  \tag{2}
L_{t+1} = L_t(1 + d) + cn*W_t - cp*W_t
\end{equation}

This shows the liabilities are determined by the liabilities of last year growing by the discount rate (as the present value of benefits is rolled forward), plus normal costs (assuming constant normal cost rate), and minus benefit payments.

In our model, we assume that payroll W increases annually by the assumed inflation rate. Normal cost rate $cn$ and payment rate $cp$ are constant. Assets and Liabilities at year 0 ($A_0$ and $L_0$) are determined. 

As for $rt$, we use stochastic process to generate investment returns in each year using several simulated ESG models with different parameters.

As for $ct$, we use a fully funding approach with level-dollar 30-year closed amortization of unfunded liabilities $A – L$. Contributions are determined by the equation below:

\begin{equation}
  \tag{3}
c_t=cn * W_t+(L_j-A_j)/(ä_{30})
\end{equation}


In Equation (3), contribution in year $t$ is the sum of the normal cost and the amortization cost. Let $ä_{30}$ be a 30-year annuity factor to calculate the amortization payment to pay off the unfunded liability $(L-A)$ within 30 years. $ä_{30}= (1-(1+i)^{(-30)}/i$  where $i$ is the interest rate for amortization. 

The assumptions for a typical public plan are listed below, based on the median values in US state pension plans.

	* Liabilities: $100
	* Assets: $78 (funded ratio of 78%)
	* Starting payroll: $19 (19% of actuarial liabilities)
	* Payroll growth rate: 4%
	* Benefit payout: $5.13 (27% of payroll)
	* Benefit growth rate: 4%
	* Amortization: 15 year closed level dollar
	* Discount rate: US Intermediate term Govt bond rate
	* Normal cost rate: $2.28 (12% of payroll)

To simulate a private defined benefit plan, we have made some reasonable adjustments based on aggregate corporate DB plan data in the US. The assumptions are listed below:
	
	* Liabilities: $100
	* Assets: $100 (funded ratio of 100%)
	* Starting payroll: $19 (19% of actuarial liabilities)
	* Payroll growth rate: 4%
	* Benefit payout: $5.13 (27% of payroll)
	* Benefit growth rate: 2%
	* Amortization: No amortization
	* Discount rate: 10-year US treasury rate
	* Normal cost rate: $2.28 (12% of payroll)
