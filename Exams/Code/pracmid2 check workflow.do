****************************************************************************************************************************************************************************************************************************
* name: pracmid2 check workflow.do
* author: Yu Xia
* description: try to check the root for the arbitrage free pricing formula
* last updated: Nov 2, 2022
****************************************************************************************************************************************************************************************************************************
*Question 1
mata
mata clear
function myfunc(x, a1, a2, p, r) return((p*(x-a1)+(1-p)*(x-a2))/(1+r))
a1=1.14
a2=1.10
p=0.6
r=0.005
mm_root(x=., &myfunc(), 1.06, 1.14, 0, 1000000, a1, a2, p, r)
x
mata clear
*swap
function myfunc(x, a1, a2, a3, a4, a5, a6, p, r) return((p*(x-a1)+(1-p)*(x-a2))/(1+r)+(p/((1+r)^2))*(p*(x-a3)+(1-p)*(x-a4))+((1-p)/((1+r)^2))*(p*(x-a5)+(1-p)*(x-a6)))
a1=1.12
a2=1.08
a3=1.14
a4=1.10
a5=1.10
a6=1.06
p=0.6
r=0.005
mm_root(x=., &myfunc(), 1.06, 1.14, 0, 1000000, a1, a2, a3, a4, a5, a6, p, r)
x
*Q1 (c)
*q_{p_{1}}(q_{1u})
mata clear
function myfunc(K, a1, p, r) return(((1-p)*(K-a1*1000))/(1+r))
K=1110
a1=1.10
p=0.6
r=0.005
((1-p)*(K-a1*1000))/(1+r)
200*((1-p)*(K-a1*1000))/(1+r)
mata clear
*q_{p_{1}}(q_{1d})
function myfunc(K, a1, a2, p, r) return((p*(K-a1*1000)+(1-p)*(K-a2*1000))/(1+r))
K=1110
a1=1.10
a2=1.06
p=0.6
r=0.005
(p*(K-a1*1000)+(1-p)*(K-a2*1000))/(1+r)
200*(p*(K-a1*1000)+(1-p)*(K-a2*1000))/(1+r)
mata clear
*#q_{p_{0}}(q_{0})
mata clear
function myfunc(K, a1, a2, a3, p, r) return((p*(((1-p)*(K-a1*1000))/(1+r))+(1-p)*((p*(K-a2*1000)+(1-p)*(K-a3*1000))/(1+r)))/(1+r))
K=1110
a1=1.10
a2=1.10
a3=1.06
p=0.6
r=0.005
(p*(((1-p)*(K-a1*1000))/(1+r))+(1-p)*((p*(K-a2*1000)+(1-p)*(K-a3*1000))/(1+r)))/(1+r)
200*(p*(((1-p)*(K-a1*1000))/(1+r))+(1-p)*((p*(K-a2*1000)+(1-p)*(K-a3*1000))/(1+r)))/(1+r)
*Question 2
*(a)
mata clear
function myfunc(p, q0, q1, q2, r) return(-q0+(p*q1+(1-p)*q2)/(1+r))
q0=100
q1=120
q2=80
r=0.04
mm_root(p=., &myfunc(), 0, 1, 0, 1000000, q0, q1, q2, r)
p
mata clear
*(c)
mata clear
function myfunc(K, q0, q1, p, r) return(K-q0-((1-p)/(1+r))*(K-q1))
q0=98.18
r=0.1
q1=90
p=(98.18*1.1-90)/30
mm_root(K=., &myfunc(), 90, 120, 0, 1000000, q0, q1, p, r)
K
p
end
