# ------------------------------------------------------------------------------
# name: mid2 check.R
# author: Yu Xia
# description: try to check the root for the arbitrage free pricing formula
# last updated: Nov 4, 2022
# ------------------------------------------------------------------------------

library(rootSolve) #for multi roots
library(utils)

#Question 1
#(a)
f1 <- function(x, a1, a2, p, r) (p*(x-a1)+(1-p)*(x-a2))/(1+r)
uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=0.94, a2=0.90, p=0.5, r=0.005)
phi1u<-uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=0.94, a2=0.90, p=0.5, r=0.005)
uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=0.90, a2=0.86, p=0.5, r=0.005)
phi2u<-uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=0.90, a2=0.86, p=0.5, r=0.005)
uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=0.92, a2=0.88, p=0.5, r=0.005)
f1 <- function(x, a1, a2, p, r) (p*(x-a1)+(1-p)*(x-a2))/(1+r)
uniroot(f1, c(0.86,0.94),tol = 1e-9, a1=phi1u$root, a2=phi2u$root, p=0.5, r=0.005)
uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=0.94, a2=0.90, p=0.5, r=0.005)
phi1u<-uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=0.94, a2=0.90, p=0.5, r=0.005)
uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=0.90, a2=0.86, p=0.5, r=0.005)
phi2u<-uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=0.90, a2=0.86, p=0.5, r=0.005)
uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=0.92, a2=0.88, p=0.5, r=0.005)
uniroot.all(f1, c(0.86,0.94),tol = 1e-9, a1=phi1u, a2=phi2u, p=0.5, r=0.005)

#(b)
f0<-function(x, a1, a2, a3, a4, a5, a6, p, r) (p*(x-a1)+(1-p)*(x-a2))/1.005+p/(1.005^2)*(p*(x-a3)+(1-p)*(x-a4))+(1-p)/(1.005^2)*(p*(x-a5)+(1-p)*(x-a6))
uniroot(f0, c(0.86,0.94),tol = 1e-9, a1=0.92, a2=0.88, a3=0.94, a4=0.90, a5=0.90, a6=0.86, p=0.5, r=0.005)                                                                                
uniroot.all(f0, c(0.86,0.94),tol = 1e-9, a1=0.92, a2=0.88, a3=0.94, a4=0.90, a5=0.90, a6=0.86, p=0.5, r=0.005)                                                                              

#(c)
#Workflow for function:
qp1<-function(K, a1, a2, p, r) (p*max(K-a1*1000, 0)+(1-p)*max(K-a2*1000, 0))/(1+r)
qp1(910, 0.94, 0.90, 0.5, 0.005)
qp1(910, 0.90, 0.86, 0.5, 0.005)
qp0<-function(qp1u, qp1d, p, r) (p*qp1u+(1-p)*qp1d)/(1+r)
qp0(qp1(910, 0.94, 0.90, 0.5, 0.005), qp1(910, 0.90, 0.86, 0.5, 0.005), 0.5, 0.005)

#Question 2
#(a)
#workflow
fp<-function(p, qh, ql, q0, r) (p*qh+(1-p)*ql)/(1+r)-q0
uniroot(fp, c(0,1),tol = 1e-9, qh=120, ql=80, q0=100, r=0.04)
uniroot.all(fp, c(0,1),tol = 1e-9, qh=120, ql=80, q0=100, r=0.04)
pi2<-uniroot.all(fp, c(0,1),tol = 1e-9, qh=120, ql=80, q0=100, r=0.04)
fK<-function(K, q0, p, r, ql) K-q0-((1-p)/(1+r))*(K-ql) #jump to (d)
uniroot(fK, c(80,120), tol=1e-9, q0=100, ql=80, p=pi2, r=0.04)
uniroot.all(fK, c(80,120), tol=1e-9, q0=100, ql=80, p=pi2, r=0.04)

#(b)
#Given Equations:
#120a + 1.04b = 30
#80a + 1.04b = 0
A<-rbind(c(120, 1.04),
         c(80, 1.04))
B<-c(30, 0)
solve(A, B)
sum(solve(A, B)*c(100, 1)) #100a+b

#Other attempt
#Question 1
uniroot(function(x) (0.5*(x-0.94)+0.5*(x-0.90))/1.005, lower = 0.90, upper = 0.94, tol = 1e-9)
uniroot(function(x) (0.5*(x-0.90)+0.5*(x-0.86))/1.005, c(0.86,0.90), tol = 1e-9)
uniroot(function(x) (0.5*(x-0.92)+0.5*(x-0.88))/1.005, c(0.92,0.88), tol = 1e-9)
uniroot(function(x) (0.5*(x-0.92)+0.5*(x-0.88))/1.005+0.5/(1.005^2)*(0.5*(x-0.94)+0.5*(x-0.90))+0.5/(1.005^2)*(0.5*(x-0.90)+0.5*(x-0.86)), c(0.86,0.94), tol = 1e-9)

uniroot.all(function(x) (0.5*(x-0.94)+0.5*(x-0.90))/1.005, lower = 0.90, upper = 0.94, tol = 1e-9)
uniroot.all(function(x) (0.5*(x-0.90)+0.5*(x-0.86))/1.005, c(0.86,0.90), tol = 1e-9)
uniroot.all(function(x) (0.5*(x-0.92)+0.5*(x-0.88))/1.005, c(0.88,0.92), tol = 1e-9)
uniroot.all(function(x) (0.5*(x-0.92)+0.5*(x-0.88))/1.005+0.5/(1.005^2)*(0.5*(x-0.94)+0.5*(x-0.90))+0.5/(1.005^2)*(0.5*(x-0.90)+0.5*(x-0.86)), c(0.86,0.94), tol = 1e-9)

#q_{p_{1}}(q_{1u})
(0.5*(910-900))/1.005
#q_{p_{1}}(q_{1d})
(0.5*(910-900)+0.5*(910-860))/1.005
#q_{p_{0}}(q_{0})
(0.5*(0.5*(910-900))/1.005+0.5*((0.5*(910-900)+0.5*(910-860))/1.005))/1.005
(0.5*5/1.005+0.5*30/1.005)/1.005
#Workflow for function:
qp1u<-function(K, a1, a2, p, r) (p*max(K-a1*1000, 0)+(1-p)*max(K-a2*1000, 0))/(1+r)
K<-910
a1<-0.94
a2<-0.90
p<-0.5
r<-0.005
(p*max(K-a1*1000, 0)+(1-p)*max(K-a2*1000, 0))/(1+r)

#Question 2
pi<-uniroot.all(function(p) (p*120+(1-p)*80)/1.04-100, c(0,1), tol = 1e-9)
pi
uniroot.all(function(K) K-100-((1-pi)/1.04)*(K-80), c(80,120), tol = 1e-9)

pi1<-uniroot(function(p) (p*120+(1-p)*80)/1.04-100, c(0,1), tol = 1e-9)
pi1$root
uniroot(function(K) K-100-((1-pi1$root)/1.04)*(K-80), c(90,120), tol = 1e-9)

A<-rbind(c(120, 1.04),
         c(80, 1.04))
B<-c(30, 0)
solve(A, B)
a<-solve(A, B)
sum(a*c(100, 1))

