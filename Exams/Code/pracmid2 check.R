# ------------------------------------------------------------------------------
# name: pracmid2 check.R
# author: Yu Xia
# description: try to check the root for the arbitrage free pricing formula
# last updated: Nov 2, 2022
# ------------------------------------------------------------------------------

#Question 1
uniroot(function(x) (0.6*(x-1.14)+0.4*(x-1.10))/1.005, lower = 1.10, upper = 1.14, tol = 1e-9)
uniroot(function(x) (0.6*(x-1.10)+0.4*(x-1.06))/1.005, c(1.10,1.06), tol = 1e-9)
uniroot(function(x) (0.6*(x-1.124)+0.4*(x-1.084))/1.005, c(1.124,1.084), tol = 1e-9)
uniroot(function(x) (0.6*(x-1.12)+0.4*(x-1.08))/1.005+0.6/(1.005^2)*(0.6*(x-1.14)+0.4*(x-1.10))+0.4/(1.005^2)*(0.6*(x-1.10)+0.4*(x-1.06)), c(1.14,1.06), tol = 1e-9)
#q_{p_{1}}(q_{1u})
(0.4*(1110-1100))/1.005
200*(0.4*(1110-1100))/1.005
#q_{p_{1}}(q_{1d})
(0.6*(1110-1100)+0.4*(1110-1060))/1.005
200*(0.6*(1110-1100)+0.4*(1110-1060))/1.005
#q_{p_{0}}(q_{0})
(0.6*(0.4*(1110-1100))/1.005+0.4*((0.6*(1110-1100)+0.4*(1110-1060))/1.005))/1.005
(0.6*4/1.005+0.4*26/1.005)/1.005
200*(0.6*(0.4*(1110-1100))/1.005+0.4*((0.6*(1110-1100)+0.4*(1110-1060))/1.005))/1.005

#Question 2
pi<-uniroot(function(p) (p*120+(1-p)*90)/1.1-98.18, c(0,1), tol = 1e-9)
uniroot(function(K) K-98.18-((1-pi$root)/1.1)*(K-90), c(90,120), tol = 1e-9)
#workflow
fp<-function(p, qh, ql, q0, r) (p*qh+(1-p)*ql)/(1+r)-q0
uniroot(fp, c(0,1),tol = 1e-9, qh=120, ql=90, q0=98.18, r=0.1)
uniroot.all(fp, c(0,1),tol = 1e-9, qh=120, ql=90, q0=98.18, r=0.1)
pi<-uniroot.all(fp, c(0,1),tol = 1e-9, qh=120, ql=90, q0=98.18, r=0.1)
fK<-function(K, q0, p, r, ql) K-q0-((1-p)/(1+r))*(K-ql)
uniroot(fK, c(80,120), tol=1e-9, q0=98.18, ql=90, p=pi, r=0.1)
uniroot.all(fK, c(80,120), tol=1e-9, q0=98.18, ql=90, p=pi, r=0.1)

#Attempt to use as a workflow
library(utils)
f <- function(x, a1, a2, p, r) (p*(x-a1)+(1-p)*(x-a2))/(1+r)
str(uniroot(f, c(1.06,1.14),tol = 1e-9, a1=1.14, a2=1.10, p=0.6, r=0.005))
str(uniroot.all(f, c(1.06,1.14),tol = 1e-9, a1=1.14, a2=1.10, p=0.6, r=0.005))

f1<-function(x) (0.6*(x-1.12)+0.4*(x-1.08))/1.005+0.6/(1.005^2)*(0.6*(x-1.14)+0.4*(x-1.10))+0.4/(1.005^2)*(0.6*(x-1.10)+0.4*(x-1.06))
str(uniroot(f1, c(1.06,1.14),tol = 1e-9))                                                                                 

library(rootSolve) #for multi roots
#Question 1
uniroot.all(function(x) (0.6*(x-1.14)+0.4*(x-1.10))/1.005, lower = 1.10, upper = 1.14, tol = 1e-9)
uniroot.all(function(x) (0.6*(x-1.10)+0.4*(x-1.06))/1.005, c(1.10,1.06), tol = 1e-9)
uniroot.all(function(x) (0.6*(x-1.124)+0.4*(x-1.084))/1.005, c(1.124,1.084), tol = 1e-9)
uniroot.all(function(x) (0.6*(x-1.12)+0.4*(x-1.08))/1.005+0.6/(1.005^2)*(0.6*(x-1.14)+0.4*(x-1.10))+0.4/(1.005^2)*(0.6*(x-1.10)+0.4*(x-1.06)), c(1.14,1.06), tol = 1e-9)
#Question 2
pi<-uniroot.all(function(p) (p*120+(1-p)*90)/1.1-98.18, c(0,1), tol = 1e-9)
pi
uniroot.all(function(K) K-98.18-((1-pi)/1.1)*(K-90), c(90,120), tol = 1e-9)
