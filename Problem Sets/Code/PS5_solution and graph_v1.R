# ------------------------------------------------------------------------------
# name: PS5_solution and graph_v1.R
# author: Yu Xia
# description: v_0: try to solve a 2-variable optimize problem in R, and plot graph
#              v_1: check
# last updated: Oct 02, 2022
# ------------------------------------------------------------------------------

#Problem 1
f1 <- function(x,y){sqrt(x)+sqrt(y)}
x<-seq(0,200,length=1000)
y<-seq(0,200,length=1000)
z1<-outer(x,y,f1)

library(emdbook)
curve3d(sqrt(x)+sqrt(y),
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,levels=18,family="serif",axes=FALSE)
abline(v = 121,lty=2)
abline(h=49,lty=2)
p1a<-c(121,49)
points(121,49,pch=19)

par(family = "serif")

ps1a<-diag(2)*p1a
points(ps1a, pch=c("|", "-"), cex=1:2)
text(ps1a, labels=paste(diag(ps1a)), pos=c(1, 4))

abline(v=0)
abline(h=0)
text(-6, -6, expression(0))
#1(b)
curve3d(sqrt(x)+sqrt(y),
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,levels=sqrt((49*1.03+121*(1.03)^2)/2.03)+sqrt(49/(1.03*2.03)+121/2.03),
        family="serif",axes=FALSE,
        col="green",add=TRUE)
curve3d(1.03*x+y,
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(1000,1000),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=49+121*1.03,
        family="serif",axes=FALSE,add=TRUE,col="blue")
abline(v=49/(1.03*2.03)+121/2.03,lty=2)
abline(h=(49*1.03+121*(1.03)^2)/2.03,lty=2)
points(49/(1.03*2.03)+121/2.03,(49*1.03+121*(1.03)^2)/2.039,pch=19)
p1b<-c(49/(1.03*2.03)+121/2.03,(49*1.03+121*(1.03)^2)/2.039)
ps1b<-diag(2)*p1b
points(ps1b, pch=c("|", "-"), cex=1:2)
text(ps1b, labels=c("87.71","83.04"), pos=c(1, 4))
legend(165, 195, 
       legend=c(expression(italic("V")*"("*italic("e"[0])*","*italic("e"[1])*")"), expression(italic("V")*"("*italic("x"[0])*","*italic("x"[1])*")"),expression("Budget Constraint")),
       col=c("black", "green","blue"), lty=c(1,1,1), text.font=2,
       cex=0.8,
       box.lty=0,
       bg="transparent")
#Check the max
library('nloptr')
eval_f1 <- function(x,r)
{
  return (-(sqrt(x[1])+sqrt(x[2])))
}
eval_g_eq1 <- function (x,r) {
  constr1 <-  c(x[1]+x[3]-121,
                x[2]-(1+r)*x[3]-49)
  return(constr1)
}
#If denote b=x[3]
lb1 <- c(0, 0,-450) #The lower bound actually implies xi>=0
ub1 <- c(400,400,450)
x01 <- c(121, 49,0)
r<-0.03
opts1 <- list( "algorithm"
                = "NLOPT_GN_ISRES",
                "xtol_rel"
                = 1.0e-15,
                "maxeval"= 500000)
res1 <- nloptr ( x0 = x01,
                eval_f = eval_f1,
                lb = lb1,
                ub = ub1,
                eval_g_eq = eval_g_eq1,
                opts = opts1,
                r = r
)
print(res1)

#Problem 2
f2 <- function(x,y){(1/4)*(sqrt(x))+(3/4)*(sqrt(y))}
x<-seq(0,200,length=1000)
y<-seq(0,200,length=1000)
z2<-outer(x,y,f2)

curve3d((1/4)*(sqrt(x))+(3/4)*(sqrt(y)),
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,levels=8,family="serif",axes=FALSE)
abline(v = 121,lty=2)
abline(h=49,lty=2)
points(121,49,pch=19)

p2a<-c(121,49)

ps2a<-diag(2)*p2a

points(ps2a, pch=c("|", "-"), cex=1:2)
text(ps2a, labels=paste(diag(ps2a)), pos=c(1, 4))

abline(v=0)
abline(h=0)
text(-6, -6, expression(0))
#By the result of 2(b)
curve3d((1/4)*(sqrt(x))+(3/4)*(sqrt(y)),
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/4)*sqrt((67+18*0.03)/(3+2*0.03))*(sqrt((3-0.03)/1.03)+9*sqrt(1.03/(3-0.03))), #if r=0.03
        family="serif",axes=FALSE,
        col="green",add=TRUE)
curve3d(1.03*x+(3-0.03)*y, #if r=0.03
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(1000,1000),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=268+72*0.03,
        family="serif",axes=FALSE,add=TRUE,col="blue")
abline(v=((67+18*0.03)/1.03)*(3-0.03)/(3+2*0.03),lty=2)
abline(h=(9*1.03*(67+18*0.03))/((3-0.03)*(3+2*0.03)),lty=2)
points(((67+18*0.03)/1.03)*(3-0.03)/(3+2*0.03),
       (9*1.03*(67+18*0.03))/((3-0.03)*(3+2*0.03)),pch=19)
p2b<-c(((67+18*0.03)/1.03)*(3-0.03)/(3+2*0.03),
       (9*1.03*(67+18*0.03))/((3-0.03)*(3+2*0.03)))
ps2b<-diag(2)*p2b
points(ps2b, pch=c("|", "-"), cex=1:2)
text(ps2b, labels=c("63.64","68.89"), pos=c(1, 4))
legend(165, 195, 
       legend=c(expression(italic("V")*"("*italic("e"[0])*","*italic("e"[1])*")"), expression(italic("V")*"("*italic("x"[0])*","*italic("x"[1])*")"),expression("Budget Constraint")),
       col=c("black", "green","blue"), lty=c(1,1,1), text.font=2,
       cex=0.8,
       box.lty=0,
       bg="transparent")

#Check corner solution
fc1 <- function (r) (1/6)*((3-r)/(sqrt((3+2*r)*(1+r)))+9*sqrt((1+r)/(3+2*r)))
rc1min <- optimise(fc1, c(0,0.5), tol = 1.0e-15, maximum = FALSE)
rc1min
#---------------------------------------------------
#$minimum (where r min the objective function)
#[1] 1
#$objective (where the objective function minimized)
#[1] 1.054093
#---------------------------------------------------
fc2 <- function (r) (1/2)*(sqrt((3-r)/(3+2*r))+9*(1+r)/sqrt((3+2*r)*(3-r)))
rc2min <- optimise(fc2, c(0,1), tol = 1.0e-15, maximum = FALSE)
rc2min

#Check interior solution, if r=0.03:
((67+18*0.03)/1.03)*(3-0.03)/(3+2*0.03) #x[1]
(9*1.03*(67+18*0.03))/((3-0.03)*(3+2*0.03)) #x[2]
(9*(67+18*0.03))/((3-0.03)*(3+2*0.03))-49/1.03 #x[3]=b
196/1.03-(36*(67+18*0.03))/((3-0.03)*(3+2*0.03)) #x[4]=k
(1/4)*sqrt((67+18*0.03)/(3+2*0.03))*(sqrt((3-0.03)/1.03)+9*sqrt(1.03/(3-0.03))) #maxV

#min of b
fcb <- function (r) (9*(67+18*r))/((3-r)*(3+2*r))-49/(1+r)
bmin <- optimise(fcb, c(0,1), tol = 1.0e-15, maximum = FALSE)
bmin

#max of k
fck <- function (r) 196/(1+r)-(36*(67+18*r))/((3-r)*(3+2*r))
kmax <- optimise(fcb, c(0,1), tol = 1.0e-15, maximum = TRUE)
kmax


eval_f2 <- function(x,r)
{
  return (-((1/4)*sqrt(x[1])+(3/4)*sqrt(x[2])))
}
eval_g_eq2 <- function (x,r) {
  constr2 <-  c(x[3]+(1/4)*x[4],
                x[1]-(1+r)*x[3]-x[4]-121,
                x[2]-(1+r)*x[3]-49)
  return(constr2)
}
lb2 <- c(0, 0,-450,-450) #The lower bound actually implies xi>=0
ub2 <- c(400,400,450,450)
x02 <- c(121,49,0,0)
r<-0.03
opts2 <- list( "algorithm"
               = "NLOPT_GN_ISRES",
               "xtol_rel"
               = 1.0e-15,
               "maxeval"= 1000000)
res2 <- nloptr ( x0 = x02,
                 eval_f = eval_f2,
                 lb = lb2,
                 ub = ub2,
                 eval_g_eq = eval_g_eq2,
                 opts = opts2,
                 r = r
)
print(res2)

#Plot 2 indiff at the same time:
abline(v=0)
abline(h=0)
text(-6, -6, expression(0))
#Earlier attempt
curve3d((1/4)*(sqrt(x))+(3/4)*(sqrt(y)),
        xlim=c(0,200),
        ylim=c(0,200),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,levels=c(8,10),family="serif")
abline(v = 121,lty=2)
abline(h=49,lty=2)

#3(d)
library("SciViews")

(1/2)*(ln(11/6))+(1/2)*(ln(11/8))

f3 <- function(x,y){(1/2)*(ln(x))+(1/2)*(ln(y))} #I've generate the seq of x & y
z3<-outer(x,y,f3)

contour(x,y,z3) #have an idea about how large xlim & ylim should be

curve3d((1/2)*(ln(x))+(1/2)*(ln(y)),
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(1))+(1/2)*(ln(2)),
        family="serif",axes=FALSE,col="blue",lty=2)
curve3d((1/2)*(ln(x))+(1/2)*(ln(y)),
              xlim=c(0,4),
              ylim=c(0,3),
              n=c(100,100),
              xlab = expression(italic("x"[1])),
              ylab = expression(italic("x"[2])),
              sys3d="contour",drawlabels=FALSE,
              levels=(1/2)*(ln(11/6))+(1/2)*(ln(11/8)),
              family="serif",axes=FALSE,add=TRUE,col="blue")
abline(v=0)
abline(h=0)
text(-.07, -.07, expression(A)) #coordinate of text "0" depends
abline(v=1,lty=2)
abline(h=2,lty=2)
abline(v=11/6,lty=2)
abline(h=11/8,lty=2)
p3A0<-c(1,2)
ps3A0<-diag(2)*p3A0
#points(ps3A0, pch=c("|", "-"), cex=1:2)
text(ps3A0, labels=paste(diag(ps3A0)), pos=c(1, 4))
p3A1<-c(11/6,11/8)
ps3A1<-diag(2)*p3A1
#points(ps3A1, pch=c("|", "-"), cex=1:2)
text(ps3A1, labels=c("11/6", "11/8"), pos=c(1, 4))
#Budget Constraint
curve3d(x+(4/3)*y,
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(1000,1000),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=11/3,
        family="serif",axes=FALSE,add=TRUE,col="black")
curve3d((1/2)*(ln((11/3)-x))+(1/2)*(ln((11/4)-y)),
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(11/6))+(1/2)*(ln(11/8)),
        family="serif",axes=FALSE,add=TRUE,col="red")
curve3d((1/2)*(ln((11/3)-x))+(1/2)*(ln((11/4)-y)),
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln((11/3)-1))+(1/2)*(ln((11/4)-2)),
        family="serif",axes=FALSE,add=TRUE,col="red",lty=2)
abline(v=4)
abline(h=3)
text(4+.07, 3+.07, expression(B))
text(11/6, 3+.07, expression("13/6"))
text(4+.07, 11/8, expression("13/8"))
text(1, 3+.07, expression(3))
text(4+.07, 2, expression(1))
par(family = "serif")
legend(3, 2.85, 
       legend=c(expression(italic("V"^"A")*"("*italic("e"[1])*","*italic("e"[2])*")"), expression(italic("V"^"A")*"("*italic("x"[1])*","*italic("x"[2])*")"),expression(italic("V"^"B")*"("*italic("x"[1])*","*italic("x"[2])*")"),expression(italic("V"^"B")*"("*italic("e"[1])*","*italic("e"[2])*")"),expression("Budget Constraint")),
       col=c("blue", "blue","red", "red", "black"), lty=c(2,1,1,2,1), text.font=2,
       cex=0.6,
       box.lty=0,
       bg="transparent")

#Check
curve3d((1/2)*(ln(4-x))+(1/2)*(ln(3-y)),
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(4-11/6))+(1/2)*(ln(3-11/8)),
        family="serif",axes=FALSE,add=TRUE,col="red")
curve3d((1/2)*(ln(4-x))+(1/2)*(ln(3-y)),
        xlim=c(0,4),
        ylim=c(0,3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(4-1))+(1/2)*(ln(3-2)),
        family="serif",axes=FALSE,add=TRUE,col="red",lty=2)

#4(d)
f4A <- function(x,y){(1/3)*(ln(x))+(2/3)*(ln(y))} #I've generate the seq of x & y
z4A<-outer(x,y,f4A)

contour(x,y,z4A) #have an idea about how large xlim & ylim should be

(1/3)*(ln(2))+(2/3)*(ln(8/3))

curve3d((1/3)*(ln(x))+(2/3)*(ln(y)),
        xlim=c(0,4),
        ylim=c(0,4),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/3)*(ln(3))+(2/3)*(ln(2)),
        family="serif",axes=FALSE,col="blue",lty=2)
curve3d((1/3)*(ln(x))+(2/3)*(ln(y)),
        xlim=c(0,4),
        ylim=c(0,4),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/3)*(ln(2))+(2/3)*(ln(8/3)),
        family="serif",axes=FALSE,add=TRUE,col="blue")
abline(v=0)
abline(h=0)
text(-.07, -.07, expression(A)) #coordinate of text "0" depends
abline(v=3,lty=2)
abline(h=2,lty=2)
abline(v=2,lty=2)
abline(h=8/3,lty=2)
p4A0<-c(3,2)
ps4A0<-diag(2)*p4A0
#points(ps3A0, pch=c("|", "-"), cex=1:2)
text(ps4A0, labels=paste(diag(ps4A0)), pos=c(1, 4))
p4A1<-c(2,8/3)
ps4A1<-diag(2)*p4A1
#points(ps3A1, pch=c("|", "-"), cex=1:2)
text(ps4A1, labels=c("2", "8/3"), pos=c(1, 4))
2+(3/2)*(8/3)
#Budget Constraint
curve3d(x+(3/2)*y,
        xlim=c(0,4),
        ylim=c(0,4),
        n=c(1000,1000),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=6,
        family="serif",axes=FALSE,add=TRUE,col="black")
curve3d((1/2)*(ln(4-x))+(1/2)*(ln(4-y)),
        xlim=c(0,4),
        ylim=c(0,4),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(4-8/3))+(1/2)*(ln(4-2)),
        family="serif",axes=FALSE,add=TRUE,col="red")
curve3d((1/2)*(ln(4-x))+(1/2)*(ln(4-y)),
        xlim=c(0,4),
        ylim=c(0,4),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(4-3))+(1/2)*(ln(4-2)),
        family="serif",axes=FALSE,add=TRUE,col="red",lty=2)
abline(v=4)
abline(h=4)
text(4+.07, 4+.07, expression(B))
text(3, 4+.07, expression("1"))
text(4+.07, 8/3, expression("4/3"))
text(2, 4+.07, expression(2))
text(4+.07, 2, expression(2))
legend(3, 3.85, 
       legend=c(expression(italic("V"^"A")*"("*italic("e"[1])*","*italic("e"[2])*")"), expression(italic("V"^"A")*"("*italic("x"[1])*","*italic("x"[2])*")"),expression(italic("V"^"B")*"("*italic("x"[1])*","*italic("x"[2])*")"),expression(italic("V"^"B")*"("*italic("e"[1])*","*italic("e"[2])*")"),expression("Budget Constraint")),
       col=c("blue", "blue","red", "red", "black"), lty=c(2,1,1,2,1), text.font=2,
       cex=0.6,
       box.lty=0,
       bg="transparent")

#Check
curve3d((1/2)*(ln(x))+(1/2)*(ln(y)),
        xlim=c(0,4),
        ylim=c(0,16/3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(2))+(1/2)*(ln(4/3)),
        family="serif",axes=FALSE,col="red")
curve3d((1/2)*(ln(x))+(1/2)*(ln(y)),
        xlim=c(0,4),
        ylim=c(0,16/3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=(1/2)*(ln(1))+(1/2)*(ln(2)),
        family="serif",axes=FALSE,add=TRUE,col="red",lty=2)
curve3d(x+(3/2)*y,
        xlim=c(0,4),
        ylim=c(0,16/3),
        n=c(100,100),
        xlab = expression(italic("x"[1])),
        ylab = expression(italic("x"[2])),
        sys3d="contour",drawlabels=FALSE,
        levels=4,
        family="serif",axes=FALSE,add=TRUE,col="black")

# Example problem, number 71 from the Hock-Schittkowsky test suite.
#
# \min_{x} x1*x4*(x1 + x2 + x3) + x3
# s.t.
# x1*x2*x3*x4 >= 25
# x1^2 + x2^2 + x3^2 + x4^2 = 40
# 1 <= x1,x2,x3,x4 <= 5
#
# we re-write the inequality as
# 25 - x1*x2*x3*x4 <= 0
#
# and the equality as
# x1^2 + x2^2 + x3^2 + x4^2 - 40 = 0
#
# x0 = (1,5,5,1)
#
# optimal solution = (1.00000000, 4.74299963, 3.82114998, 1.37940829)

library('nloptr')
eval_f <- function(x,b,k)
{
  return (-((1/4)*(sqrt(x[1]))+(3/4)*(sqrt(x[2]))))
}
eval_g_eq <- function (x,r,b,k) {
  constr <- c(b+k/4,
              x[1] - r*b-k-121,
              x[2]-r*b-49)
  return (constr)
}
lb <- c(0, 0)
ub <- c(Inf,Inf)
x0 <- c(3, 1)
r<-
b<-
k<-
opts <- list( "algorithm"
              = "NLOPT_GN_ISRES",
              "xtol_rel"
              = 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 5 ))
res <- nloptr(
  x0          = x0,
  eval_f      = eval_f,
  lb          = lb,
  ub          = ub,
  eval_g_ineq = eval_g_ineq,
  eval_g_eq = eval_g_eq,
  opts        = opts,
  r           = r,
  b           = b,
  k           = k)
print(res)

#Examples
# Objective Function
eval_f <- function(x)
{
  return (x[1]*x[4]*(x[1] +x[2] + x[3] ) + x[3] )
}
# Inequality constraints
eval_g_ineq <- function(x)
{
  return (25 - x[1]*x[2]*x[3]*x[4])
}
# Equality constraints
eval_g_eq <- function(x)
{
  return ( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
}
# Lower and upper bounds
lb <- c(1,1,1,1)
ub <- c(5,5,5,5)
#initial values
x0 <- c(1,5,5,1)
# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 0 )
res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts
)
print(res)

#Another example, with many constraints
# Objective function
eval_f <- function(x)
{
  return ( x[1]^2 + x[2]^2 )
}
# Inequality constraints
eval_g_ineq <- function (x) {
  constr <- c(1 - x[1] - x[2],
              1 - x[1]^2 - x[2]^2,
              9 - 9*x[1]^2 - x[2]^2,
              x[2] - x[1]^2,
              x[1] - x[2]^2)
  return (constr)
}# Lower and upper bounds
lb <- c(-50, -50)
ub <- c(50, 50)
# Initial values
x0 <- c(3, 1)
opts <- list( "algorithm"
              = "NLOPT_GN_ISRES",
              "xtol_rel"
              = 1.0e-15,
              "maxeval"= 160000,
              "tol_constraints_ineq" = rep( 1.0e-10, 5 ))
res <- nloptr(
  x0          = x0,
  eval_f      = eval_f,
  lb          = lb,
  ub          = ub,
  eval_g_ineq = eval_g_ineq,
  opts        = opts )
print(res)

#Other attempt
contour(x,y,z,level=8)
abline(v = 121,lty=2)
abline(h=49,lty=2)

filled.contour(x,y,z,level=8)

geom_segment(aes(x =121, y = 0, xend =121, yend = 49), linetype="dashed")

to.plot <- imp.solve(x,y,f1)
ggplot(to.plot,aes(x=x,y=y))+geom_point()+xlim(range(x))+ylim(range(y))


library(ggplot2)
library(ggthemes)

ggplot() +
  xlim(-99,99)+
  ylim(-99,99)+
  geom_function(fun = function(x, y) (1/4)*(sqrt(x))+(3/4)*(sqrt(y)))+
  theme_minimal() +
  scale_fill_economist()

