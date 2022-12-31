***********************************
* Name: max.do
* Author: Yu Xia
* Description: try to solve a single variable optimize problem in stata
* Last updated: Sep 30, 2022
***********************************

clear all
mata:
 void myeval(todo, x,  y, g, H)
{
y = (2/3)*(1-exp(-0.01*(0.35*x+1.02)))+(1/3)*(1-exp(-0.01*(-0.01*x+1.02)))
}
S = optimize_init()
optimize_init_evaluator(S, &myeval())
optimize_init_params(S, 0)
x = optimize(S)
x
