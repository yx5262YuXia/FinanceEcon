# ------------------------------------------------------------------------------
# name: PS7 graph.R
# author: Yu Xia
# description: try to plot graph
# last updated: Oct 20, 2022
# ------------------------------------------------------------------------------

library(ggplot2)
library(ggthemes)

#Payoff of European call
ggplot() +
  coord_cartesian(xlim=c(150,200),ylim=c(-12, 8))+
  geom_segment(aes(x = 165, y = 0, xend = 200 , yend = -35))+
  geom_segment(aes(x = 0, y = 0, xend = 165 , yend = 0))+
  labs(x = expression(italic("r"[1])), y=expression(italic("r"[c])))+
  theme_minimal(base_family = "serif") +
  scale_fill_economist()

ggsave("D:/Xia Yu/夏宇的文档/读书/textbook/Econ/Financial/Problem Sets/figures/PS7/PS7a1.png", width=8, height=4.5)

#Profit of European call
ggplot() +
  coord_cartesian(xlim=c(150,200),ylim=c(-12, 8))+
  geom_segment(aes(x = 165, y = 7, xend = 200 , yend = -28))+
  geom_segment(aes(x = 0, y = 7, xend = 165 , yend = 7))+
  labs(x = expression(italic("r"[1])), y="")+
  theme_minimal(base_family = "serif") +
  scale_fill_economist()

ggsave("D:/Xia Yu/夏宇的文档/读书/textbook/Econ/Financial/Problem Sets/figures/PS7/PS7a2.png", width=8, height=4.5)

#Payoff of European put
ggplot() +
  coord_cartesian(xlim=c(150,200),ylim=c(-12, 8))+
  geom_segment(aes(x = 0, y =-165 , xend = 165, yend = 0))+
  geom_segment(aes(x = 165, y = 0, xend = 250, yend = 0))+
  labs(x = expression(italic("r"[1])), y=expression(italic("r"[p])))+
  theme_minimal(base_family = "serif") +
  scale_fill_economist()

ggsave("D:/Xia Yu/夏宇的文档/读书/textbook/Econ/Financial/Problem Sets/figures/PS7/PS7a3.png", width=8, height=4.5)

#Profit of European put
ggplot() +
  coord_cartesian(xlim=c(150,200),ylim=c(-12, 8))+
  geom_segment(aes(x = 0, y =-162.5 , xend = 165, yend = 2.5))+
  geom_segment(aes(x = 165, y = 2.5, xend = 250, yend = 2.5))+
  labs(x = expression(italic("r"[1])), y="")+
  theme_minimal(base_family = "serif") +
  scale_fill_economist()

ggsave("D:/Xia Yu/夏宇的文档/读书/textbook/Econ/Financial/Problem Sets/figures/PS7/PS7a4.png", width=8, height=4.5)
