#############################################
# Paper: Spreading the word!
# Author: Jose David Lopez Rivas
# Plots add p-values (Randomized inference)
#############################################
library(ggplot2)
setwd("~/Documents/GitHub/spreading_the_word/simulation")
load("simulation 20000.RData")

# RI p-values: this is based on the idea that the specific units in our sample that are 
# treated are random. Thus there is some chance of a treatment-control 
# difference in outcomes of any given magnitude simply based on which units 
# are assigned to the treatment group – even if the treatment has no effect. 
# So we re-assign “treatment” at random, to compute the probability of 
# differences of various magnitudes under the null hypothesis that the 
# treatment does nothing.

# Test Statistics  ###############################################

# Compute the share of the fake #s that are higher in absolute value than our 
# point estimates. This is our randomization inference p-value.

# Aggregated model
beta.all.coeff <- as_tibble(beta.all) 
beta.all.coeff <- beta.all.coeff %>% 
  drop_na()

for(l in 1:2){
  all.test.stat[l,] <- sum((beta.obs.all[l]) >= (beta.all[,l]))/sims
}

# Sloped Model
for(l in 1:6){
  all.sat.test.stat[l,] <- sum((beta.obs.all.sat[l]) >= (beta.all.sat[,l]))/sims
}

# Pooled model by billing frequency
for(l in 1:4){
  bill.test.stat[l,] <- sum((beta.obs.bill[l]) >= (beta.bill[,l]))/sims
}

# Sloped model by billing frequency
for(l in 1:12){
  bill.sat.test.stat[l,] <- sum((beta.obs.bill.sat[l]) >= (beta.bill.sat[,l]))/sims
}

# Plot simulated coefficients ######################################
## Aggregate Model
library(wesanderson)

data.beta.all <- data.frame(direct_sim=beta.all[,1], spillover_sim=beta.all[,2],
                            direct_obs=beta.obs.all[1,1],spillover_obs=beta.obs.all[2,1] )

ggplot(data.beta.all, aes(x=direct_sim, fill=(direct_sim>direct_obs)))+
  geom_histogram(color = "white", bins = 100, alpha=0.8)+
  geom_vline(xintercept =  beta.obs.all[1,1], linetype = 1, colour="red")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(size=14,face="bold"),
        legend.position="bottom",
        legend.text = element_text(colour="black", size = 12))+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"), 
                    name = "Simulation <= Observed", labels = c("True", "False"))+
  ggtitle("Direct effect")+
  xlab("Estimates")+
  ylab("Count") +
  ggsave("direct_effect_all.pdf")

ggplot(data.beta.all, aes(x=spillover_sim, fill=(spillover_sim>=spillover_obs)))+
  geom_histogram(color = "white",bins = 100, alpha=0.8)+
  geom_vline(xintercept =  beta.obs.all[2,1], linetype = 1, colour="deeppink4")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 18, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(size=14,face="bold"),
        legend.position="bottom",
        legend.text = element_text(colour="black", size = 12))+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest1"), 
                    name = "Simulation <= Observed", labels = c("True", "False"))+
  ggtitle("Spillover effect")+
  xlab("Estimates")+
  ylab("Count") +
  ggsave("spill_effect_all.pdf")

## Aggregate Model With Saturation

data.beta.all.sat <- data.frame(direct25_sim=beta.all.sat[,1], direct50_sim=beta.all.sat[,2],
                                direct75_sim=beta.all.sat[,3], spillover25_sim=beta.all.sat[,4],
                                spillover50_sim=beta.all.sat[,5], spillover75_sim=beta.all.sat[,6],
                                direct25_obs=beta.obs.all.sat[1,1], direct50_obs=beta.obs.all.sat[2,1],
                                direct75_obs=beta.obs.all.sat[3,1], spillover25_obs=beta.obs.all.sat[4,1],
                                spillover50_obs=beta.obs.all.sat[5,1], spillover75_obs=beta.obs.all.sat[6,1])

ggplot(data.beta.all.sat, aes(x=direct25_sim, fill=(direct25_sim>=direct25_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[1,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_25.pdf")

ggplot(data.beta.all.sat, aes(x=spillover25_sim, fill=(spillover25_sim>=spillover25_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[4,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_25.pdf")

ggplot(data.beta.all.sat, aes(x=direct50_sim, fill=(direct50_sim>=direct50_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[2,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_50.pdf")

ggplot(data.beta.all.sat, aes(x=spillover50_sim, fill=(spillover50_sim>=spillover50_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[5,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_50.pdf")

ggplot(data.beta.all.sat, aes(x=direct75_sim, fill=(direct75_sim>=direct75_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[3,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_75.pdf")

ggplot(data.beta.all.sat, aes(x=spillover75_sim, fill=(spillover75_sim>=spillover75_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.all.sat[6,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_75.pdf")

## Bill Model 

data.beta.bill <- data.frame(direct_M_sim=beta.bill[,1], spillover_M_sim=beta.bill[,2],
                             direct_B_sim=beta.bill[,3], spillover_B_sim=beta.bill[,4],
                             direct_M_obs=beta.obs.bill[1,1], spillover_M_obs=beta.obs.bill[2,1],
                             direct_B_obs=beta.obs.bill[3,1], spillover_B_obs=beta.obs.bill[4,1])

ggplot(data.beta.bill, aes(x=direct_M_sim, fill=(direct_M_sim>=direct_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill[1,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_monthly.pdf")

ggplot(data.beta.bill, aes(x=spillover_M_sim, fill=(spillover_M_sim>=spillover_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill[2,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_monthly.pdf")

ggplot(data.beta.bill, aes(x=direct_B_sim, fill=(direct_B_sim>=direct_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill[3,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_bimonthly.pdf")

ggplot(data.beta.bill, aes(x=spillover_B_sim, fill=(spillover_B_sim>=spillover_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill[4,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_bimonthly.pdf")

## Bill Model With Saturation

data.beta.bill.sat <- data.frame(direct25_M_sim=beta.bill.sat[,1], direct50_M_sim=beta.bill.sat[,2],
                                 direct75_M_sim=beta.bill.sat[,3], spillover25_M_sim=beta.bill.sat[,4],
                                 spillover50_M_sim=beta.bill.sat[,5], spillover75_M_sim=beta.bill.sat[,6],
                                 direct25_B_siB=beta.bill.sat[,7], direct50_B_sim=beta.bill.sat[,8],
                                 direct75_B_sim=beta.bill.sat[,9], spillover25_B_sim=beta.bill.sat[,10],
                                 spillover50_B_sim=beta.bill.sat[,11], spillover75_B_sim=beta.bill.sat[,12],
                                 direct25_M_obs=beta.obs.bill.sat[1,1], direct50_M_obs=beta.obs.bill.sat[2,1],
                                 direct75_M_obs=beta.obs.bill.sat[3,1], spillover25_M_obs=beta.obs.bill.sat[4,1],
                                 spillover50_M_obs=beta.obs.bill.sat[5,1], spillover75_M_obs=beta.obs.bill.sat[6,1],
                                 direct25_B_obs=beta.obs.bill.sat[7,1], direct50_B_obs=beta.obs.bill.sat[8,1],
                                 direct75_B_obs=beta.obs.bill.sat[9,1], spillover25_B_obs=beta.obs.bill.sat[10,1],
                                 spillover50_B_obs=beta.obs.bill.sat[11,1], spillover75_B_obs=beta.obs.bill.sat[12,1])

ggplot(data.beta.bill.sat, aes(x=direct25_M_sim, fill=(direct25_M_sim>=direct25_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[1,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_25_monthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover25_M_sim, fill=(spillover25_M_sim>=spillover25_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[4,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_25_monthly.pdf")

ggplot(data.beta.bill.sat, aes(x=direct50_M_sim, fill=(direct50_M_sim>=direct50_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[2,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica Light"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_50_monthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover50_M_sim, fill=(spillover50_M_sim>=spillover50_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[5,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_50_monthly.pdf")

ggplot(data.beta.bill.sat, aes(x=direct75_M_sim, fill=(direct75_M_sim>=direct75_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[3,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_75_monthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover75_M_sim, fill=(spillover75_M_sim>=spillover75_M_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[6,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_75_monthly.pdf")

##plot bi
ggplot(data.beta.bill.sat, aes(x=direct25_B_sim, fill=(direct25_B_sim>=direct25_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[7,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_25_bimonthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover25_B_sim, fill=(spillover25_B_sim>=spillover25_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[10,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_25_bimonthly.pdf")

ggplot(data.beta.bill.sat, aes(x=direct50_B_sim, fill=(direct50_B_sim>=direct50_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[8,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_50_bimonthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover50_B_sim, fill=(spillover50_B_sim>=spillover50_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[11,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_50_bimonthly.pdf")

ggplot(data.beta.bill.sat, aes(x=direct75_B_sim, fill=(direct75_B_sim>=direct75_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[9,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("direct_effect_75_bimonthly.pdf")

ggplot(data.beta.bill.sat, aes(x=spillover75_B_sim, fill=(spillover75_B_sim>=spillover75_B_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.7)+
  geom_vline(xintercept =  beta.obs.bill.sat[12,1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  theme(axis.line = element_line( colour = "black"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Helvetica", face = "bold"),
        text=element_text(family="Helvetica"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9))+
  ggsave("spill_effect_75_bimonthly.pdf")

# Plot distributions of p-value ########################################
data.all.ttest <- data.frame(direct_sim=se.all[,1], spillover_sim=se.all[,2],
                             direct_obs=se.obs.all[1], spillover_obs=se.obs.all[2])

ggplot(data.all.ttest, aes(x=direct_sim, fill = (direct_sim <= direct_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  se.obs.all[1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  ggsave("direct_all_pval.pdf")


ggplot(data.all.ttest, aes(x=spillover_sim, fill = (spillover_sim <= spillover_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  se.obs.all[2], linetype = 1, colour="deeppink4")
+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "Spillover = 0 Test Statistics",
                     breaks = seq(0, 1,100),
                     limits=c(0, 100))+
  ggsave("spill_all_pval.pdf")

data.all.sat.ttest <- data.frame(direct25_sim=pval.all.sat[,1], direct50_sim=pval.all.sat[,2],
                                 direct75_sim=pval.all.sat[,3], spillover25_sim=pval.all.sat[,4],
                                 spillover50_sim=pval.all.sat[,5], spillover75_sim=pval.all.sat[,6],
                                 direct25_obs=pval.obs.all.sat[1], direct50_obs=pval.obs.all.sat[2],
                                 direct75_obs=pval.obs.all.sat[3], spillover25_obs=pval.obs.all.sat[4],
                                 spillover50_obs=pval.obs.all.sat[5], spillover75_obs=pval.obs.all.sat[6])

ggplot(data.all.sat.ttest, aes(x=direct25_sim, fill = (direct25_sim <= direct25_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[1], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "direct at 25% = 0 Test Statistics",
                     breaks = seq(0, 1, 0.25),
                     limits=c(0, 1))+
  ggsave("direct25_all_pval.pdf")

ggplot(data.all.sat.ttest, aes(x=direct50_sim, fill = (direct50_sim <= direct50_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[2], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "direct at 50% = 0 Test Statistics",
                     breaks = seq(0, 1, 0.25),
                     limits=c(0, 1))+
  ggsave("direct50_all_pval.pdf")

ggplot(data.all.sat.ttest, aes(x=direct75_sim, fill = (direct75_sim <= direct75_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[3], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "direct at 75% = 0 Test Statistics",
                     breaks = seq(0, .5, 0.25),
                     limits=c(0, 1))+
  ggsave("direct75_all_pval.pdf")

ggplot(data.all.sat.ttest, aes(x=spillover25_sim, fill = (spillover25_sim <= spillover25_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[4], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "spillover at 25% = 0 Test Statistics",
                     breaks = seq(0, 1, 0.25),
                     limits=c(0, 1))+
  ggsave("spillover25_all_pval.pdf")

ggplot(data.all.sat.ttest, aes(x=spillover50_sim, fill = (spillover50_sim <= spillover50_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[5], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "spillover at 50% = 0 Test Statistics",
                     breaks = seq(0, 1, 0.25),
                     limits=c(0, 1))+
  ggsave("spillover50_all_pval.pdf")

ggplot(data.all.sat.ttest, aes(x=spillover75_sim, fill = (spillover75_sim <= spillover75_obs))) +
  geom_histogram(color = "white", bins = 100, alpha=0.55)+
  geom_vline(xintercept =  pval.obs.all.sat[6], linetype = 1, colour="deeppink4")+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_x_continuous(name = "spillover at 75% = 0 Test Statistics",
                     breaks = seq(0, 1, 0.25),
                     limits=c(0, 1))+
  ggsave("spillover75_all_pval.pdf")






## Matrix of saturation ()
block_m_each <- rbind(c(sum(assign.vill=="T1")*pi[1,1],sum(assign.vill=="T1")*pi[1,2], sum(assign.vill=="T1")*pi[1,3]), 
                      c(sum(assign.vill=="T2")*pi[2,1],round(sum(assign.vill=="T2")*pi[2,2], digits=0), round(sum(assign.vill=="T2")*pi[2,3],digits = 0)),
                      c(sum(assign.vill=="T3")*pi[3,1],round(sum(assign.vill=="T3")*pi[3,2], digits=0), round(sum(assign.vill=="T3")*pi[3,3],digits = 0)),
                      c(sum(assign.vill=="T4")*pi[4,1],round(sum(assign.vill=="T4")*pi[4,2], digits=0), round(sum(assign.vill=="T4")*pi[4,3],digits = 0)))

# Household level randomization
declaration <- declare_ra(blocks = assign.vill, block_prob_each = pi)
T <- conduct_ra(declaration)

head(table(T, village))
prob_hhs <- obtain_condition_probabilities(declaration, T)
head(table(prob_hhs,T))

prob_mat <- declaration$probabilities_matrix # Matrix of assigned probabilities
cond_prob <- obtain_condition_probabilities(declaration, T) # Conditional probabilities
IPW_weights <- 1/(cond_prob) # Inverse probability weights
