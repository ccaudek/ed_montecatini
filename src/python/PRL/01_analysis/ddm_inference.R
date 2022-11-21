setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to this file path

# load pkgs ====
library("tidybayes"); library("tidyverse")

# load data (extracted ddm model traces) ====
load("ddm/traces.Rda")

# THRESHOLD (a) ====
# marginal posterior median and CI95
a_long %>% 
  group_by(diag_cat, stim, draw) %>% 
  summarize(M=mean(val)) %>% 
  tidybayes::median_qi(M)

# compute contrasts
a_contrasts =
  a_long %>% 
  group_by(diag_cat, stim, draw) %>% 
  summarize(M=mean(val)) %>% 
  pivot_wider(names_from = c(diag_cat, stim), values_from = M, names_prefix = "m") %>% 
  mutate(
    a_an = mAN_food - mAN_neutral,
    a_hc = mHC_food - mHC_neutral,
    a_ah = a_an - a_hc) %>% 
  pivot_longer(cols = c(a_an, a_hc, a_ah), names_to = "contrast", values_to = "delta")

# keep just slow vs silence and fast vs silence contrasts
a_contrasts2 =
  a_contrasts %>% 
  filter(contrast != "a_ah") %>% 
  select(delta, draw, contrast) %>% 
  mutate(
    contrast=factor(contrast, levels=c("a_an", "a_hc")))

# compute posterior summaries
a_contrasts_summ =
  a_contrasts %>% 
  group_by(contrast) %>% 
  median_qi(delta)

# keep just slow vs silence and fast vs silence contrasts
a_contrasts_summ2 =
  a_contrasts_summ %>% 
  filter(contrast!="a_ah") %>% 
  mutate(ys = c(5, 1),
         contrast=factor(contrast, levels=c("a_an", "a_hc")))

# plot threshold contrasts (fig 4a)
tempo_cols=c("#E69F00", "#56B4E9")
hdiwd=5
psz=15
fsz=60
alf=.8

ggplot() +
  scale_fill_manual(values=tempo_cols) +
  scale_color_manual(values=tempo_cols) +
  
  # SILENCE ref line
  geom_segment(aes(x = 0, y =-0.2, xend = 0, yend = 56),
               colour = "grey", lwd = 5 , lty=1, alpha =.8) +
  
  # DENSITIES
  stat_halfeye(data=a_contrasts2, aes(y=1,x=delta, fill=contrast),
                .width=0, alpha=alf, lwd=0,scale = 56) +
  # HDI
  geom_segment(data=a_contrasts_summ2, 
               aes( y=-1-ys,
                    yend=-1-ys,
                    x=.lower, 
                    xend=.upper),
               lwd=hdiwd, lineend = "round") +
  #MEDIAN
  geom_point(data=a_contrasts_summ2, aes(x=delta,y=-1-ys), 
            size=psz,shape=21,color="black", fill="black")+
  geom_point(data=a_contrasts_summ2, aes(x=delta,y=-1-ys), 
            size=psz-4,shape=21, fill="white")+
  geom_point(data=a_contrasts_summ2, aes(x=delta,y=-1-ys, fill=contrast), 
            alpha=.75,size=psz-4,shape=21)+
  geom_hline(yintercept = -0.2,lwd=1, lty=1)+
  labs(y= "", x= "") +
  coord_flip() +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        text = element_text(size=fsz),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x= element_blank())

# ggsave('figures/ddm/a_param_grp.png', width = 18, height = 10, dpi = 300)


# fast vs slow music contrast  (figure-4b)
a_contrasts3 =
  a_contrasts %>% 
  filter(contrast=="a_ah") %>% 
  select(delta, draw, contrast)

a_contrasts_summ3 =
  a_contrasts_summ %>% 
  filter(contrast=="a_ah")

ggplot() +
  # ref line
  geom_vline (xintercept = 0,lwd=1.5, lty=2)+
  
  # #DENSITY
  stat_halfeyeh(data=a_contrasts3,aes(y=0,x=delta), 
                fill=tempo_cols[1],.width=0, alpha=alf,lwd=0,scale = 1)+
  # HDI
  geom_segment(data=a_contrasts_summ3, 
               aes( y=0,yend=0,
                    x=.lower,xend=.upper),
               lwd=6, lineend = "round")+
  #MEDIAN
  geom_point(data=a_contrasts_summ3, aes(x=delta,y=0), 
             size=psz,shape=21,color="black", fill="black")+
  geom_point(data=a_contrasts_summ3, aes(x=delta,y=0), 
             size=psz-4,shape=21, fill="white")+
  labs(y= "", x= "") +
  theme_classic() +
  coord_flip()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        text = element_text(size=fsz+10),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x= element_blank())
# ggsave('figures/ddm/a-SvsF.png', width = 18, height = 10, dpi = 300)


###### until here ! ! ! ! !



# Threshold  music-contrasts by task
a.contrasts.task=
  a.long %>% 
  group_by(tempo,task_idx) %>% 
  pivot_wider(names_from = tempo, values_from = val, names_prefix = "m") %>% 
  mutate(
    f0=m190bpm-msilence,
    s0=m40bpm-msilence,
    sf=m40bpm-m190bpm) %>% 
  pivot_longer(cols = c(f0,s0,sf), names_to = "contrast", values_to = "delta") %>% 
  ungroup() %>% 
  select(draw, task_idx, contrast, delta)

# keep just slow vs silence and fast vs silence contrasts
a.contrasts.task2=
  a.contrasts.task %>% 
  filter(contrast!="sf") %>% 
  mutate(
    contrast=factor(contrast, levels=c("s0","f0")))

# compute posterior summaries (used for Supp. Table 1)
a.contrasts.task.summ=
  a.contrasts.task%>% 
  group_by(task_idx,contrast) %>% 
  median_qi(delta)
# keep just slow vs silence and fast vs silence contrasts
a.contrasts.task.summ2=
  a.contrasts.task.summ %>% 
  filter(contrast!="sf") %>% 
  mutate(ys=c(2,1),
         contrast=factor(contrast, levels=c("s0","f0")))

# iterate plots (used in SUpp. Fig 1)
for (tkstr in levels(a.contrasts.task$task_idx)){
  
  # filter task samples
  task.contrasts=
    a.contrasts.task2 %>% 
    filter(task_idx==tkstr)
  
  task.summ=
    a.contrasts.task.summ2 %>% 
    filter(task_idx==tkstr)
  
  
  # limy=max(task.contrasts$delta)
  limy=14
  tempo_cols=c("#E69F00", "#56B4E9")
  hdiwd=6
  psz=18
  fsz=100
  alf=.8
  
  ggplot() +
    scale_fill_manual(values=tempo_cols) +
    scale_color_manual(values=tempo_cols) +
    scale_x_continuous(breaks=c(-.24,-.16,-.08,0)) +
    # SILENCE
    geom_segment(aes(x = 0, y =-0.2, xend = 0, yend = limy+2),
                 colour = "grey", lwd = 5 , lty=1, alpha =.8)+
    
    # DENSITIES
    stat_halfeyeh(data=task.contrasts,aes(y=0,x=delta, fill=contrast),
                  .width=0, alpha=alf,lwd=0,scale = limy)+
    # HDI
    geom_segment(data=task.summ, 
                 aes( y=(-.008*limy)-(ys*.07* limy),
                      yend=(-.008*limy)-(ys*.07* limy),
                      x=.lower, 
                      xend=.upper),
                 lwd=hdiwd, lineend = "round")+
    #MEDIAN
    geom_point(data=task.summ, aes(x=delta,y=(-.008*limy)-(ys*.07* limy)), 
               size=psz,shape=21,color="black", fill="black")+
    geom_point(data=task.summ, aes(x=delta,y=(-.008*limy)-(ys*.07* limy)), 
               size=psz-4,shape=21, fill="white")+
    geom_point(data=task.summ, aes(x=delta,y=(-.008*limy)-(ys*.07* limy), fill=contrast), 
               alpha=.75,size=psz-4,shape=21)+

    
    geom_hline (yintercept = -0.2,lwd=1, lty=1)+
    labs(y= "", x= "") +
    coord_flip(xlim =c(-.25,.007) )+
    
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          panel.spacing.x = unit(0,"line"),
          text = element_text(size=fsz),
          legend.position="none",
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x= element_blank())
  
  ggsave(paste("figures/ddm/",tolower(tkstr),'-a-tempo.png',sep=""), width = 18, height = 10, dpi = 300)
}


# DRIFT RATE (v)====
# difficulty eeffect on drift-rate, by task
dif.cols=c("#66a182","#edae49","#d1495b")
v.long%>%
  ggplot(aes(val, group=dif,  fill=dif))+
  scale_fill_manual(values = dif.cols)+
  labs(x="Drift rate", y="density\n")+
  geom_density(color="white", alpha=.5)+
  geom_hline (yintercept = 0,lwd=.5, lty=1)+
  facet_grid(task_idx~.)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        text = element_text(size=30),
        axis.text =  element_text(size=18)
  )

# music effect on drift rate (reported in main text)
vmusic.long %>% 
  pivot_wider(names_from = tempo, values_from = val, names_prefix = "m") %>% 
  mutate(
    sf=m40bpm-m190bpm) %>% 
  pivot_longer(cols = c(m40bpm ,m190bpm,sf), names_to = "contrast", values_to = "delta") %>%
  group_by(contrast) %>% 
  median_qi(delta)

# NON- DECISION TIME (t)====
t.long%>%
  ggplot(aes(x=val*1000,y=0.05, group=task_idx,  fill=task_idx))+
  labs(x="Non-decision time (ms)", y="density\n")+
  stat_halfeyeh(alpha=.5, lwd=8)+
  geom_hline (yintercept = 0,lwd=.5, lty=1)+
  facet_grid(task_idx~.)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        text = element_text(size=32),
        axis.text.y = element_blank()
  )


# INTER-TRIAL VARIABILITY (sv)====
sv.long %>% 
  ggplot(aes(x=val,y=0.05, group=task_idx,  fill=task_idx))+
  labs(x="Inter-trial variability", y="density\n")+
  stat_halfeyeh(alpha=.5, lwd=8)+
  geom_hline (yintercept = 0,lwd=.5, lty=1)+
  facet_grid(task_idx~.)+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank(),
        text = element_text(size=32),
        axis.text.y = element_blank()
  )
