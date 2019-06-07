setwd("/home/sebschne/Work/archives/ijsr/analysis/")

rFromWilcox<-function(wilcoxModel, N){
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z/sqrt(N)
  # cat(wilcoxModel$data.name, "Effect Size, r = ", r)
  return(r)
}

cohens_d <- function(x, y) {
  lx <- length(x)- 1
  ly <- length(y)- 1
  md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
  csd <- lx * var(x) + ly * var(y)
  csd <- csd/(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation
  
  cd  <- md/csd                        ## cohen's d
  return(cd)
}

numformat <- function(val, i) {
  if (i==2){
    sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }
  else if(i ==3 ){
    sub("^(-?)0.", "\\1.", sprintf("%.3f", val))
  }else if(i ==4){
    sub("^(-?)0.", "\\1.", sprintf("%.24", val))
  }
}

output_ttest <- function(tmodel, data){
  t <- round(tmodel[["parameter"]],digits =2)
  tstatistics <- round(tmodel[["statistic"]],digits =2)
  print(tmodel[["p.value"]])
  
  if (tmodel[["p.value"]] < .001){
    p <- numformat(round(tmodel[["p.value"]],digits =4),4)  
  }else if(tmodel[["p.value"]] < .01){
    p <- numformat(round(tmodel[["p.value"]],digits =3),3)
  }else{
    p <- numformat(round(tmodel[["p.value"]],digits =2),2)
  }
  
  d <- numformat(round(cohens_d(data$Score[data$Conditions == "adaptability"],data$Score[data$Conditions == "adaptivity"]),digits=2),2)
  out <- paste("$t$(",t,") = ",tstatistics,", $p$ = ", p,", $d$ = ", d ,sep = '')
  return(out)
}

output_wilcoxtest <- function(wilcoxmodel,n){
  W <- round(wilcoxmodel[["statistic"]],digits =2)
  p <- numformat(round(wilcoxmodel[["p.value"]],digits =2 ),2)
  r <- numformat(round(rFromWilcox(wilcoxmodel,n), digits=2),2)
  out <- paste("$W$ = ",W,", $p$ = ", p,", $r$ = ", r ,sep = '')
  return(out)
}

# library(bibtex)
# require(knitcitations)
# bib <- read.bibtex("mybibfile.bib")

palette <- 'npg'


library(nlme)
library(ggsignif)
library(car)
library(ggplot2)
library(ggpubr)
library(multilevel)
library(mediation)
library(bda)
library(gvlma)
var_palette <- "gray"
var_ggtheme <- theme_gray()

source("pre_process.R")
alpha.wai <- round(cronbach(WAI)[['Alpha']],digits =2)
alpha.nars <- round(cronbach(NARS)[['Alpha']],digits =2)
alpha.sus <- round(cronbach(SUS)[['Alpha']],digits =2)
alpha.paes <- round(cronbach(PAES)[['Alpha']],digits =2)
alpha.tp <- round(cronbach(TP)[['Alpha']],digits =2)
alpha.warmth <- round(cronbach(ROSAS.warmth)[['Alpha']],digits =2)
alpha.discomfort <- round(cronbach(ROSAS.discomfort)[['Alpha']],digits =2)
alpha.competence <- round(cronbach(ROSAS.competence)[['Alpha']],digits =2)
alpha.coop <- round(cronbach(COOP)[['Alpha']],digits =2)
alpha.open <- round(cronbach(OPENNESS)[['Alpha']],digits =2)

n = length(alpha.wai)


result.nars <- t.test(NARS.df$Score~NARS.df$Conditions)
result.coop <- t.test(COOP.df$Score~COOP.df$Conditions)
result.prior <- wilcox.test(EXP.df$Score~EXP.df$Conditions)
result.sus <- t.test(SUS.df$Score~SUS.df$Conditions)
result.open <- wilcox.test(OPENNESS.df$Score~OPENNESS.df$Conditions)
result.paes <- wilcox.test(PAES.df$Score~PAES.df$Conditions)
result.warmth <- t.test(warmth.df$Score~warmth.df$Conditions)
result.competence <- t.test(competence.df$Score~competence.df$Conditions)
result.discomfort <- wilcox.test(discomfort.df$Score~discomfort.df$Conditions)
result.wai <- t.test(WAI.df$Score~WAI.df$Conditions)


my_comparisons <- list(c("adaptivity","adaptability"))

SCALES <- subset(hri.df, hri.df$Item == "SUS" | hri.df$Item == "Physical Activity Enjoyment" | hri.df$Item == "Cooperation" |hri.df$Item == "Working Alliance Inventory" )
p <- ggboxplot(SCALES, x = "Conditions", y = "Score",
color = "black", palette = palette,fill="Conditions",ggtheme = theme_pubclean(), facet.by = "Item",  panel.labs.font = list( size = 14))+ 
stat_compare_means(comparisons = my_comparisons, label = "p.signif", method='wilcox.test')
ggpar(p, ylim = c(1,6),
font.x = c(14),
font.y = c(14), 
font.main = c(14),
font.legend = c(14),
font.tickslab =c(14), legend ="top")
ggsave(filename="../src/figures/figure-latex/sus.pdf", plot = p)


ROSAS <- subset(hri.df,hri.df$Item == "Warmth" | hri.df$Item == "Discomfort" | hri.df$Item == "Competence")

p<-ggboxplot(na.omit(ROSAS), x = "Conditions", y = "Score",
color = "black", palette = palette,fill="Conditions",ggtheme = theme_pubclean(), panel.labs.font = list( size = 16) ,facet.by = "Item")+ 
stat_compare_means(comparisons = my_comparisons, label = "p.signif", method='t.test') + theme(axis.text.x = element_text(angle = 10))


ggpar(p,
font.x = c(14),
font.y = c(14), 
font.main = c(14),
font.legend = c(14),
font.tickslab =c(14), legend ="none")
ggsave(filename="../src/figures/figure-latex/rosas.pdf", plot = p)

crosstable_result <- chisq.test(countTable, correct = FALSE)

p_chi <- numformat(round(crosstable_result[["p.value"]],digits = 2),2)
stat_chi <- numformat(round(crosstable_result[["statistic"]], digits =2 ),2)

crosstable_result <- chisq.test(countTable, correct = TRUE)
p_chi_corr <- numformat(round(crosstable_result[["p.value"]],digits = 2),2)
stat_corr <- numformat(round(crosstable_result[["statistic"]], digits =2 ),2)

p <- ggplot(data=countTableLong) +
geom_bar(stat="identity", aes(x=Conditions, y=Count, group = Opt, fill=Opt), colour="black") +scale_fill_manual(values=c("#cccccc", "#333333")) + theme_pubclean()
ggpar(p , legend = "top", legend.title = "Opted", xlab = "Conditions",
font.x = c(14),
font.y = c(14), 
font.main = c(14),
font.legend = c(14),
font.tickslab =c(14))


library(diagram)
data <- c(0, "'1.15*'", 0,
0, 0, 0, 
"'.48*'", "'.8*** (.31)'", 0)
M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
plot<- plotmat (M, pos=c(1,2), 
name= c( "Competence","Condition", "Working Alliance"), 
box.type = "rect", box.size = 0.15, box.prop=0.1,  curve=.0)


pref_ground_truth <- read.csv("../raw/pl_study/groundtruth.csv")

test <- read.csv("melted_ranking.csv")

p <- ggplot(test) + geom_segment(aes(x=1, xend=2, y=`ranked`, yend=`learned`), size=.75, show.legend=F)
p + theme(panel.background = element_blank())

# +  # color of lines
#   labs(x="", y="Mean GdpPerCap") +  # Axis labels
#   xlim(.5, 2.5) + ylim(0,(1.1*(max(df$`1952`, df$`1957`))))  

ggplot(data = test, aes(x = category, y = id)) +
  stat_bin_2d(aes(fill = score ), 
              binwidth = c(5,15),
              colour = 'green',
              size = 1.05)

p <- ggplot(test, aes(score, learner)) + geom_tile(aes(fill = category),
                                                   colour="white",size=0.15) + 
  # scale_y_discrete(expand=c(0.1,0.1)) +
  # scale_x_discrete(expand=c(0.03,0.0))  +
  scale_fill_manual(values=rev(brewer.pal(5,"Pastel1")))
p <- p + theme(panel.spacing = unit(0, 'lines')) + facet_wrap(id~.) 
ggpar(p, legend = "top", xlab = 'ranking',ylab = '')

