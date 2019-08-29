# This script is preprocessing the data of the adaptable condition
# setwd("/home/sebschne/Work/agaitheses.git.thesis-sebschne/results/pl_learning_study/")
setwd("/home/sebschne/Work/archives/ijsr/analysis/")
data_set <- read.csv('../raw/answercodes_complete_cleaned.csv')
## first let's extract the data from the original data set before we continue processing

exp <- cbind(data_set$G3Q00004.EXP1.,data_set$G3Q00004.EXP5.,data_set$G3Q00004.EXP2.,data_set$G3Q00004.EXP3.,data_set$G3Q00004.EXP4.)

countTable <- table(data_set[["WDH"]][1:40],data_set[["condition"]][1:40])
library(reshape2)
countTableLong <- melt(countTable,id.vars=c('adaptability','adaptivity')) 
countTableLong <- subset(countTableLong, countTableLong[["Var1"]]=="Y" | countTableLong[["Var1"]]=="N")
colnames(countTableLong) <- c("Opt",'Conditions','Count')
library(gmodels)
#countTable <- countTable[2:3,]
# crosstable_result <- CrossTable(countTable,fisher=TRUE, chisq=TRUE,expected=TRUE,sresid=TRUE)

### 
NEO_NEUROTICISM <- cbind(data_set$NEOFFI.NEURO11.,data_set$NEOFFI.NEURO6.,data_set$NEOFFI.NEURO21.,data_set$NEOFFI.NEURO26.,data_set$NEOFFI.NEURO41.,data_set$NEOFFI.NEURO51.)
NEO_EXTROVERSION <- cbind(data_set$NEOFFI.EXTRA2.,data_set$NEOFFI.EXTRA7.,data_set$NEOFFI.EXTRA22.,data_set$NEOFFI.EXTRA32.,data_set$NEOFFI.EXTRA37.,data_set$NEOFFI.EXTRA52.)
NEO_CONSCIENTOUSNESS <- cbind(data_set$NEOFFI.CONSCIE10.,data_set$NEOFFI.CONSCIE5.,data_set$NEOFFI.CONSCIE20.,data_set$NEOFFI.CONSCIE40.,data_set$NEOFFI.CONSCIE50.,data_set$NEOFFI.CONSCIE55.)
NEO_OPENNESS <- cbind(data_set$NEOFFI.OPEN8.,data_set$NEOFFI.OPEN13.,data_set$NEOFFI.OPEN23.,data_set$NEOFFI.OPEN43.,data_set$NEOFFI.OPEN48.,data_set$NEOFFI.OPEN58.)
NEO_AGREEABLENESS <- cbind(data_set$NEOFFI.AGREE9.,data_set$NEOFFI.AGRE14.,data_set$NEOFFI.AGRE24.,data_set$NEOFFI.AGRE39.,data_set$NEOFFI.AGRE49.,data_set$NEOFFI.AGRE59.)


### WORKING ALLIANCE 
WAI <- cbind(
  data_set$WAI.WAI001.,
  data_set$WAI.WAI002.,
  data_set$WAI.WAI003.,
  data_set$WAI.WAI004.,
  data_set$WAI.WAI005.,
  data_set$WAI.WAI006.,
  data_set$WAI.WAI007.,
  data_set$WAI.WAI008.,
  data_set$WAI.WAI009.,
  data_set$WAI.WAI010.,
  data_set$WAI.WAI011.,
  data_set$WAI.WAI012.
)

 # alpha.wai <- alpha(WAI)[['total']]['raw_alpha']

PAES <- cbind(6 - data_set$PAES.SQ001.,
              data_set$PAES.SQ002.,
              data_set$PAES.SQ003.,
              6 - data_set$PAES.SQ004.,
              6 - data_set$PAES.SQ005.,
              data_set$PAES.SQ006.,
              6-data_set$PAES.SQ007.,
              data_set$PAES.SQ008.,
              6-data_set$PAES.SQ009.,
              6-data_set$PAES.SQ010.,
              6-data_set$PAES.SQ011.,
              data_set$PAES.SQ012.,
              6-data_set$PAES.SQ013.,
              6-data_set$PAES.SQ014.,
              data_set$PAES.SQ015.,
              6-data_set$PAES.SQ016.,
              6-data_set$PAES.SQ017.)
# alpha.paes <- alpha(PAES)[["total"]]['raw_alpha']

SUS <- cbind(data_set$SUS.SUS001.,
             6-data_set$SUS.SUS002.,
             data_set$SUS.SUS003.,
             data_set$SUS.SUS005.,
             6-data_set$SUS.SUS006.,
             6-data_set$SUS.SUS008.,
             data_set$SUS.SUS009.,
             data_set$SUS.SUS010.,
             6-data_set$SUS.SUS011.,
             6-data_set$SUS.SUS012.)
# alpha.sus <- alpha(SUS)[["total"]]['raw_alpha']

AGENCY <- cbind(
  data_set$Agency.SQ001.,
  data_set$Agency.SQ002.,
  data_set$Agency.SQ003.,
  data_set$Agency.SQ004.,
  data_set$Agency.SQ005.,
  data_set$Agency.SQ006.,
  data_set$Agency.SQ007.,
  data_set$Agency.SQ008.,
  data_set$Agency.SQ009.
  
)
# alpha.agency <- alpha(AGENCY)$totatl$raw_alpha

TP <- cbind(data_set[101:104],6-data_set[105])
# alpha.tp <- alpha(TP)[["total"]]['raw_alpha']

COOP <- cbind(data_set$Coop.SQ001.,data_set$Coop.SQ002.,data_set$Coop.SQ003.)
# alpha.coop <- alpha(COOP)[["total"]]['raw_alpha']

ROSAS.competence <- cbind(data_set$ROSAS.competence001.,
  data_set$ROSAS.competence002.,
  data_set$ROSAS.competence003.,
  data_set$ROSAS.competence004.,
  data_set$ROSAS.competence005.
)

# alpha.competence <- alpha(ROSAS.competence)[["total"]]['raw_alpha']

ROSAS.warmth <- cbind(
  data_set$ROSAS.warmth01.,
  data_set$ROSAS.warmth002.,
  data_set$ROSAS.warmth003.,
  data_set$ROSAS.warmth004.,
  data_set$ROSAS.warmth005.,
  data_set$ROSAS.warmth006.
)
# alpha.warmth <- alpha(ROSAS.warmth)[["total"]]['raw_alpha']


ROSAS.discomfort <- cbind(
  data_set$ROSAS.discomfort001.,
  data_set$ROSAS.discomfort002.,
  data_set$ROSAS.discomfort003.,
  data_set$ROSAS.discomfort004.,
  data_set$ROSAS.discomfort005.,
  data_set$ROSAS.discomfort006.
)
# alpha.discomfort <- alpha(ROSAS.discomfort)[["total"]]['raw_alpha']

OPENNESS <- cbind(
  data_set$INFL.SQ001.,
  data_set$INFL.SQ002.,
  data_set$INFL.SQ003.,
  data_set$INFL.SQ004.,
  data_set$INFL.SQ005.,
  data_set$INFL.SQ006.,
  data_set$INFL.SQ007.
)

# alpha.openness <- alpha(OPENNESS)[["total"]]['raw_alpha']

NARS <- cbind(
  data_set$NARS.S21.,
  data_set$NARS.S22.,
  6-data_set$NARS.S31.,
  6-data_set$NARS.S34.,
  6-data_set$NARS.S32.,
  data_set$NARS.S33.,
  data_set$NARS.S12.,
  data_set$NARS.S13.,
  data_set$NARS.S14.,
  data_set$NARS.S15.,
  data_set$NARS.S23.,
  data_set$NARS.S16.,
  data_set$NARS.S24.,
  data_set$NARS.S25.
)

# alpha.nars <- alpha(NARS)[["total"]]['raw_alpha']

prepare <- function(x,name){
  x <- na.omit(x)
  conditions <- data_set$condition[1:40]
  df <- cbind(name, as.numeric(rowMeans(x)))
  df <- as.data.frame(df)
  df["Conditions"] <- conditions
  colnames(df) <- c("Item","Score","Conditions")
  df["Score"] <- lapply(df["Score"],as.character)
  df["Score"] <- lapply(df["Score"],as.numeric)
  return(df)
}

EXP.df <- prepare(exp[1:40,],"Experience")
NARS.df <- prepare(NARS,"NARS")
OPENNESS.df <- prepare(OPENNESS[1:40,],"Openness")
discomfort.df <- prepare(ROSAS.discomfort,"Discomfort")
warmth.df <- prepare(ROSAS.warmth,"Warmth")
competence.df <- prepare(ROSAS.competence,"Competence")
COOP.df <- prepare(COOP,"Cooperation")
TP.df <- prepare(TP[1:40,],"Team Perception")
AGENCY.df <- prepare(AGENCY,"Agency")
SUS.df <- prepare(SUS[1:40,],"SUS")
PAES.df <- prepare(PAES[1:40,], "Physical Activity Enjoyment")
WAI.df <- prepare(WAI, "Working Alliance Inventory")
FIRST_RANKED.df <- data.frame(data_set$NLP.1.,data_set$condition)
colnames(FIRST_RANKED.df) <- c("FIRST","condition")

NEO_AGREEABLENESS.df <- prepare(NEO_AGREEABLENESS[1:40,], "Agreeableness") 
NEO_OPENNESS.df <- prepare(NEO_OPENNESS[1:40,], "Opennens") 
NEO_CONSCIENTOUSNESS.df <- prepare(NEO_CONSCIENTOUSNESS[1:40,], "Conscientiousness") 
NEO_EXTROVERSION.df <- prepare(NEO_EXTROVERSION[1:40,], "Extroversion") 
NEO_NEUROTICISM.df <- prepare(NEO_NEUROTICISM[1:40,], "Neuroticism") 

NEO.df <- rbind(NEO_AGREEABLENESS.df,NEO_OPENNESS.df,NEO_CONSCIENTOUSNESS.df,NEO_EXTROVERSION.df,NEO_NEUROTICISM.df)

my_comparisons <- list(c("adaptive","adaptable"))
ggboxplot(na.omit(NEO.df), x = "Item", y = "Score", ggtheme = theme_pubclean(),label.rectangle = TRUE, palette = "npg",fill=c("Conditions"))

ggboxplot(na.omit(NEO.df), x = "Conditions", y = "Score",fill=c("Conditions"), facet.by = "Item") + stat_compare_means(comparisons = my_comparisons, label = "p.value", method='t.test')

hri.df <- rbind(NARS.df,OPENNESS.df,
                discomfort.df,warmth.df,competence.df,
                COOP.df,TP.df,AGENCY.df,
                SUS.df,PAES.df,WAI.df,NEO.df)






