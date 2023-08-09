#### FE3H and FEW3H supports, a little analysis (not really)
#### @bakaxdoaho


#### > List of all units ####

# Source: fedatamine.com; FEW3H in-game library 


AllUnits = c("Byleth", "Shez", 
             "Edelgard", "Hubert", 
               "Ferdinand", "Linhardt", "Caspar", "Bernadetta", 
               "Dorothea", "Petra", "Monica", 
             "Dimitri", "Dedue", 
               "Felix", "Ashe", "Sylvain", "Mercedes", "Annette", 
               "Ingrid", "Rodrigue", 
             "Claude", "Hilda", 
               "Lorenz", "Raphael", "Ignatz", "Lysithea", "Marianne", 
               "Leonie", "Holst",
             "Yuri", "Balthus", "Constance", "Hapi", 
             "Seteth", "Flayn", 
               "Hanneman", "Manuela", "Jeritza", "Gilbert", 
               "Alois", "Catherine", "Shamir", "Cyril", "Jeralt",
             "Rhea", "Sothis", "Arval", "Gatekeeper")


#### List of all combinations ####


#### > Data input - All supports, FE3H + FEW3H ####


AllSupports.empty = matrix(rep(0, times = length(AllUnits)^2), 
                     nrow = length(AllUnits), ncol = length(AllUnits))
rownames(AllSupports.empty) = AllUnits
colnames(AllSupports.empty) = AllUnits

AllSupports.empty


# Write the data table, manually input support data in Excel 
# 【【【SO VERY LIKELY CONTAINS TYPO/MISTAKE, I found 1 already so there has to be more sorry orz】】】 ####

# 【【【None=0, C=1, C+=1.5, B=2, B+=2.5, A=3, A+=3.5, S=4; Take the highest between FE3H and FEW3H (but only counts those with dialogues for FEW3H) 】】】 ####
write.csv(AllSupports.empty, "/FE3H.all.supports.empty.csv", row.names = TRUE)

# load the data, a lower triangle matrix
AllSupports = read.csv("/FE3H.all.supports.complete.csv", header = TRUE)
AllSupports = AllSupports[, -1]
rownames(AllSupports) = AllUnits #remove the first column of names
# str(AllSupports) # (mind the mixture of num. and int.)

# and make a version of only A/B/C support levels 
# 【【【FROM NOW ON: I'm treating Support levels A+ as A (B+ as B, C+ as C) because related in-game effects (e.g. hit/avo boosts) don't differentiate between A+ vs A】】】 ####
AllSupports.int = floor(AllSupports)
# str(AllSupports.int)

#### and make a full matrix from this lower triangle ####
library(rcompanion)
temp = t(AllSupports.int)
AllSupports.int.full = AllSupports.int + temp
# str(AllSupports.int.full)

#### and a full matrix for whether a support exists between a pair ####
IfAnySupport.full = AllSupports.int.full
for (i in 1:length(AllUnits)){
  IfAnySupport.full[,i] = as.logical(IfAnySupport.full[,i])
}



#### > So who share the most and who the least? Plus some other data? ####

# more base info ####
UnitSex = as.factor(c("Both", "Both", 
            "F", "M", "M", "M", "M", "F", "F", "F", "F",
            "M", "M", "M", "M", "M", "F", "F", "F", "M",
            "M", "F", "M", "M", "M", "F", "F", "F", "M",
            "M", "M", "F", "F",
            "M", "F", "M", "F", "M", "M", "M", "F", "F", "M", "M", "F",
            "F", "Both", "M"))
Gender.colors = c("Both"="violet", "F"="pink", "M"="lightblue")
UnitHouse = as.factor(c(rep("Protagonist", 2),
              rep("Eagle", 9), rep("Lion", 9), rep("Deer", 9),
              rep("Wolf", 4), rep("ChurchOrElse", 12),
              rep("Special", 3)))
House.colors = c("Protagonist"="purple", 
                 "Eagle"="firebrick1", "Lion"="dodgerblue",
                 "Deer"="goldenrod1", "Wolf"="thistle3",
                 "ChurchOrElse"="lightgreen", "Special"="violet")

# a. most bonds/海王x ####
Num.Of.Supports = colSums(IfAnySupport.full)
sort(Num.Of.Supports, decreasing = TRUE)
# by the way, total possible number of supports (including Rhea, Arval, Gatekeeper):
Possible.Num.Of.Supports.for.one = length(AllUnits) - 1
Possible.Num.Of.Supports.for.one # =47

# deepest bonded with everyon/用情深的海王x ####
Sum.Degree.Supports = colSums(AllSupports.int.full)
sort(Sum.Degree.Supports, decreasing = TRUE)
# P.S. max=:
Possible.Num.Of.Supports.for.one*3
Possible.Num.Of.Supports.for.one*4


# deepest/shallowest with friends ####
# sum.depth.bonds/n.bonds
Num.Of.Supports
Sum.Degree.Supports

How.deep.you.friend = Sum.Degree.Supports/Num.Of.Supports
How.deep.you.friend = sort(How.deep.you.friend, decreasing = TRUE)
# Rhea & Sothis who have only 1 bond that's S with Byleth lol
# And Sylvain you famous...

# b. number of shared bonds ####

# IfAnySupport.full

SharedBonds = matrix(data=rep(0), 
                     nrow=length(AllUnits), 
                     ncol=length(AllUnits))
rownames(SharedBonds) = AllUnits
colnames(SharedBonds) = AllUnits
for (i in 1:length(AllUnits)){
  for (j in 1:length(AllUnits)){
    for (k in 1:length(AllUnits)){
      if (IfAnySupport.full[i,j]==1 & IfAnySupport.full[i,k]==1){
        SharedBonds[j,k] = SharedBonds[j,k] + 1
      }
    }
  }
}
for (i in 1:length(AllUnits)){
  SharedBonds[i,i] = 0
}



# c. deepest within-/inter-House ####
# sum(depth within House)/sum(depth all bonds) (including Byleth & Shez)
# sum(inter-House)/sum(depth all bonds)


# ...open in Excel to see more easily ry
write.csv(AllSupports.int, file="/AllSupports.int.csv")

# calculate House by House
levels(UnitHouse)

In.Out.House = data.frame(
  Housees = levels(UnitHouse)[c(5,3,2,4,7,1,6)], 
  Within.House = rep(0, length(levels(UnitHouse))),
  Outside.House = rep(0, length(levels(UnitHouse))),
  All.House = rep(0, length(levels(UnitHouse))),
  Wihtin.House.Ratio = rep(0, length(levels(UnitHouse))),
  Outside.House.Ratio = rep(0, length(levels(UnitHouse))))
House.id = c(1, 
             3, 12, 21, 30,
             34, 46) # where each House starte in the roster
for (i in 1:length(House.id)){
  if (i == 1){
    t = House.id[i]
    t2 = House.id[i+1]
    In.Out.House$Within.House[i] = sum(
      AllSupports.int[t:(t2-1), t:(t2-1)])
    In.Out.House$Outside.House[i] = sum(
      AllSupports.int[(t2):(length(AllUnits)), t:(t2-1)])
    In.Out.House$All.House[i] = sum(
      AllSupports.int[t:(t2-1), 1:(t2-1)], 
      AllSupports.int[t2:length(AllUnits), t:(t2-1)])
  } else {
    if (i == length(House.id)) {
      t = House.id[i]
      t2 = length(AllUnits) + 1
      In.Out.House$Within.House[i] = sum(
        AllSupports.int[t:(t2-1), t:(t2-1)])
      In.Out.House$Outside.House[i] = sum(
        AllSupports.int[t:(t2-1), 1:(t-1)])
      In.Out.House$All.House[i] = sum(
        AllSupports.int[t:(t2-1), 1:(t2-1)])
    } else {
      t = House.id[i]
      t2 = House.id[i+1]
      In.Out.House$Within.House[i] = sum(
        AllSupports.int[t:(t2-1), t:(t2-1)])
      In.Out.House$Outside.House[i] = sum(
        AllSupports.int[t:(t2-1), 1:(t-1)], 
        AllSupports.int[(t2):(length(AllUnits)), t:(t2-1)])
      In.Out.House$All.House[i] = sum(
        AllSupports.int[t:(t2-1), 1:(t2-1)], 
        AllSupports.int[t2:length(AllUnits), t:(t2-1)])
    }
  }
  In.Out.House$Wihtin.House.Ratio[i] = 
    In.Out.House$Within.House[i] / In.Out.House$All.House[i]
  In.Out.House$Outside.House.Ratio[i] = 
    In.Out.House$Outside.House[i] / In.Out.House$All.House[i]
}
In.Out.House

# and inter-House communicaitons?
Housepairs = rep(0, time=length(levels(UnitHouse))^2)

Housepairs = c()
for (i in 1:length(levels(UnitHouse))){
  for (j in 1:length(levels(UnitHouse))){
    if (i != j){
      pair.i = paste0(levels(UnitHouse)[i], "-", levels(UnitHouse)[j])
      Housepairs = c(Housepairs, pair.i)
    }
  }
}
# just manually reorder into old order ry
Housepairs = Housepairs[c(27, 28, 26, 30, 25, 29, # protagonist-
                        15, 14, 18, 13, 17, # eagle-
                        20, 24, 19, 23, # lion-
                        12, 7, 11, # deer-
                        37, 42, # wolf-
                        5)] # church-special
Housepairs

bond.depths = c( # in the order of Housepairs ↑; sums/(total possible number of bonds); I calculated on Excel which is faster ry
  59/18, 59/18, 59/18, 28/8, 68/24, 4/6, # protagonist-
  38/81, 59/81, 32/36, 58/108, 0, # eagle-
  64/81, 22/36, 59/108, 0, # lion-
  27/36, 59/108, 0, # deer-
  11/48, 0, # wolf-
  0) # church-special

Inter.House = data.frame(House.pair = Housepairs,
                         Pair.bond.depths = bond.depths)
Inter.House



# d. most same/diff. gender (exclude Byleth, Shez, Arval: gender=both) ####
# cpoy the T/F IfAnySupport matrix
# replace row and col names with their gender string
# replace all diff.gender with FALSE, save as SameGender
# count remaining T which is same gender;
# original IfAnySupport minus SameGender = DiffGender
# row sums give numbers of same/diff gender for each unit

gender.support = IfAnySupport.full
gender.support = gender.support[-c(1,2,47), -c(1,2,47)] # remove the 3 gender=both
# rownames(gender.support) = UnitSex # must now have repeated names ry

str(UnitSex) # 1=Both, 2=Female, 3=Male
gender.support = rbind(UnitSex[-c(1,2,47)], gender.support)
gender.support = cbind(c(0,UnitSex[-c(1,2,47)]), gender.support)
gender.support[1:6, 1:6] # take a look
nrow(gender.support); ncol(gender.support) # =1+length(AllUnits[-c(1,2,47)])
SameGender = gender.support
for (i in 1:length(AllUnits[-c(1,2,47)])){
  for (j in 1:length(AllUnits[-c(1,2,47)])){
    if (SameGender[(i+1),1] != SameGender[1,(j+1)]){
      SameGender[(i+1),(j+1)] = 0
    }
  }
}
SameGender = SameGender[-1, -1]
SameGender[1:6, 1:6] 

DiffGender = IfAnySupport.full[-c(1,2,47),-c(1,2,47)] - SameGender
DiffGender[1:6, 1:6]

Num.Same.Gender = colSums(SameGender)
Num.Same.Gender = sort(Num.Same.Gender, decreasing = TRUE)
Num.Diff.Gender = colSums(DiffGender)
Num.Diff.Gender = sort(Num.Diff.Gender, decreasing = TRUE)


# > Some plots ####

# an output dataframe for plot section a ry ####
Results.data.1 = data.frame(
  Units = AllUnits,
  Gender = UnitSex,
  House = UnitHouse,
  Num.Of.Supports = Num.Of.Supports,
  Sum.Degree.Supports = Sum.Degree.Supports
)

# plots really starts here ry ####

library(ggplot2)
library(ggpubr)
library(ggdist)
library(gghalves)

# add House and Gender to some data (written for the plot-shared-bonds part but I'm moving this to more the front... ####
# ...IDK, I'll brute force it ry
# str(Tops.10.Bonds)
# str(Tops.5.Bonds)
# str(Results.data.1)
UnitAttrib = Results.data.1[, c(1:3)]
# str(UnitAttrib)
AddAttribInfo = function(SourceData, TargetDataFrame, WhichFromSource){
  tempAttribID = rep(0, times=nrow(TargetDataFrame))
  for (i in 1:nrow(TargetDataFrame)){
    for (j in 1:nrow(SourceData)){
      if (TargetDataFrame[i,1] ==  # assume this is the Unit Name column
          SourceData$Units[j]){ # again, mind column naming
        tempAttribID[i] = WhichFromSource[j] # e.g. WhichFromSource = UnitAttrib$House
      }
    }
  }
  GotAttribString = levels(WhichFromSource)[tempAttribID]
} # then add it to the TargetData as a new column ry


# 0. overall network ####

# trying this method: https://kateto.net/network-visualization

library(igraph)

# make the nodes & links files then the igraph object ####

# nodes
network.nodes = data.frame(id = AllUnits,
                           House = UnitHouse)
# links
network.links = 
  data.frame(
    from = rep(AllUnits, each = length(AllUnits)), # just counting it twice ry
    to = rep(AllUnits, times = length(AllUnits)),
    weight = unlist(AllSupports.int.full)) 

# the graph can't plot non-positive weight, so remove those with no support
network.links = network.links[-which(network.links$weight==0),]
# also remove those with no bonds from the nodes
temp = rep(0, nrow(network.nodes))
for (i in 1:nrow(network.nodes)){
  if (network.nodes$id[i] %in% network.links$from){} 
  else {
    temp[i] = 999
  }
}
network.nodes = network.nodes[-which(temp==999),]

# make the igraph object
head(network.links)
network = graph_from_data_frame(
  d = network.links, vertices = network.nodes, directed = FALSE)


# the plot ####

# plot(network)

# remove doubled & loops
network = simplify(network, remove.multiple = T, remove.loops = T) 

# color by House
V(network)$color = House.colors[V(network)$House]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(network, mode="all")
V(network)$size <- deg/3

# Set edge width based on weight:
E(network)$width <- E(network)$weight/4

# We can even set the network layout:
graph_attr(network, "layout") <- layout_with_lgl

#plot
plot(network, edge.arrow.size=0,
     edge.color="lightpink", vertex.frame.color="lightpink",
     edge.curved=0.1,
     layout = layout_with_graphopt(network, charge=0.02), 
     main = "Fódlan Supports \n Excluding Arval & Gatekeeper who have none :(")



# a. combine: Num.Of.Supports and Sum.Degree.Supports, ####
#   high to low, fill color by House, border color by gender

# sort a bit
Results.data.1 = Results.data.1[order(Results.data.1$Num.Of.Supports, 
                                      decreasing=TRUE),] 

# P.S. ALWAYS FACTOR the unit names so tha ggplot2 won't order them alphabetically ry ####
Results.data.1$Units = factor(Results.data.1$Units, 
                              levels = Results.data.1$Units)

str(Results.data.1)



# plot ####

p1 = ggplot(
  Results.data.1, aes(x = Units)) + 
  scale_y_continuous(name="# Supports (Max=47)",
                     breaks = seq(from=0, to=60, by=5),
                     sec.axis=sec_axis(~.*4, #same as first axis
                                       name="How Deep (Max=4*#Support)")) +
  coord_cartesian(ylim = c(0, 50)) +
  geom_bar(aes(y = Num.Of.Supports,
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_point(aes(y = Sum.Degree.Supports/4, #align with the second axis
                 group=1),
             size = 2, shape = 8) +
  geom_vline(xintercept=seq(from=5, to=50, by=5), color="slateblue") +
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan Supports") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))
p1

# too long lol, divide to see the names:
p2 = ggplot(
  Results.data.1[3:24,], aes(x = Units)) + 
    #exclude Byleth and Shez for they bond with too many lol; also Arval & Gatekeeper
  scale_y_continuous(name="# Supports (Max=47)",
                     breaks = seq(from=0, to=30, by=5),
                     sec.axis=sec_axis(~.*2.5, #same as first axis
                                       name="How Deep (Max=4*#Support)")) +
  coord_cartesian(ylim = c(0, 25)) +
  geom_bar(aes(y = Num.Of.Supports,
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_point(aes(y = Sum.Degree.Supports/2.5, 
                 group=1), #so that it draws the points ry
             size = 2, shape = 8) +
  geom_vline(xintercept=c(5,10,15,20), color="slateblue") +
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan Supports") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))
p3 = ggplot(
  Results.data.1[25:44,], aes(x = Units)) +
  scale_y_continuous(name="# Supports (Max=47)",
                     breaks = seq(from=0, to=30, by=5),
                     sec.axis=sec_axis(~.*2.5, #same as first axis
                                       name="How Deep (Max=4*#Support)")) +
  coord_cartesian(ylim = c(0, 25)) + 
  geom_bar(aes(y = Num.Of.Supports,
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_point(aes(y = Sum.Degree.Supports/2.5, 
                 group=1), #so that it draws the points ry
             size = 2, shape = 8) +
  geom_vline(xintercept=c(3,8,13,18,23), color="slateblue") +
               scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  theme(axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))

ggarrange(p2, p3, 
          ncol = 1, nrow = 2)


# performance by House ####

library(dplyr)
library(forcats)

Results.data.2 = Results.data.1
# sort a bit + so that shows on x-axis in desired order
Results.data.2$mean.n.supports = ave(Results.data.2$Num.Of.Supports,
                                     Results.data.2$House)
Results.data.2 = Results.data.2 %>%
  mutate(House=fct_reorder(House, mean.n.supports, .desc = TRUE))

# take a look at the order:
# ggplot(Results.data.2, aes(x=Num.Of.Supports, y=House)) + geom_boxplot()

# and do the same for the depth of bonds:
Results.data.3 = Results.data.1
Results.data.3$mean.depth.supports = ave(Results.data.3$Sum.Degree.Supports,
                                         Results.data.3$House)
Results.data.3 = Results.data.3 %>%
  mutate(House=fct_reorder(House, mean.depth.supports, .desc = TRUE))

library(ggrepel)

p4 = ggplot(Results.data.2, 
            aes(x = House, y = Num.Of.Supports,
                col = House)) + 
  scale_y_continuous(name="# Supports (Max=47)",
                     breaks = seq(from=0, to=60, by=5)) +
  coord_cartesian(ylim = c(0, 45)) +
  geom_boxplot(width = 0.3) +
#  geom_half_violin(aes(fill = House), side="r") +
  geom_point(aes(x=as.numeric(House), y=Num.Of.Supports)) +
  geom_text_repel(aes(label = rownames(Results.data.2))) +
  scale_color_manual(values = House.colors) +
  ggtitle("Fódlan Support Numbers by House") +
  theme(panel.background = element_rect(fill = "gray96"), # make the background color shallower
        legend.position="none", # hide legend
        plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))

p5 = ggplot(Results.data.3, 
            aes(x = House, y = Sum.Degree.Supports,
                col = House)) + 
  scale_y_continuous(name="How Deep (Max=4*#Support)",
                     breaks = seq(from=0, to=200, by=10)) +
  coord_cartesian(ylim = c(0, 170)) +
  geom_boxplot(width = 0.3) +
  geom_point(aes(x=as.numeric(House), y=Sum.Degree.Supports),
             position = position_jitter(width=0.1)) +
  geom_text_repel(aes(label = rownames(Results.data.3))) +
  scale_color_manual(values = House.colors) +
  ggtitle("Fódlan Bond Depths by House") +
  theme(panel.background = element_rect(fill = "gray96"),
        plot.title = element_text(hjust = 0.5),
        legend.position="none",
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))

ggarrange(p4, p5, 
          ncol = 2, nrow = 1) # 丑且糊，看个各组位置&极值就行ry

# show as table ####

write.csv(Results.data.2, file="/Results.data.2.csv")

# deepest/shallowest with friends ####

# add grouping attribues
deep.names = names(How.deep.you.friend)
deep.means = as.numeric(How.deep.you.friend)
How.deep.you.friend = data.frame(Unit = deep.names,
                                 AverageDepth = deep.means)

How.deep.you.friend$House = AddAttribInfo(
  UnitAttrib, How.deep.you.friend, UnitAttrib$House)
How.deep.you.friend$Gender = AddAttribInfo(
  UnitAttrib, How.deep.you.friend, UnitAttrib$Gender)
How.deep.you.friend

# factor for x axis for ggplot2 ry
How.deep.you.friend$Unit = factor(How.deep.you.friend$Unit, 
                              levels = How.deep.you.friend$Unit)

# plot
p.deep.mean = ggplot(
  How.deep.you.friend, aes(x = Unit)) + 
  scale_y_continuous(name="Average bond depths (0=None to 4=S)",
                     breaks = seq(from=0, to=5, by=0.2)) +
  coord_cartesian(ylim = c(2, 4)) +
  geom_bar(aes(y = AverageDepth, 
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_vline(xintercept=seq(from=5, to=46, by=5), color="slateblue") + #breaks into 5s so easy to see rank
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan most/least devoted friend???") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))
p.deep.mean


# b. shared bonds ####

SharedBonds.no.heros = SharedBonds[-(1:2),-(1:2)]

# check distribution: ####
length(unlist(SharedBonds.no.heros))
quantile(unlist(SharedBonds.no.heros))

library(Hmisc)

hist(unlist(SharedBonds.no.heros), 
     col = "mediumpurple1",
     main = "Distribution of number of shared friends between pairs (excluding Byleth & Shez who friend everyone ww) \n Vertical lines indicate quantiles",)
abline(v = quantile(unlist(SharedBonds.no.heros)),
       col = "slateblue1")
minor.tick(nx = 1)

# # practice: highest: ####
# most.temp = which(SharedBonds.no.heros == max(SharedBonds.no.heros), #Exclude Byleth & Shez
#       arr.ind = TRUE)
# most.temp.value = rep(0, times=nrow(most.temp))
# for (i in 1:nrow(most.temp)){
#   most.temp.value[i] = SharedBonds.no.heros[most.temp[i,1], most.temp[i,2]]
# }
# # most.temp
# # most.temp.value
# 
# Most.Shared.Bonds = data.frame(Unit.1 = names(most.temp[,1]),
#                                Unit.2 = colnames(SharedBonds.no.heros)[most.temp[,2]],
#                                Num.Shared.Bonds = most.temp.value)
# Most.Shared.Bonds
# Most.Shared.Bonds = Most.Shared.Bonds[c(2,5,4),] # manually remove repeated pairs
# Most.Shared.Bonds


# share at least 10 friends (excluding Byleth & Shez, supports with them not counted): ####

# tail(sort(unlist(SharedBonds.no.heros, use.names = FALSE)), 289)
most.10plus = which(SharedBonds.no.heros %in% tail(
  sort(unlist(SharedBonds.no.heros, use.names = TRUE)), 288),
  arr.ind = TRUE)
# most.10plus 
#these locations in SharedBonds.no.heros are >=10 bonds

# how many shared bonds at these locations:
most.10plus.value = SharedBonds.no.heros[most.10plus]
# most.10plus.value

# locate the row/cols thus the units at these locations
unit1.10bonds.loc = rep(0, times=length(most.10plus))
for(i in 1:length(most.10plus)){
  unit1.10bonds.loc[i] = ceiling(
    most.10plus[i] / nrow(SharedBonds.no.heros))
}
# unit1.10bonds.loc

unit2.10bonds.loc = rep(0, times=length(most.10plus))
for(i in 1:length(most.10plus)){
  if (most.10plus[i] %% ncol(SharedBonds.no.heros) == 0){
    unit2.10bonds.loc[i] = ncol(SharedBonds.no.heros)
  } else {
    unit2.10bonds.loc[i] = 
      most.10plus[i] %% ncol(SharedBonds.no.heros)
  }
}
# unit2.10bonds.loc 

unit1.10bonds = rownames(SharedBonds.no.heros)[unit1.10bonds.loc]
unit2.10bonds = rownames(SharedBonds.no.heros)[unit2.10bonds.loc]
unit1.10bonds
unit2.10bonds

# make the table locating the unit pairs
Share.10.Bonds = data.frame(Unit.1 = unit1.10bonds,
                            Unit.2 = unit2.10bonds,
                            N.Shared.Bonds = most.10plus.value)
# Share.10.Bonds
# remove repeated pairs
for (i in 1:nrow(Share.10.Bonds)){
  for (j in 1:nrow(Share.10.Bonds)){
    if (Share.10.Bonds[i,1]==Share.10.Bonds[j,2] &
        Share.10.Bonds[j,1]==Share.10.Bonds[i,2]){
      Share.10.Bonds[j,2] = 0
    }
  }
}
# Share.10.Bonds
str(Share.10.Bonds)
Share.10.Bonds = Share.10.Bonds[-which(Share.10.Bonds[,2]==0),]
# sort shared high to low, and give a rank
Share.10.Bonds = Share.10.Bonds[order(Share.10.Bonds$N.Shared.Bonds, 
                                      decreasing=TRUE),] 
Share.10.Bonds$Shared.rank = rank(0 - Share.10.Bonds$N.Shared.Bonds,
                                  ties.method = "min")
Share.10.Bonds

write.csv(Share.10.Bonds, "/FE3H.Share.10.Bonds.csv", row.names = FALSE)

# BTW, top among these tops? ####
Share.10.Bonds.units = c(Share.10.Bonds$Unit.1, Share.10.Bonds$Unit.2)
# str(Share.10.Bonds.units)  
Tops.10.Bonds = as.data.frame(table(Share.10.Bonds.units))
Tops.10.Bonds = Tops.10.Bonds[order(Tops.10.Bonds$Freq, 
                              decreasing=TRUE),]
Tops.10.Bonds$freq.rank = rank(0 - Tops.10.Bonds$Freq, 
                               ties.method="min")

Tops.10.Bonds$Share.10.Bonds.units = factor(Tops.10.Bonds$Share.10.Bonds.units, 
                              levels = Tops.10.Bonds$Share.10.Bonds.units)
Tops.10.Bonds
str(Tops.10.Bonds)



# and share no more than 5 friend (excluding Rhea & Sothis and the FEW3H new Units i.e. Gatekeeper, Arval, Monica, Rodrigue, Holst, Jeralt - supports with them not counted but these units themselves excluded; and excluding supports with Byleth & Shez)? ####

# tail(head(sort(unlist(SharedBonds.no.heros)), 1113), 100)
five.or.less = which(SharedBonds.no.heros %in% head(
  sort(unlist(SharedBonds.no.heros, use.names = TRUE)), 1112),
  arr.ind = TRUE)
#these locations in SharedBonds.no.heros are <=1 bonds

# how many shared bonds at these locations:
five.or.less.value = SharedBonds.no.heros[five.or.less]

# locate the row/cols thus the units at these locations
unit1.5bonds.loc = rep(0, times=length(five.or.less))
for(i in 1:length(five.or.less)){
  unit1.5bonds.loc[i] = ceiling(
    five.or.less[i] / nrow(SharedBonds.no.heros))
}

unit2.5bonds.loc = rep(0, times=length(five.or.less))
for(i in 1:length(five.or.less)){
  if (five.or.less[i] %% ncol(SharedBonds.no.heros) == 0){
    unit2.5bonds.loc[i] = ncol(SharedBonds.no.heros)
  } else {
    unit2.5bonds.loc[i] = 
      five.or.less[i] %% ncol(SharedBonds.no.heros)
  }
}

unit1.5bonds = rownames(SharedBonds.no.heros)[unit1.5bonds.loc]
unit2.5bonds = rownames(SharedBonds.no.heros)[unit2.5bonds.loc]
unit1.5bonds
unit2.5bonds

# make the table locating the unit pairs
Share.five.or.less.Bond = data.frame(Unit.1 = unit1.5bonds,
                            Unit.2 = unit2.5bonds,
                            N.Shared.Bonds = five.or.less.value)

# remove Rhea & Sothis and the FEW3H new Units
for (i in 1:nrow(Share.five.or.less.Bond)){
  if (Share.five.or.less.Bond[i,1] %in% 
      c("Gatekeeper", "Arval", 
        "Monica", "Rodrigue", "Holst", 
        "Jeralt", "Rhea", "Sothis") |
      Share.five.or.less.Bond[i,2] %in% 
      c("Gatekeeper", "Arval", 
        "Monica", "Rodrigue", "Holst", 
        "Jeralt", "Rhea", "Sothis")){
    Share.five.or.less.Bond[i,3] = 999
  }
}
Share.five.or.less.Bond = Share.five.or.less.Bond[-which(
  Share.five.or.less.Bond$N.Shared.Bonds == 999), ]

# remove self-self "pairs"
temp = Share.five.or.less.Bond
for(i in 1:nrow(temp)){
  if (temp[i,1] == temp[i,2]){
    temp[i,3] = 999
  }
}
temp # I don't know why I can't do this loop with the original dataframe directly but can with a copy ry
Share.five.or.less.Bond = temp
Share.five.or.less.Bond = Share.five.or.less.Bond[-which(
  Share.five.or.less.Bond$N.Shared.Bonds == 999), ]

# remove repeated pairs
for (i in 1:nrow(Share.five.or.less.Bond)){
  for (j in 1:nrow(Share.five.or.less.Bond)){
    if (Share.five.or.less.Bond[i,1]==Share.five.or.less.Bond[j,2] &
        Share.five.or.less.Bond[j,1]==Share.five.or.less.Bond[i,2]){
      Share.five.or.less.Bond[j,2] = 0
    }
  }
}
# str(Share.five.or.less.Bond)
Share.five.or.less.Bond = 
  Share.five.or.less.Bond[-which(Share.five.or.less.Bond[,2]==0),]
Share.5.Bond = Share.five.or.less.Bond # rename I don't want to change previous codes ry

# sort shared high to low, and give a rank
Share.5.Bond = Share.5.Bond[order(Share.5.Bond$N.Shared.Bonds, 
                                      decreasing=TRUE),] 
Share.5.Bond$Shared.rank = rank(Share.5.Bond$N.Shared.Bonds,
                                  ties.method = "min")
head(Share.5.Bond, n=10)
tail(Share.5.Bond, n=10)
# REMEMINDER - I did ".10.Bonds" but only ".5.Bond" (no "s") here, I'm too lazy to change just leave it here and remember it ry

write.csv(Share.5.Bond, "/Share.5.Bond.csv", row.names = FALSE)

# again, "top" among these bottoms? ####
Share.5.Bond.units = c(Share.5.Bond$Unit.1, Share.5.Bond$Unit.2)
# str(Share.5.Bond.units)  
Tops.5.Bonds = as.data.frame(table(Share.5.Bond.units))
Tops.5.Bonds = Tops.5.Bonds[order(Tops.5.Bonds$Freq, 
                                    decreasing=TRUE),]
Tops.5.Bonds$freq.rank = rank(0 - Tops.5.Bonds$Freq, 
                               ties.method="min")

Tops.5.Bonds$Share.5.Bond.units = factor(Tops.5.Bonds$Share.5.Bond.units, 
                                            levels = Tops.5.Bonds$Share.5.Bond.units)
Tops.5.Bonds
str(Tops.5.Bonds)



# plots ####

# add grouping attribues

Tops.10.Bonds$House = AddAttribInfo(
  UnitAttrib, Tops.10.Bonds, UnitAttrib$House)
Tops.10.Bonds$Gender = AddAttribInfo(
  UnitAttrib, Tops.10.Bonds, UnitAttrib$Gender)
Tops.5.Bonds$House = AddAttribInfo(
  UnitAttrib, Tops.5.Bonds, UnitAttrib$House)
Tops.5.Bonds$Gender = AddAttribInfo(
  UnitAttrib, Tops.5.Bonds, UnitAttrib$Gender)
# Tops.10.Bonds
# Tops.5.Bonds


# plot
p.10bonds = ggplot(
  Tops.10.Bonds, aes(x = Share.10.Bonds.units)) + 
  scale_y_continuous(name="# Friends with whom Unit shares >= 10 friends \n Excluding Byleth & Shez",
                     breaks = seq(from=0, to=30, by=2)) +
  scale_x_discrete(name = "Unit \n Excluding Byleth & Shez") +
  coord_cartesian(ylim = c(0, 20)) +
  geom_bar(aes(y = Freq, 
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_vline(xintercept=seq(from=5, to=50, by=5), color="slateblue") + #breaks into 5s so easy to see rank
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan deepest in friend web? \n (Excluding supports with Byleth & Shez who friend everyone)") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1)) # 倾斜label
p.10bonds

p.5bonds = ggplot(
  Tops.5.Bonds, aes(x = Share.5.Bond.units)) + 
  scale_y_continuous(name="# Friends with whom Unit shares <= 5 friends \n (Excluding Byleth & Shez, and \n Sothis, Rhea, and FEW3H new units i.e. Arval, Monica, Rodrigue, Holst, Jeralt, Gatekeeper)",
                     breaks = seq(from=0, to=40, by=2)) +
  scale_x_discrete(name = "Unit \n (Excluding Sothis, Rhea, Monica, Rodrigue, Holst, Jeralt, Gatekeeper)") +
  coord_cartesian(ylim = c(0, 36)) +
  geom_bar(aes(y = Freq, 
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_vline(xintercept=seq(from=5, to=50, by=5), color="slateblue") + #breaks into 5s so easy to see rank
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan shallowest in friend web??? \n (Excluding supports with Byleth & Shez who friend everyone, \n and excluding Sothis, Rhea, Arval, Monica, Rodrigue, Holst, Jeralt, Gatekeeper who weren't awarded with many supports)") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))
p.5bonds

# some people appear in both groups! Who're these who friend rather directionally?
str(Tops.10.Bonds)
str(Tops.5.Bonds)
Tops.10.Bonds$Share.10.Bonds.units %in% Tops.5.Bonds$Share.5.Bond.units
# ...that's everyone. Hmmmm......
# And these are ALL those who rank high & low in sharing bonds...
# does this mean those who don't share many friends with many people also won't share too few with people?
# how about the reverse?
Tops.5.Bonds$Share.5.Bond.units[-which(
  Tops.5.Bonds$Share.5.Bond.units %in% Tops.10.Bonds$Share.10.Bonds.units)]
# ...yeah, I guess this just reflects the fact that only if you friend broadly enough (i.e. can rank high in "sharing friends with friends") can you get to know those who don't have many friends (those in the not-sharing-friends group)
# while if you appear only in this second group then you probably really don't have many friends lol (nope
# BTW, I'm an Ashen Wolf person so I'm speciically saying it here, 
# all 4 Ashen Wolves appear in the second group which makes sense because they (as the Church people) in general have less supports, but
# YURI YOU THE ONLY ASHEN WOLF WHO DOESN'T ALSO APPEAR IN THE "FRIEND WITH THOSE WHO HAVE MANY FRIENDS" CHART! WHO THEN DO YOU FRIEND WITH www
# (2333挠挠头，尤里你，万花丛中过根本不社交！x)




# c. deepest within/inter House ####

In.Out.House2 = In.Out.House
In.Out.House3 = In.Out.House
# sort a bit + so that shows on x-axis in desired order
In.Out.House2 = In.Out.House2[order(
  In.Out.House2$Wihtin.House.Ratio, decreasing=TRUE), ]
In.Out.House3 = In.Out.House3[order(
  In.Out.House3$Outside.House.Ratio, decreasing=TRUE), ]
# factor for ggplot2...
In.Out.House2$Housees = factor(In.Out.House2$Housees, 
                              levels = In.Out.House2$Housees)
In.Out.House3$Housees = factor(In.Out.House3$Housees, 
                               levels = In.Out.House3$Housees)

p.within.House = ggplot(In.Out.House2, 
            aes(x = Housees, y = Wihtin.House.Ratio, fill = Housees)) + 
  scale_y_continuous(name="Ratio of within-House bonds (Max=1)",
                     breaks = seq(from=0, to=1, by=0.05)) +
  coord_cartesian(ylim = c(0, 0.28)) +
  geom_bar(position="dodge", stat="identity",
           size = 1.5) +
  scale_fill_manual(values = House.colors) +
  ggtitle("Fódlan Most united House?") +
  theme(panel.background = element_rect(fill = "gray96"), # make the background color shallower
        legend.position="none", # hide legend
        plot.title = element_text(hjust = 0.5)) #设置标题居中

p.outside.House = ggplot(In.Out.House3, 
                         aes(x = Housees, y = Outside.House.Ratio, fill = Housees)) + 
  scale_y_continuous(name="Ratio of outside-House bonds (Max=1)",
                     breaks = seq(from=0, to=1, by=0.2)) +
  coord_cartesian(ylim = c(0, 1)) +
  geom_bar(position="dodge", stat="identity",
           size = 1.5) +
  scale_fill_manual(values = House.colors) +
  ggtitle("Fódlan Most outgoing House?") +
  theme(panel.background = element_rect(fill = "gray96"), # make the background color shallower
        legend.position="none", # hide legend
        plot.title = element_text(hjust = 0.5)) #设置标题居中
ggarrange(p.within.House, p.outside.House,
          ncol = 2, nrow = 1)


# inter House, bar & network ####

Inter.House = Inter.House[order(Inter.House$Pair.bond.depths, decreasing=TRUE),]
Inter.House$House.pair = factor(Inter.House$House.pair, levels=Inter.House$House.pair)

p.inter.House = ggplot(Inter.House, 
                         aes(x = House.pair, y = Pair.bond.depths,
                             fill = "plum1")) + # IDK why it's not this color but whatever ry
  scale_y_continuous(name="Depths of bonds over total possible number of bonds",
                     breaks = seq(from=0, to=5, by=0.5)) +
  coord_cartesian(ylim = c(0, 3.5)) +
  geom_bar(position="dodge", stat="identity",
           size = 1.5) +
  ggtitle("Fódlan Deepest inter-House communications?") +
  theme(panel.background = element_rect(fill = "gray96"), # make the background color shallower
        legend.position="none", # hide legend
        plot.title = element_text(hjust = 0.5),  #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1)) # tilt label text
p.inter.House

# network
# nodes
network2.nodes = data.frame(id = levels(UnitHouse),
                            House = levels(UnitHouse))
# links
network2.links.raw = data.frame(
    from = rep(levels(UnitHouse), each = length(levels(UnitHouse))), # just counting it twice ry
    to = rep(levels(UnitHouse), times = length(levels(UnitHouse))),
    weight = rep(0))  
write.csv(network2.links.raw, "/network2.links.raw.csv")
Inter.House
# just manually input here ry; remove those 0 bonds
# 无关鹿狼人随机bb：鹿/狼能排到狼的2/6我看好兄弟一个人贡献俩A一B功不可没（。
# 前略以及红班果然是和教会关系最不好的（而狮鹫关系比鹫教会还不好（。
network2.links = read.csv("/network2.links.raw.csv", header=TRUE)
# make the igraph object
network2 = graph_from_data_frame(
  d = network2.links, vertices = network2.nodes, directed = FALSE)
# Compute node degrees (#links) and use that to set node size:
deg <- degree(network2, mode="all")
V(network2)$size <- deg *3
# Set edge width based on weight:
E(network2)$width <- E(network2)$weight *5
# Set color
V(network2)$color = House.colors[V(network2)$House]
#plot
plot(network2, edge.arrow.size=0,
     edge.color="lightpink", vertex.frame.color="lightpink",
     edge.curved=0.1,
     main = "Fódlan Inter-House relationships", # just realized I used "Class" rather than "House" in everywhere above...don't want to change for now, let it be ry # ...edited now ry
     layout = layout_with_dh)

# d. most same/diff. gender  ####

Num.Same.Gender = data.frame(Units = names(Num.Same.Gender),
                             n.same.gender.bonds = as.numeric(Num.Same.Gender))
Num.Diff.Gender = data.frame(Units = names(Num.Diff.Gender),
                             n.diff.gender.bonds = as.numeric(Num.Diff.Gender))
Num.Same.Gender$House = AddAttribInfo(
  UnitAttrib, Num.Same.Gender, UnitAttrib$House)
Num.Diff.Gender$House = AddAttribInfo(
  UnitAttrib, Num.Diff.Gender, UnitAttrib$House)
Num.Same.Gender$Gender = AddAttribInfo(
  UnitAttrib, Num.Same.Gender, UnitAttrib$Gender)
Num.Diff.Gender$Gender = AddAttribInfo(
  UnitAttrib, Num.Diff.Gender, UnitAttrib$Gender)
# Num.Same.Gender
# Num.Diff.Gender
Num.Same.Gender$Units = factor(Num.Same.Gender$Units, 
                              levels = Num.Same.Gender$Units)
Num.Diff.Gender$Units = factor(Num.Diff.Gender$Units, 
                               levels = Num.Diff.Gender$Units)

p.same.gender = ggplot(
  Num.Same.Gender, aes(x = Units)) + 
  scale_y_continuous(name="# Same-gender Supports \n (Excluding Byleth, Shez, Arval)",
                     breaks = seq(from=0, to=30, by=2)) +
  coord_cartesian(ylim = c(0, 10)) +
  scale_x_discrete(name = "Unit \n Excluding Byleth, Shez, Arval who can be both Female/Male") +
  geom_bar(aes(y = n.same.gender.bonds,
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_vline(xintercept=seq(from=5, to=50, by=5), color="slateblue") +
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan Popular among same gender") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))

p.diff.gender = ggplot(
  Num.Diff.Gender, aes(x = Units)) +
  scale_y_continuous(name="# Different-gender Supports \n (Excluding Byleth, Shez, Arval)",
                     breaks = seq(from=0, to=30, by=2)) +
  coord_cartesian(ylim = c(0, 14)) + 
  scale_x_discrete(name = "Unit \n Excluding Byleth, Shez, Arval who can be both Female/Male") +
  geom_bar(aes(y = n.diff.gender.bonds,
               fill = House, color = Gender),
           position="dodge", stat="identity",
           size = 1.5) +
  geom_vline(xintercept=seq(from=5, to=50, by=5), color="slateblue") +
  scale_fill_manual(values = House.colors) +
  scale_color_manual(values = Gender.colors) +
  ggtitle("Fódlan Popular among different gender") +
  theme(plot.title = element_text(hjust = 0.5), #设置标题居中
        axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))

ggarrange(p.same.gender, p.diff.gender, 
          ncol = 1, nrow = 2)

####