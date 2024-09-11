#CPK: my comments preceded by 'CPK'
#CPK: need to load libraries [-1], e,g, load(tidyverse)

#CPK: Objective 1
#CPK: file not in the repo [-1]. I can't loaded it unless I add it.
dat <- read.csv("scales.csv")
#CPK: Objective 2
sapply(dat, class)
#CPK: Objective 3
dim(dat)
#CPK: unneeded
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
length(species)
A.rup <- length(dat$species[dat$species==species[1]])
L.gib <- length(dat$species[dat$species==species[2]])
L.mac <- length(dat$species[dat$species==species[3]])
M.sal <- length(dat$species[dat$species==species[4]])
M.sax <- length(dat$species[dat$species==species[5]])
P.fla <- length(dat$species[dat$species==species[6]])
species.obs <- data.frame(sp=species, n=c(A.rup,L.gib,L.mac,M.sal,M.sax,P.fla))
species.obs
#CPK: Objective 4
dat %>%
  group_by(species) %>%
  summarise(n=n())
#CPK: Objective 5
dat %>%
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#CPK: Objective 6, though named incorrectly
pdf("species_quadrant.pdf")
for(i in species) {
  p <- dat %>%
    filter(species==i) %>%
    ggplot()+geom_boxplot(aes(x=quadrant, y=N)) + ggtitle(i)
  print(p)
}
dev.off()

#CPK: Unneeded
list.files(pattern=".pdf")

#CPK: this is a good effort and it appears to be makings sense. But, there's a little bit that's unneeded (e.g., examples from description, etc. Also it's very hard to follow bc there's little organization according to the objectives. Good work overall.

#Total points: 8