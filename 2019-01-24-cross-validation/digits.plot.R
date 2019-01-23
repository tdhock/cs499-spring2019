data(zip.train, package="ElemStatLearn")
library(data.table)
library(ggplot2)

zip.wide <- data.table(zip.train)
setnames(zip.wide, c(
  "digit", paste(rep(1:16, 16), rep(1:16, each=16))))
zip.wide[, example.i := 1:.N]
zip.tall <- melt(zip.wide, id.vars=c("example.i", "digit"))
zip.tall[, row := as.integer(sub(".* ", "", variable))]
zip.tall[, col := as.integer(sub(" .*", "", variable))]
zip.some <- zip.tall[example.i %in% 1:10]

zip.feature.mat <- zip.train[,-1]
pca <- princomp(zip.feature.mat)
pca.dt <- data.table(pca$scores[,1:2], label=zip.train[,1])

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_text(aes(
    Comp.1, Comp.2, label=label, color=factor(label)),
    data=pca.dt)

## viz digits.
ggplot()+
  facet_wrap("example.i")+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_tile(aes(
    col, -row, fill=value),
    data=zip.some)+
  scale_fill_gradient(low="white", high="black")
