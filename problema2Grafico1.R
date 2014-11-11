musicas <- read.csv('data.csv')
musicas$nomeMusica <- tolower(musicas$nomeMusica)

titulos <- tolower(musicas$nomeMusica)
lista <-strsplit(titulos, "\\W+", perl=TRUE)
vetor <- unlist(lista)
freq <- table(vetor)
sorted.freq <- sort(freq, decreasing = T)

sorted.table<-paste(names(sorted.freq), sorted.freq, sep="\t")

cat("Word\tFREQ", sorted.table, file="ordenadas", sep="\n")
palavras <- read.table("ordenadas", sep="\t")
#palavras pra analisar, por exemplo:
#love, life, night, world,  heart, baby, little, good, girl, never, home, god

#portugues/espanhol
#amor, vida, mundo, corazon, chica (girl), nunca, casa, adios
#noite so tem 1 / coração 1 / pequeno(a) 1 / 

#pra ver quais musicas tem o titulos, exemplo
summary(musicas$tag[grep("you",musicas$nomeMusica)])
summary(musicas$tag[grep("love",musicas$nomeMusica)])
summary(musicas$tag[grep("life",musicas$nomeMusica)])
summary(musicas$tag[grep("girl",musicas$nomeMusica)])


you <- summary(musicas$tag[grep("you",musicas$nomeMusica)])
names(you)

love <- c(summary(musicas$tag[grep("love",musicas$nomeMusica)]))
names(love)

life <- c(summary(musicas$tag[grep("life",musicas$nomeMusica)]))
names(life)

girl <- c(summary(musicas$tag[grep("girl",musicas$nomeMusica)]))
names(girl)


# for ggplot2 0.92 release
library(ggplot2)
library(reshape)
library(grid)

dfpalavras <- structure(list(Temas = structure(c(4L, 2L, 3L, 1L), .Label = c("You", "Life", "Love", "Girl"), class = "factor"), 
                             Electronica = c(1, 0, 0, 5), Jazz = c(0, 0, 4, 4), Pop = c(16, 5, 40, 59), Reggae= c(0, 2, 6, 13), Rock = c(16, 16, 72, 171)),
                        .Names = c("Temas", "Electronica", "Jazz", "Pop", "Reggae", "Rock"), class = "data.frame", row.names = c(NA, -4L))
dfm <- melt(dfpalavras, variable_name = "genero")

levels(dfm$genero) <- c("Electronica", "Jazz", "Pop", "Reggae", "Rock")
p <- ggplot(dfm, aes(genero, value, group = Temas, colour = Temas))
p1 <- p + geom_line(size = 1)

p2 <- p1 + theme_bw() 
+ theme(panel.grid.major = none, panel.grid.minor = none, legend.position = "none",
        panel.background = none, panel.border = none, axis.line = element_line(colour = "grey50"))
p3 <- p2 + geom_vline( colour = "grey85", alpha = 0.5) + 
  geom_hline(yintercept = 32, colour = "grey80", alpha = 0.5) 
p4 <- p3 + geom_text(data = dfm[dfm$genero == "Pop", ], aes(label = Temas), hjust = 0.7, vjust = 1)
data_table <- ggplot(dfm, aes(x = genero, y = factor(Temas), label = format(value, nsmall = 1), colour = Temas)) 
+ geom_text(size = 3.5) + theme_bw() + scale_y_discrete(labels = abbreviate,
                                                        limits = c("Life", "Love", "You")) 
+ theme(panel.grid.major = none, legend.position = "none", panel.border = none, axis.text.x = none, 
        axis.ticks = none, plot.margin = unit(c(-0.5, 1, 0, 0.5), "lines")) + labs(x = NULL, y = NULL)
Layout <- grid.layout(nrow = 2, ncol = 1, heights = unit(c(2, 0.25), c("null", "null")))
grid.show.layout(Layout)
vplayout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}
subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
mmplot <- function(a, b) {
  vplayout()
  print(a, vp = subplot(1, 1))
  print(b, vp = subplot(2, 1))
}
mmplot(p4, data_table)

