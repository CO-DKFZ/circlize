circos.par$circle.margin = 0.3
circos.par$cell.padding = rep(0, 4)
circos.par$track.margin = rep(0, 2)

circos.initialize(sectors = letters[1:4], xlim = c(0, 1))

sectors = sample(letters[1:4], 40, replace = TRUE)
x = runif(40)
text = sapply(letters[sample(26, 40, replace = TRUE)], function(x) strrep(x, sample(4:6, 1)))
circos.stackedText(sectors, x, text, bg.col = "#EEEEEE")

circos.track(ylim = c(0, 1), track.height = 0.02)



sectors = sample(letters[1:4], 20, replace = TRUE)
x = runif(20)
text = sapply(letters[sample(26, 20, replace = TRUE)], function(x) strrep(x, sample(4:6, 1)))
circos.stackedText(sectors, x, text, bg.col = "#EEEEEE", side = "inside")

circos.clear()



circos.initialize(sectors = letters[1:4], xlim = c(0, 1))
circos.track(ylim = c(0, 1), track.height = 0.4)

circos.rect(0.1, 0.1, 0.4, 0.4, radius = "4mm")
circos.rect(0.5, 0.1, 0.8, 0.9, radius = "4mm")


