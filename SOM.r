data(yeast)
yeast <- yeast[, -c(1, 11)]
yeast.f <- filtering(yeast)
yeast.f.n <- normalize(yeast.f)
foo <- som(yeast.f.n, xdim=5, ydim=6)
foo <- som(yeast.f.n, xdim=4, ydim=1, topol="rect", neigh="gaussian")
plot(foo)

NBA.SOM1 <- som(scale(yeast.f), grid = somgrid(2, 2, "rectangular"))
plot(NBA.SOM1)
plot(NBA.SOM1, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(NBA.SOM1, main = "Default SOM Plot")
