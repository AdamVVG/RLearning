set.seed(123)
x = runif(500)
mu = sin(2*(4*x-2)) + 2*exp(-(16^2)*((x-.5)^2))
y = rnorm(500, mu, .3)
d = data.frame(x,y)

color <- rep(2, length(y))
color_transparent <- adjustcolor(color, alpha.f = 0.3)

mod_poly2 = lm(y ~ poly(x, 2))
poly2 = data.frame(x, as.numeric(predict(mod_poly2)))

mod_poly3 = lm(y ~ poly(x, 3))
poly3 = data.frame(x, as.numeric(predict(mod_poly3)))

mod_poly4 = lm(y ~ poly(x, 4))
poly4 = data.frame(x, as.numeric(predict(mod_poly4)))

mod_poly5 = lm(y ~ poly(x, 5))
poly5 = data.frame(x, as.numeric(predict(mod_poly5)))

plot(x, y, pch=20, col = color_transparent)
lines(poly2[order(x),], col = 3)
lines(poly3[order(x),], col = 4)
lines(poly4[order(x),], col = 5)
lines(poly5[order(x),], col = 6)
legend(0.82, 2.2, legend=c("2nd degree", "3rd degree", "4th degree", "5th degree"),
       col=3:6, lty=1, cex=0.8)

data()
