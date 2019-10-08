## ----lorenz_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----

# Lorenz Curve
par(mar=c(6,4,2,2))

plot(Lc(com$INC, com$TH),                 
     panel.first=grid(10, 10),
     main="Lorenz Curve - Income distribution",
     xlab="Percent of income (cumulative)",
     ylab="Percent of households"
)

# Inequality indexes (Gini, standard deviation and cofficient of variation)
Gini(com$ratio)
sd(com$ratio, na.rm=TRUE)
sd(com$ratio, na.rm = TRUE)/mean(com$ratio, na.rm = TRUE) * 100

