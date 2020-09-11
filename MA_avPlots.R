##### avPLots for MA Research

library(car)
library(tikzDevice)

load("MA_DATASET.RData")

row.names(wb_epi_df) <- wb_epi_df$iso3c # row name change to iso3c code

### LM for gov effectiveness
lm.3 <- lm(EPI2018Score ~ GE.EST + lied_dummy + trade_log + 
               log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
           data = wb_epi_df)

### Avplot for LM.3
pdf(file = "avPlot_GE", width = 10, height = 8)
avPlot(lm.3, variable = "GE.EST", 
       id=list(n=101, cex=1, col=carPalette()[1], 
               col.lines = carPalette()[2], location="lr"))

dev.off()

### GDP and GE plot
avPlots(lm.3, terms = "log(NY.GDP.PCAP.CD)",
        id=list(n=51, cex=1, col=carPalette()[2], location="lr"))

### LM for Reg Quality

lm.4 <- lm(EPI2018Score ~ RQ.EST  + lied_dummy + trade_log + 
               log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
           data = wb_epi_df)

### AVPLOT for LM.4 
pdf(file = "avPlot_RQ", width = 8, height = 11)
avPlot(lm.4, variable = "RQ.EST", id=list(n=101, cex=1, col=carPalette()[1], location="lr"))
dev.off()

### TEXTPLOT

out.x <- lm(GE.EST ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$x.vals <- resid(out.x)

out.y <- lm(EPI2018Score ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$y.vals <- resid(out.y)

plot(lm.3)
abline()

plot.new()
abline(a = 23.7332, b =  3.0352, lwd = 1.5, col = "blue")
textplot(wb_epi_df$x.vals, wb_epi_df$y.vals, words = wb_epi_df$iso3c, 
         cex = 0.5, pch = 19, new = T, show.lines = T)


abline(a = 23.7332, b =  3.0352, lwd = 1.5, h= col = "blue")


reg <- lm(EPI2018Score ~ GE.EST + lied_dummy + trade_log + 
              log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
          data = wb_epi_df)
coeff <-  coefficients(reg)
eq <-  paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

plot()

textplot.new <- function (x, y, words, cex = 1, new = TRUE, show.lines = TRUE,...) 
{
    if (new) 
        plot(x, y, type = "n", ...)
    lay <- wordlayout(x, y, words, cex, ...)
    if (show.lines) {
        for (i in 1:length(x)) {
            xl <- lay[i, 1]
            yl <- lay[i, 2]
            w <- lay[i, 3]
            h <- lay[i, 4]
            if (x[i] < xl || x[i] > xl + w || y[i] < yl || y[i] > 
                yl + h) {
                points(x[i], y[i], pch = 16, col = "blue", cex = 0.5)
                nx <- xl + 0.5 * w
                ny <- yl + 0.5 * h
                lines(c(x[i], nx), c(y[i], ny), col = "grey")
            }
        }
    }
    text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4], 
         words, cex = cex, ...)
}


function (a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, 
          coef = NULL, untf = FALSE, ...) 
{
    int_abline <- function(a, b, h, v, untf, col = par("col"), 
                           lty = par("lty"), lwd = par("lwd"), ...) .External.graphics(C_abline, 
                                                                                       a, b, h, v, untf, col, lty, lwd, ...)
    if (!is.null(reg)) {
        if (!is.null(a)) 
            warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if (is.object(a) || is.list(a)) {
        p <- length(coefa <- as.vector(coef(a)))
        if (p > 2) 
            warning(gettextf("only using the first two of %d regression coefficients", 
                             p), domain = NA)
        islm <- inherits(a, "lm")
        noInt <- if (islm) 
            !as.logical(attr(stats::terms(a), "intercept"))
        else p == 1
        if (noInt) {
            a <- 0
            b <- coefa[1L]
        }
        else {
            a <- coefa[1L]
            b <- if (p >= 2) 
                coefa[2L]
            else 0
        }
    }
    if (!is.null(coef)) {
        if (!is.null(a)) 
            warning("'a' and 'b' are overridden by 'coef'")
        a <- coef[1L]
        b <- coef[2L]
    }
    int_abline(a = a, b = b, h = h, v = v, untf = untf, ...)
    invisible()
}
 