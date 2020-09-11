### GGPLOTS for Partial Regression PLot
library(tikzDevice)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(ggpmisc)

load("MA_DATASET.RData")

row.names(wb_epi_df) <- wb_epi_df$iso3c # row name change to iso3c code

## Residuals to plot
out.x <- lm(GE.EST ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$x.vals <- resid(out.x)

out.y <- lm(EPI2018Score ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$y.vals <- resid(out.y)

wb_epi_df$region <- as.factor(wb_epi_df$region)

wb_epi_df$lied_dummy2 <- as.numeric(wb_epi_df$lied_dummy)

wb_epi_df$lied_dummy2[wb_epi_df$lied_dummy2 == 0] <- "Non-democratic"
wb_epi_df$lied_dummy2[wb_epi_df$lied_dummy2 == 1] <- "Democratic"

wb_epi_df$Regime <- as.factor(wb_epi_df$lied_dummy2)

### colours

"#377EB8" "#4DAF4A"

display.brewer.all()

geom_text(aes(label=ifelse(EPI2018Score < -5 | GE.EST > as.character(Name),'')),hjust=0,vjust=0)


geom_text_repel(data = wb_epi_df, aes(x = x.vals, y = y.vals, label= iso3c,
                                      size = 0.02), 
                color = "black", force = 2, 
                segment.size = 0.2) +
    
    stat_dens2d_filter(data = wb_epi_df, aes(x = x.vals, y = y.vals, label= iso3c,
                                             size = 0.02), color = "black", 
                       position = "identity", 
                       segment.size = 1,
                       geom = "text_repel", keep.fraction = 0.53) +
## GGPLOT

    pdf(file = "ggplot_ge.pdf")
    ggplot(data = wb_epi_df, aes(x = x.vals, y = y.vals)) + 
           geom_point(aes(shape = Regime, color = Regime), 
                      size = 3, data = wb_epi_df) +
        scale_shape_manual(values = c(19, 1)) +
        scale_color_manual(values = c("black", "blue")) +
        geom_smooth(aes(x = x.vals, y = y.vals), method = "lm", se=T, color = "red") +
        scale_x_continuous(name = "Government Effectiveness | others") +
        scale_y_continuous(name = "EPI | others") +
        geom_text_repel(data = wb_epi_df, 
                        aes(x = x.vals, y = y.vals, label= iso3c)
                            , size = 3, color = "black", force = 2) +
        theme_classic() +
        theme(legend.position = "bottom",
                  legend.box = "vertical")
    dev.off()

### GGPLOT and RQ

lm.4 <- lm(EPI2018Score ~ RQ.EST  + lied_dummy + trade_log + 
               log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
           data = wb_epi_df)

out.x2 <- lm(RQ.EST ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$x.vals2 <- resid(out.x2)

out.y2 <- lm(EPI2018Score ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$y.vals2 <- resid(out.y2)

pdf(file = "ggplot_rq.pdf")
ggplot(data = wb_epi_df) + 
    geom_point(aes(x = x.vals2, y = y.vals2, colour = Regime), size = 2) +
    geom_smooth(aes(x = x.vals2, y = y.vals2), method = "lm", se=F, color = "red") +
    scale_x_continuous(name = "Regulatory Quality | others") +
    scale_y_continuous(name = "EPI | others") +
    geom_text_repel(data = wb_epi_df,  aes(x = x.vals2, y = y.vals2, label=iso3c), 
                    size = 3, color = "black", force = 2, 
                    segment.size = 1) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.box = "vertical")
dev.off()
    


### GGPLOT GDP and EPI

out.x1 <- lm(log(NY.GDP.PCAP.CD) ~ GE.EST + lied_dummy + trade_log + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$x.vals1 <- resid(out.x1)

out.y1 <- lm(EPI2018Score ~ lied_dummy + trade_log + 
                log(NY.GDP.PCAP.CD) + resource_rents_log + log(PopDensity), 
            data = wb_epi_df)

wb_epi_df$y.vals1 <- resid(out.y1)

ggplot(data = wb_epi_df) + 
    geom_point(aes(x = x.vals1, y = y.vals1,colour = lied_dummy2), size = 2) +
    geom_smooth(aes(x = x.vals1, y = y.vals1), method = "lm", se=F, color = "red") +
    scale_x_continuous(name = "GDP | others") +
    scale_y_continuous(name = "EPI | others") +
    geom_text_repel(data = wb_epi_df,  aes(x = x.vals1, y = y.vals1, label=iso3c), 
                    size = 3, color = "black", force = 2, 
                    segment.size = 1) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.box = "vertical")

