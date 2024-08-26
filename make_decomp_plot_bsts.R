library(scales)
library(patchwork)
library(bsts)


bsts_tot<-readRDS("bsts_tot.RDS")

# decomposition
pdf(file = "./vis/appxBSTS1.pdf")
plot(bsts_tot)
dev.off()
pdf(file = "./vis/appxBSTS2.pdf")
PlotBstsComponents(bsts_tot,
                       burn = SuggestBurn(.1, bsts_tot),
                       same.scale = F)
dev.off()
pdf(file = "./vis/appxBSTS3.pdf")
plot(bsts_tot,
     y = "residuals")
dev.off()

