library(FactoMineR)
library(factoextra)
library(corrplot)

data <- read.table(file="clipboard",quote="", sep= "\t", header=TRUE)
data$Types <- as.factor(data$Types)
data.active<-data[,2:16]

cor.mat <- round(cor(data.active),2)
corrplot(cor.mat, type="upper", order="hclust",
         tl.col="black", tl.srt=45)

data.pca <- PCA(data, quali.sup = 1)
## plot of the eigenvalues
## barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(data.pca)
plot(data.pca,choix="ind",habillage=1)
## Not run:
## To describe the dimensions
dimdesc(data.pca, axes = 1:2)

## To draw ellipses around the categories of the 13th variable (which is categorical)
plotellipses(data.pca,1)

fviz_screeplot(data.pca, ncp=10)

fviz_pca_ind(data.pca,label="none", habillage=1, geom ="point",
             pointsize = 3) # Adjust the size as needed

fviz_pca_biplot(data.pca,
                habillage = 1,
                addEllipses = FALSE,
                col.var = "cornflowerblue",
                alpha.var ="cos2",
                label = "var",
                pointsize = 3,
                mean.point=FALSE) +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

# Define the custom colors and shapes
custom_colors <- c("gold3", "#7570b3","orange2")  # Example colors
custom_shapes <- c(16,17,18)  # Example shapes: circle, triangle, square, diamond

# PCA Biplot with manually defined colors and shapes
fviz_pca_biplot(data.pca,
                habillage = 1,
                addEllipses = FALSE,  # Set to TRUE if you want to add ellipses
                col.var = "cornflowerblue",
                alpha.var = "cos2",
                label = "var",
                pointsize = 3,
                mean.point = FALSE) +
  scale_color_manual(values = custom_colors) +  # Manually set the colors for groups
  scale_shape_manual(values = custom_shapes) +  # Manually set the shapes for groups
  theme_minimal(base_size = 20)  # Increase the base font size for better readability

# Manual shapes, define as many as you need, recycling if necessary
shapes <- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 30)

p <- ggplot(data, aes(x = PC1, y = PC5, color = Types)) +
  geom_point(size = 4) +
  labs(x = "PC1", y = "PC3") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = shapes) + # Use manual shapes
  theme_minimal(base_size = 20) #+  # Increase the base font size globally
  # coord_cartesian(xlim = c(0, 60000), ylim = c(15.3, 16.0))
p

