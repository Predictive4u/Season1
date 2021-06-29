rm(list=ls())

devtools::install_github("mattflor/chorddiag", 
                         build_vignettes = TRUE)
library(chorddiag)

m <- matrix(c(4627632, 287124, 252084, 713284,
              327066, 911996, 210372, 1022351,
              79346, 72944, 230949, 335746,
              143303, 227545, 140923, 6014907),
            byrow = TRUE,
            nrow = 4, ncol = 4)/1e+03
categ <- c("Emp. (f.)", "Emp. (i.)", "Unemp.", "Inactive")
dimnames(m) <- list("2020-I" = categ, "2020-II" = categ)
groupColors <- c("#5D78F5", "#51F5A0", "#F5412C", "#F5C946")

chorddiag(m, 
          groupColors = groupColors, 
          groupnamePadding = 35,
          groupPadding = 5,
          groupnameFontsize	=15,
          chordedgeColor = 1,
          tickInterval = 300
          )
