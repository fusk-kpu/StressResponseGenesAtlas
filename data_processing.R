## ======================================================== ##
## Menu : Microarray
## ======================================================== ##

# Loading data ####
load("data/microarray/SRscore_microarray.RData")
colnames_microarray <- colnames(Filter(is.numeric, SRscore_microarray))
SRscore_microarray[colnames_microarray] <- round(SRscore_microarray[colnames_microarray])

load("data/microarray/SRratio/ABA.RData")
load("data/microarray/SRratio/cold.RData")
load("data/microarray/SRratio/DC3000.RData")
load("data/microarray/SRratio/drought.RData")
load("data/microarray/SRratio/heat.RData")
load("data/microarray/SRratio/highlight.RData")
load("data/microarray/SRratio/hypoxia.RData")
load("data/microarray/SRratio/osmotic.RData")
load("data/microarray/SRratio/oxidation.RData")
load("data/microarray/SRratio/salt.RData")
load("data/microarray/SRratio/wound.RData")

load("data/microarray/Metadata/ABA.RData")
load("data/microarray/Metadata/cold.RData")
load("data/microarray/Metadata/DC3000.RData")
load("data/microarray/Metadata/drought.RData")
load("data/microarray/Metadata/heat.RData")
load("data/microarray/Metadata/highlight.RData")
load("data/microarray/Metadata/hypoxia.RData")
load("data/microarray/Metadata/osmotic.RData")
load("data/microarray/Metadata/oxidation.RData")
load("data/microarray/Metadata/salt.RData")
load("data/microarray/Metadata/wound.RData")

# Color settings of a heatmap ####
paletteLength <- 100
microarrayBreaks <- c(seq(min(Filter(is.numeric, SRscore_microarray)), 0, length.out = (paletteLength/2)), 
              seq(0, max(Filter(is.numeric, SRscore_microarray)), length.out = (paletteLength/2))[-1])
microarrayColor <- colorRampPalette(c("deepskyblue", "white", "hotpink"))(paletteLength)

# Links to external databases ####
link <- rep("link", nrow(SRscore_microarray))

## AlphaFold2
url <- rep(paste0("https://alphafold.ebi.ac.uk/search/text/",
                  SRscore_microarray$ensembl_gene_id))
AF2 <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## ATTED-II
url <- rep(paste0("https://atted.jp/kwsearch/?stype=any&kword=", 
                  SRscore_microarray$ensembl_gene_id, 
                  "&searchBtnK.x=0&searchBtnK.y=0"))
ATTED2 <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## eFP Browser
url <- rep(paste0("https://bar.utoronto.ca/efp/cgi-bin/efpWeb.cgi?dataSource=Abiotic_Stress&mode=Absolute&primaryGene=",
                  SRscore_microarray$ensembl_gene_id))
eFP <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## KEGG
url <- rep(paste0("https://www.genome.jp/dbget-bin/www_bget?ath:", SRscore_microarray$ensembl_gene_id))
KEGG <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## STRING
url <- rep(paste0("https://string-db.org/cgi/network?identifiers=",
                  SRscore_microarray$ensembl_gene_id,
                  "&species=3702&show_query_node_labels=1"))
STRING <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## TAIR
url <- rep(paste0("https://www-arabidopsis-org.translate.goog/servlets/TairObject?type=locus&name=", 
                  SRscore_microarray$ensembl_gene_id, 
                  "&_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc"))
TAIR <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## ThaleMine
url <- rep(paste0("https://bar.utoronto.ca/thalemine/keywordSearchResults.do?searchTerm=",
                  SRscore_microarray$ensembl_gene_id, "&searchSubmit=GO"))
TM <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## Joining links to the atlas
SRscore_microarray <- cbind(SRscore_microarray, "AlphaFold2" = AF2, "ATTED-II" = ATTED2, "eFP Browser" = eFP, 
                         "KEGG" = KEGG, "STRING" = STRING, "TAIR" = TAIR, "Thale Mine" = TM)

## Convert the atlas to template matching format 
genefinder_microarray <- column_to_rownames(SRscore_microarray, var = "ensembl_gene_id")
genefinder_microarray <- Filter(is.numeric, genefinder_microarray)
genefinder_microarray <- as.matrix(genefinder_microarray)


## ========================================================= ##
## Menu : RNA-Seq
## ========================================================= ##

# Loading data ####
load("data/rnaseq/SRscore_rnaseq.RData")
colnames_rnaseq <- colnames(Filter(is.numeric, SRscore_rnaseq))
SRscore_rnaseq[colnames_rnaseq] <- round(SRscore_rnaseq[colnames_rnaseq])

load("data/rnaseq/SRratio/ABA.RData")
load("data/rnaseq/SRratio/cold.RData")
load("data/rnaseq/SRratio/DC3000.RData")
load("data/rnaseq/SRratio/drought.RData")
load("data/rnaseq/SRratio/heat.RData")
load("data/rnaseq/SRratio/highlight.RData")
load("data/rnaseq/SRratio/hypoxia.RData")
load("data/rnaseq/SRratio/osmotic.RData")
load("data/rnaseq/SRratio/oxidation.RData")
load("data/rnaseq/SRratio/salt.RData")
load("data/rnaseq/SRratio/wound.RData")

load("data/rnaseq/Metadata/ABA.RData")
load("data/rnaseq/Metadata/cold.RData")
load("data/rnaseq/Metadata/DC3000.RData")
load("data/rnaseq/Metadata/drought.RData")
load("data/rnaseq/Metadata/heat.RData")
load("data/rnaseq/Metadata/highlight.RData")
load("data/rnaseq/Metadata/hypoxia.RData")
load("data/rnaseq/Metadata/osmotic.RData")
load("data/rnaseq/Metadata/oxidation.RData")
load("data/rnaseq/Metadata/salt.RData")
load("data/rnaseq/Metadata/wound.RData")

# Color settings of a heatmap ####
paletteLength <- 100
rnaseqBreaks <- c(seq(min(Filter(is.numeric, SRscore_rnaseq)), 0, length.out = (paletteLength/2)), 
                  seq(0, max(Filter(is.numeric, SRscore_rnaseq)), length.out = (paletteLength/2))[-1])
rnaseqColor <- colorRampPalette(c("deepskyblue", "white", "hotpink"))(paletteLength)

# Links to external databases ####
link <- rep("link", nrow(SRscore_rnaseq))

## AlphaFold2
url <- rep(paste0("https://alphafold.ebi.ac.uk/search/text/",
                  SRscore_rnaseq$ensembl_gene_id))
AF2 <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## ATTED-II
url <- rep(paste0("https://atted.jp/kwsearch/?stype=any&kword=", 
                  SRscore_rnaseq$ensembl_gene_id, 
                  "&searchBtnK.x=0&searchBtnK.y=0"))
ATTED2 <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## eFP Browser
url <- rep(paste0("https://bar.utoronto.ca/efp/cgi-bin/efpWeb.cgi?dataSource=Abiotic_Stress&mode=Absolute&primaryGene=",
                  SRscore_rnaseq$ensembl_gene_id))
eFP <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## KEGG
url <- rep(paste0("https://www.genome.jp/dbget-bin/www_bget?ath:", SRscore_rnaseq$ensembl_gene_id))
KEGG <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## STRING
url <- rep(paste0("https://string-db.org/cgi/network?identifiers=",
                  SRscore_rnaseq$ensembl_gene_id,
                  "&species=3702&show_query_node_labels=1"))
STRING <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## TAIR
url <- rep(paste0("https://www-arabidopsis-org.translate.goog/servlets/TairObject?type=locus&name=", 
                  SRscore_rnaseq$ensembl_gene_id, 
                  "&_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc"))
TAIR <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

## ThaleMine
url <- rep(paste0("https://bar.utoronto.ca/thalemine/keywordSearchResults.do?searchTerm=",
                  SRscore_rnaseq$ensembl_gene_id, "&searchSubmit=GO"))
TM <- paste0("<a href = ", "'", url, "'", ">", link, "</a>")

# Joining links to the atlas
SRscore_rnaseq <- cbind(SRscore_rnaseq, "AlphaFold2" = AF2, "ATTED-II" = ATTED2, "eFP Browser" = eFP,
                     "KEGG" = KEGG, "STRING" = STRING, "TAIR" = TAIR, "Thale Mine" = TM)

# Convert the atlas to template matching format 
genefinder_rnaseq <- column_to_rownames(SRscore_rnaseq, var = "ensembl_gene_id")
genefinder_rnaseq <- Filter(is.numeric, genefinder_rnaseq)
genefinder_rnaseq <- as.matrix(genefinder_rnaseq)
