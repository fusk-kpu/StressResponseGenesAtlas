# Menu : Microarray #########################################################

## Sub Menu : Atlas ####
### アトラスを読み込む ####
SRscore_microarray <- readRDS("data/microarray/SRscore_microarray.rds")
colnames_microarray <- gsub("\\..+", "", list.files("data/microarray/SRratio/"))
SRscore_microarray[colnames_microarray] <- round(SRscore_microarray[colnames_microarray])

### アトラスをヒートマップ化する時の色の設定 ####
paletteLength <- 100
microarrayBreaks <- c(seq(min(SRscore_microarray[colnames_microarray]), 0, length.out = (paletteLength/2)), 
              seq(0, max(SRscore_microarray[colnames_microarray]), length.out = (paletteLength/2))[-1])
microarrayColor <- colorRampPalette(c("deepskyblue", "white", "hotpink"))(paletteLength)

### 外部DBへのリンクをアトラスに追加する ####
# リンクの見た目設定
link <- rep("link", nrow(SRscore_microarray))

# 新規タブで開く（脆弱性対策付き）
blank <- "target = _blank rel = noopener noreferrer"

# AlphaFold2
url <- rep(paste0("https://alphafold.ebi.ac.uk/search/text/",
                  SRscore_microarray$ensembl_gene_id))
AF2 <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# ATTED-II
url <- rep(paste0("https://atted.jp/kwsearch/?stype=any&kword=", 
                  SRscore_microarray$ensembl_gene_id, 
                  "&searchBtnK.x=0&searchBtnK.y=0"))
ATTED2 <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# eFP Browser
url <- rep(paste0("https://bar.utoronto.ca/efp/cgi-bin/efpWeb.cgi?dataSource=Abiotic_Stress&mode=Absolute&primaryGene=",
                  SRscore_microarray$ensembl_gene_id))
eFP <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# KEGG
url <- rep(paste0("https://www.genome.jp/dbget-bin/www_bget?ath:", SRscore_microarray$ensembl_gene_id))
KEGG <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# STRING
url <- rep(paste0("https://string-db.org/cgi/network?identifiers=",
                  SRscore_microarray$ensembl_gene_id,
                  "&species=3702&show_query_node_labels=1"))
STRING <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# TAIR
url <- rep(paste0("https://www-arabidopsis-org.translate.goog/servlets/TairObject?type=locus&name=", 
                  SRscore_microarray$ensembl_gene_id, 
                  "&_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc"))
TAIR <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# ThaleMine
url <- rep(paste0("https://bar.utoronto.ca/thalemine/keywordSearchResults.do?searchTerm=",
                  SRscore_microarray$ensembl_gene_id, "&searchSubmit=GO"))
TM <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# それぞれのリンクをアトラスに結合 
SRscore_microarray <- cbind(SRscore_microarray, "AlphaFold2" = AF2, "ATTED-II" = ATTED2, "eFP Browser" = eFP, 
                         "KEGG" = KEGG, "STRING" = STRING, "TAIR" = TAIR, "Thale Mine" = TM)

### SRratioの読み込み ####
ABA_ratiom <- readRDS("data/microarray/SRratio/ABA.rds")
Cold_ratiom <- readRDS("data/microarray/SRratio/Cold.rds")
DC3000_ratiom <- readRDS("data/microarray/SRratio/DC3000.rds")
Drought_ratiom <- readRDS("data/microarray/SRratio/Drought.rds")
Heat_ratiom <- readRDS("data/microarray/SRratio/Heat.rds")
`High-light_ratiom` <- readRDS("data/microarray/SRratio/High-light.rds")
Hypoxia_ratiom2 <- readRDS("data/microarray/SRratio/Hypoxia.rds")
Osmotic_ratiom <- readRDS("data/microarray/SRratio/Osmotic.rds")
Oxidation_ratiom <- readRDS("data/microarray/SRratio/Oxidation.rds")
Salt_ratiom <- readRDS("data/microarray/SRratio/Salt.rds")
Wound_ratiom <- readRDS("data/microarray/SRratio/Wound.rds")

### メタデータの読み込み ####
ABA_metadatam <- readRDS("data/microarray/Metadata/ABA.rds")
Cold_metadatam <- readRDS("data/microarray/Metadata/Cold.rds")
DC3000_metadatam <- readRDS("data/microarray/Metadata/DC3000.rds")
Drought_metadatam <- readRDS("data/microarray/Metadata/Drought.rds")
Heat_metadatam <- readRDS("data/microarray/Metadata/Heat.rds")
`High-light_metadatam` <- readRDS("data/microarray/Metadata/High-light.rds")
Hypoxia_metadatam <- readRDS("data/microarray/Metadata/Hypoxia.rds")
Osmotic_metadatam <- readRDS("data/microarray/Metadata/Osmotic.rds")
Oxidation_metadatam <- readRDS("data/microarray/Metadata/Oxidation.rds")
Salt_metadatam <- readRDS("data/microarray/Metadata/Salt.rds")
Wound_metadatam <- readRDS("data/microarray/Metadata/Wound.rds")

## Sub Menu : Template Matching ####
### アトラスをgenefinder()に入力可能な形式に変換 ####
genefinder_microarray <- column_to_rownames(SRscore_microarray, var = "ensembl_gene_id")
genefinder_microarray <- genefinder_microarray[colnames_microarray]
genefinder_microarray <- as.matrix(genefinder_microarray)

# データ集計
list_microarray <- list(ABA_metadatam, 
                        Cold_metadatam,
                        DC3000_metadatam,
                        Drought_metadatam,
                        Heat_metadatam,
                        `High-light_metadatam`,
                        Hypoxia_metadatam,
                        Osmotic_metadatam,
                        Oxidation_metadatam,
                        Salt_metadatam,
                        Wound_metadatam
)
## 対象サンプルの集計
numof_control_ma <- list_microarray %>%
  lapply("[[", "control_sample") %>%
  lapply(na.omit) %>%
  lapply(length)
numof_control_ma <- unlist(numof_control_ma)
## 実験サンプルの集計
numof_treatment_ma <- list_microarray %>%
  lapply("[[", "treated_sample") %>%
  lapply(na.omit) %>%
  lapply(length)
numof_treatment_ma <- unlist(numof_treatment_ma)
## 研究プロジェクトの集計
numof_series_ma <- list_microarray %>%
  lapply("[[", "Series") %>%
  lapply(unique) %>%
  lapply(length)
numof_series_ma <- unlist(numof_series_ma)
## 集計カテゴリ行とストレス列からなるデータフレームを作成する
collections_ma <- rbind(numof_control_ma, numof_treatment_ma, numof_series_ma)
collections_ma <- as.data.frame(collections_ma)
colnames(collections_ma) <- colnames_microarray

# RNA-Seq #########################################################

## Atlas ####
### アトラスを読み込む ####
SRscore_rnaseq <- readRDS("data/rnaseq/SRscore_rnaseq.rds")
colnames_rnaseq <- gsub("\\..+", "", list.files("data/rnaseq/SRratio/"))
SRscore_rnaseq[colnames_rnaseq] <- round(SRscore_rnaseq[colnames_rnaseq])

### アトラスをヒートマップ化する時の色の設定 ####
paletteLength <- 100
rnaseqBreaks <- c(seq(min(SRscore_rnaseq[colnames_rnaseq]), 0, length.out = (paletteLength/2)), 
                  seq(0, max(SRscore_rnaseq[colnames_rnaseq]), length.out = (paletteLength/2))[-1])
rnaseqColor <- colorRampPalette(c("deepskyblue", "white", "hotpink"))(paletteLength)

### 外部DBへのリンクをアトラスに追加する ####
# リンクの見た目設定
link <- rep("link", nrow(SRscore_rnaseq))

# AlphaFold2
url <- rep(paste0("https://alphafold.ebi.ac.uk/search/text/",
                  SRscore_rnaseq$ensembl_gene_id))
AF2 <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# ATTED-II
url <- rep(paste0("https://atted.jp/kwsearch/?stype=any&kword=", 
                  SRscore_rnaseq$ensembl_gene_id, 
                  "&searchBtnK.x=0&searchBtnK.y=0"))
ATTED2 <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# eFP Browser
url <- rep(paste0("https://bar.utoronto.ca/efp/cgi-bin/efpWeb.cgi?dataSource=Abiotic_Stress&mode=Absolute&primaryGene=",
                  SRscore_rnaseq$ensembl_gene_id))
eFP <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# KEGG
url <- rep(paste0("https://www.genome.jp/dbget-bin/www_bget?ath:", SRscore_rnaseq$ensembl_gene_id))
KEGG <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# STRING
url <- rep(paste0("https://string-db.org/cgi/network?identifiers=",
                  SRscore_rnaseq$ensembl_gene_id,
                  "&species=3702&show_query_node_labels=1"))
STRING <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# TAIR
url <- rep(paste0("https://www-arabidopsis-org.translate.goog/servlets/TairObject?type=locus&name=", 
                  SRscore_rnaseq$ensembl_gene_id, 
                  "&_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc"))
TAIR <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# ThaleMine
url <- rep(paste0("https://bar.utoronto.ca/thalemine/keywordSearchResults.do?searchTerm=",
                  SRscore_rnaseq$ensembl_gene_id, "&searchSubmit=GO"))
TM <- paste0("<a href = ", "'", url, "'", blank, ">", link, "</a>")

# 各リンクをアトラスへ結合 
SRscore_rnaseq <- cbind(SRscore_rnaseq, "AlphaFold2" = AF2, "ATTED-II" = ATTED2, "eFP Browser" = eFP,
                     "KEGG" = KEGG, "STRING" = STRING, "TAIR" = TAIR, "Thale Mine" = TM)

### SRratioの読み込み ####
ABA_ratior <- readRDS("data/rnaseq/SRratio/ABA.rds")
Cold_ratior <- readRDS("data/rnaseq/SRratio/Cold.rds")
DC3000_ratior <- readRDS("data/rnaseq/SRratio/DC3000.rds")
Drought_ratior <- readRDS("data/rnaseq/SRratio/Drought.rds")
Heat_ratior <- readRDS("data/rnaseq/SRratio/Heat.rds")
`High-light_ratior` <- readRDS("data/rnaseq/SRratio/High-light.rds")
Hypoxia_ratior <- readRDS("data/rnaseq/SRratio/Hypoxia.rds")
Osmotic_ratior <- readRDS("data/rnaseq/SRratio/Osmotic.rds")
Oxidation_ratior <- readRDS("data/rnaseq/SRratio/Oxidation.rds")
Salt_ratior <- readRDS("data/rnaseq/SRratio/Salt.rds")
Wound_ratior <- readRDS("data/rnaseq/SRratio/Wound.rds")

### メタデータの読み込み ####
ABA_metadatar <- readRDS("data/rnaseq/Metadata/ABA.rds")
Cold_metadatar <- readRDS("data/rnaseq/Metadata/Cold.rds")
DC3000_metadatar <- readRDS("data/rnaseq/Metadata/DC3000.rds")
Drought_metadatar <- readRDS("data/rnaseq/Metadata/Drought.rds")
Heat_metadatar <- readRDS("data/rnaseq/Metadata/Heat.rds")
`High-light_metadatar` <- readRDS("data/rnaseq/Metadata/High-light.rds")
Hypoxia_metadatar <- readRDS("data/rnaseq/Metadata/Hypoxia.rds")
Osmotic_metadatar <- readRDS("data/rnaseq/Metadata/Osmotic.rds")
Oxidation_metadatar <- readRDS("data/rnaseq/Metadata/Oxidation.rds")
Salt_metadatar <- readRDS("data/rnaseq/Metadata/Salt.rds")
Wound_metadatar <- readRDS("data/rnaseq/Metadata/Wound.rds")

## Sub Menu : Template Matching ####
### アトラスをgenefinder()に入力可能な形式に変換 ####
genefinder_rnaseq <- column_to_rownames(SRscore_rnaseq, var = "ensembl_gene_id")
genefinder_rnaseq <- genefinder_rnaseq[colnames_rnaseq]
genefinder_rnaseq <- as.matrix(genefinder_rnaseq)

# データ集計
list_rnaseq <- list(ABA_metadatar, 
                    Cold_metadatar,
                    DC3000_metadatar,
                    Drought_metadatar,
                    Heat_metadatar,
                    `High-light_metadatar`,
                    Hypoxia_metadatar,
                    Osmotic_metadatar,
                    Oxidation_metadatar,
                    Salt_metadatar,
                    Wound_metadatar
)
## 対象サンプルの集計
numof_control_rs <- list_rnaseq %>%
  lapply("[[", "control_sample") %>%
  lapply(na.omit) %>%
  lapply(length)
numof_control_rs <- unlist(numof_control_rs)
## 実験サンプルの集計
numof_treatment_rs <- list_rnaseq %>%
  lapply("[[", "treated_sample") %>%
  lapply(na.omit) %>%
  lapply(length)
numof_treatment_rs <- unlist(numof_treatment_rs)
## 研究プロジェクトの集計
numof_series_rs <- list_rnaseq %>%
  lapply("[[", "Series") %>%
  lapply(unique) %>%
  lapply(length)
numof_series_rs <- unlist(numof_series_rs)
## 集計カテゴリ行とストレス列からなるデータフレームを作成する
collections_rs <- rbind(numof_control_rs, numof_treatment_rs, numof_series_rs)
collections_rs <- as.data.frame(collections_rs)
colnames(collections_rs) <- colnames_rnaseq

CollectionsInfo <- function(stress) {
  HTML(paste0(
    "<h4> # of control samples : ", stress[1],
    "<br><br>",
    "# of treatment samples : ", stress[2],
    "<br><br>",
    "# of GSE : ", stress[3],
    "<br></h3>"
  ))
}

# GMTファイルを読み込む
positiveSRscore_ma <- read.gmt("gmt/microarray/positiveSRscore_ma.gmt")
negativeSRscore_ma <- read.gmt("gmt/microarray/negativeSRscore_ma.gmt")

positiveSRscore_rs <- read.gmt("gmt/rnaseq/positiveSRscore_rs.gmt")
negativeSRscore_rs <- read.gmt("gmt/rnaseq/negativeSRscore_rs.gmt")

gokegg <- read.gmt("gmt/Ara_kegg_go.gmt")

positiveSRscore_ma <- rbind(positiveSRscore_ma, gokegg)
negativeSRscore_ma <- rbind(negativeSRscore_ma, gokegg)
nonzeroSRscore_ma <- rbind(positiveSRscore_ma, negativeSRscore_ma, gokegg)

positiveSRscore_rs <- rbind(positiveSRscore_rs, gokegg)
negativeSRscore_rs <- rbind(negativeSRscore_rs, gokegg)
nonzeroSRscore_rs <- rbind(positiveSRscore_rs, negativeSRscore_rs, gokegg)