#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)

plotExps <- function(log2tpms_genes, genes, conds, condcol, factors, tag){
    
    Factor <- rep(factors[1],dim(conds)[1])
    Factor[c(conds[condcol] == factors[2])]<-factors[2]
    
    mltgg <- data.frame(0,0,0)
    mltgg <- mltgg[-1,]
    genenames <- mapgenes %>% dplyr::filter(ensembl_gene_id %in% genes) %>% dplyr::select(external_gene_name)
    genenames <- unlist(c(genenames))
    
    for(xx in 1:length(genes)){
        mltgg <- rbind(mltgg, cbind(rep(genenames[xx], length(Factor)),
                                    melt(log2tpms_genes[rownames(log2tpms_genes) %in% genes[xx],]),
                                    Factor))
    }
    
    colnames(mltgg) <- c("Gene","sample","value","Factor")
    mltgg$Factor <- factor(mltgg$Factor,
                           levels = factors, 
                           ordered = TRUE)
    ggp <- ggplot(data = mltgg, aes(x = Gene, y = value)) +
        geom_boxplot(aes(colour = Factor)) +
        geom_jitter(aes(colour = Factor), position=position_dodge(0.8)) +
        labs(y = "log2TPM", title = paste0(tag, ": Expression per Group per Gene")) +
        scale_colour_manual(values = c("blue", "red")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
return(ggp)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    count_se <- readRDS(url("https://github.com/brucemoran/ebrennan_haosmc/raw/master/inst/data/salmon.merged.gene_counts.rds"))
    log2tpm_tb <- tibble::as_tibble(count_se@assays@data$abundance, rownames = "ensembl_gene_id") %>%
                    dplyr::left_join(anno_tb, .) %>% 
                    dplyr::mutate(dplyr::across(where(is.numeric), round, 2)) %>%
                    dplyr::mutate(dplyr::across(where(is.numeric), log2)) %>%
                    dplyr::mutate(dplyr::across(where(is.numeric), round, 3))
    log2tpm_ninf_tb <- log2tpm_tb[!is.infinite(rowSums(log2tpm_tb[,3:dim(log2tpm_tb)[2]])),]
    
    trt <- readr::read_csv("https://raw.githubusercontent.com/brucemoran/ebrennan_haosmc/master/inst/data/trt.csv")
    
    dataset <- shiny::reactive(log2tpm_ninf_tb)
    
    output$genetable <- DT::renderDT({
        
        gene_tb <- dplyr::filter(.data = dataset(), 
                                 external_gene_name %in% !!input$gene_search)
      
    })  
       
    output$plotOut <- renderPlot({
    
        trt$group_drug <- paste0(trt$group, "_",  trt$drug)
        gene_tb <- dplyr::filter(.data = dataset(), 
                             external_gene_name %in% !!input$gene_search)
        
        tgene_df <- data.frame(value = t(gene_tb[,-c(1,2)]),
                           sampleID = rownames(t(gene_tb[,-c(1,2)]))) %>% dplyr::left_join(., trt)
        tgene_df$group_drug <- factor(tgene_df$group_drug,
                                  levels = unlist(lapply(c("Scramble_", "Let7d_"), function(f){
                                      paste0(f, c("None", "TNFa", "Ator", "Lova"))
                                  })))
        
        ggplot2::ggplot(data = tgene_df, ggplot2::aes(x = group_drug, y = value)) +
        ggplot2::geom_boxplot( ggplot2::aes(colour = group_drug)) +
        ggplot2::geom_jitter(position =  ggplot2::position_dodge(0.8)) +
        ggplot2::labs(y = "log2TPM", title = paste0(tag, ": Expr. per Group_Drug in ", input$gene_search)) +
        ggplot2::theme(axis.text.x =  ggplot2::element_text(angle = 45, hjust = 1))          
    })
})
