library(shiny)
library(randomForest)
library(tidyverse)

ui <- fluidPage(
    # Application title
    titlePanel("Mitopredictor"),
    # Sequence input
    textInput("sequence", "Enter Peptide Sequence", "MADVSERTLQLSVLVAFASGVLLGWQANRLRRRYLDWRKRRLQDKLAATQKKLDLA"),
        mainPanel(
           helpText("Prediction:"),    
           textOutput("predict")
        )
    )

server <- function(input, output) {
    # load model
    load("modelrf.RData")
    # import rich_mito
    rich_mito <- read_csv("rich_mito.csv")

prediction <- reactive({
    Gene1 <- c("Your Protein")
    Sequence <- c(input$sequence)
    micro <- data.frame(Gene1, Sequence)
    micro %>% 
        mutate(Length = str_count(Sequence)) -> micro
    micro %>% 
        mutate(Glycine = str_count(Sequence, "G")/Length*100,
               Alanine = str_count(Sequence, "A")/Length*100,
               Valine = str_count(Sequence, "V")/Length*100,
               Leucine = str_count(Sequence, "L")/Length*100,
               Isoleucine = str_count(Sequence, "I")/Length*100,
               Methionine = str_count(Sequence, "M")/Length*100,
               Phenylalanine = str_count(Sequence, "F")/Length*100,
               Tryptophan = str_count(Sequence, "W")/Length*100,
               Proline = str_count(Sequence, "P")/Length*100,
               Serine = str_count(Sequence, "S")/Length*100,
               Threonine = str_count(Sequence, "T")/Length*100,
               Cysteine = str_count(Sequence, "C")/Length*100,
               Tyrosine = str_count(Sequence, "Y")/Length*100,
               Asparagine = str_count(Sequence, "N")/Length*100,
               Glutamine = str_count(Sequence, "Q")/Length*100,
               Aspartic_Acid = str_count(Sequence, "D")/Length*100,
               Glutamic_Acid = str_count(Sequence, "E")/Length*100,
               Lysine = str_count(Sequence, "K")/Length*100,
               Arginine = str_count(Sequence, "R")/Length*100,
               Histidine = str_count(Sequence, "H")/Length*100) -> micro 
    
    full_join(rich_mito, micro) %>% 
        filter(Gene1 == "Your Protein") -> micro.test
    
    rf.predict <- predict(modelrf, micro.test)
    rf.predict 
})

    output$predict <- renderText({
        ifelse(prediction()[[1]] == 1, "Your protein is in the mitochondrion", "Your protein is not in the mitochondrion")    
})
}

# Run the application 
shinyApp(ui = ui, server = server)
