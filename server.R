#Server:  This is the shiny set up

source("setup.R")

function(input, output, session) {

observeEvent( input$goButton,{
    selDate <- input$datesel
    OutMap <- genMap(selDate)

output$outmap <-  renderLeaflet({OutMap})

output$CHDATA=downloadHandler(
    filename= function(){
      paste0("Colorado Census Response Rate ",selDate,".docx")
    },
    content= function(file){
      print(genReport(selDate),target = file, fp_text(font.size = 10))
    }
  )

})
}