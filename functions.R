top_row_fill <- function(questions){
    for(i in seq_along(questions[1,])){
        if(is.na(questions[1,i])) questions[1,i] <- previous
        previous <- questions[1,i]
    } 
    return(questions)
}
