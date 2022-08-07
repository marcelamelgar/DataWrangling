generate_df <- function(x, tamanio){
  return(
    data.frame(
      a = sample(letters, size = tamanio, replace = TRUE),
      b = sample(1:10, size = tamanio, replace = TRUE)
    )
  )
}

lista <- lapply(1:4, generate_df, tamanio = 4)

merge