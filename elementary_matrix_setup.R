elementary_matrix <- function(o_type = 'i', dim = 1, coord = 1, value = 1) {
  #' o_type - recieves the kind of elementary matrix to be created (p, e, r) or i for Identity matrix - default value.
  #' dim - recieves the dimension of the matrix to be created (natural numbers)
  #' coord - recieves either a vector or a number in the form
  #'    for p, these coordinates indicate where the value will be applied
  #'    for e, these coordinates indicate the columns that will be swiched
  #'    for r, the number or the first coordinate of the vector indicate the line that will be multiplied by the value
  #' value - recieves a number that will be used in p and r operations
  #'
  #' in case of invalid input, the function will throw either an error or an Id matrix
  
  dim <- as.numeric(dim)
  coord <- as.array(coord)
  value <- as.numeric(value)
  mat <- rep(0, dim) %*% t(rep(0, dim)) # creats null matrix
  for (number in seq(1:dim)){ # changes the null matrix into a I matrix
    mat[number, number] = 1
  }
  
  if (o_type == 'p') {
    mat[coord[1], coord[2]] <- value
  } else if (o_type == 'e') {
    cola_tmp <- mat[,coord[2]]
    colb_tmp <- mat[,coord[1]]
    mat[,coord[1]] <- cola_tmp
    mat[,coord[2]] <- colb_tmp
  } else if (o_type == 'r') {
    mat[coord[1],] <- mat[coord[1],]*value
  } else if (o_type == 'i') {return(mat)}
  
  return(mat)
}