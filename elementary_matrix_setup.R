null_matrix <- function(dim = 1){
    dim <- as.integer(dim)
    return(rep(0, dim) %*% t(rep(0, dim)))
}

ones_matrix <- function(dim = 1){
    dim <- as.integer(dim)
    return(rep(1, dim) %*% t(rep(1, dim)))
}

id_matrix <- function(dim = 1){
    dim <- as.integer(dim)
    mat <- null_matrix(dim)
    for (i in (1:dim)){
        mat[i, i] = 1
    }
    return(mat)
}

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
  
  dim <- as.integer(dim)
  coord <- as.array(coord)
  value <- as.numeric(value)
  mat <- id_matrix(dim)
    
  if (o_type == 'i') {
  } else if (o_type == 'p') {
    mat[coord[1], coord[2]] <- value
  } else if (o_type == 'e') {
    cola_tmp <- mat[,coord[2]]
    colb_tmp <- mat[,coord[1]]
    mat[,coord[1]] <- cola_tmp
    mat[,coord[2]] <- colb_tmp
  } else if (o_type == 'r') {
    mat[coord[1],] <- mat[coord[1],]*value} 
  
  return(mat)
}