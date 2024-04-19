type vector = float array
type matrix = vector array

(* Prints a matrix to stdout *)
val print_matrix : matrix -> unit

(* Computes the Singular Value Decomposition of a matrix using the standard method *)
val svd : matrix -> matrix * matrix * matrix

(* Computes the Singular Value Decomposition of a matrix using parallel processing *)
val svd_parallel : Domainslib.Task.pool -> matrix -> matrix * matrix * matrix
