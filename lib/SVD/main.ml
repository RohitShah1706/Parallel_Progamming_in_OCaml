open Domainslib
(* QR Decomposition from QR_decomposition module, includes normal and parallel *)
open QR_decomposition
(* Define vector and matrix types *)
type vector = float array
type matrix = vector array

(* Function to create a zero matrix *)
let zero_matrix rows cols =
  Array.make_matrix rows cols 0.0

(* Function to copy a matrix *)
let copy_matrix m =
  Array.map Array.copy m

(* Function to print a matrix for debugging *)
let print_matrix m =
  Array.iter (fun row ->
    Array.iter (fun element ->
      Printf.printf "%.2f " element
    ) row;
    print_endline "")
    m

(* Matrix multiplication *)
let matrix_mult a b =
  let a_rows = Array.length a in
  let b_cols = Array.length b.(0) in
  let b_rows = Array.length b in
  Array.init a_rows (fun i ->
    Array.init b_cols (fun j ->
      let sum = ref 0.0 in
      for k = 0 to b_rows - 1 do
        sum := !sum +. a.(i).(k) *. b.(k).(j)
      done;
      !sum))

(* Parallel Matrix Multiplication *)
let matrix_mult_parallel pool a b =
  let a_rows = Array.length a in
  let b_cols = Array.length b.(0) in
  let b_rows = Array.length b in
  Task.parallel_for_reduce pool ~init:(Array.make_matrix a_rows b_cols 0.0)
    ~body:(fun i acc ->
      let row = Array.init b_cols (fun j ->
        let sum = ref 0.0 in
        for k = 0 to b_rows - 1 do
          sum := !sum +. a.(i).(k) *. b.(k).(j)
        done;
        !sum)
      in
      acc.(i) <- row;
      acc)
    ~reducer:(fun _ _ -> assert false)



(* Singular Value Decomposition - Normal *)
let svd a =
  let at = Array.init (Array.length a.(0)) (fun i ->
              Array.init (Array.length a) (fun j -> a.(j).(i))) in
  let ata = matrix_mult at a in
  let q, _ = qr_decomposition ata in
  let u, s, v_t = q, ata, q in
  (u, s, v_t)

(* Singular Value Decomposition - Parallel *)
let svd_parallel pool a =
  let at = Array.init (Array.length a.(0)) (fun i ->
              Array.init (Array.length a) (fun j -> a.(j).(i))) in
  let ata = matrix_mult_parallel pool at a in
  let q, _ = qr_decomposition_parallel pool ata in
  let u, s, v_t = q, ata, q in
  (u, s, v_t)

(* Example usage *)
let () =
  let a = [|[|1.0; 2.0|]; [|3.0; 4.0|]|] in

  (* Normal SVD computation *)
  let u, s, v_t = svd a in
  print_endline "Normal SVD Computation:";
  print_endline "Matrix U (left singular vectors):";
  print_matrix u;
  print_endline "Matrix S (singular values, simplified as A^T * A):";
  print_matrix s;
  print_endline "Matrix V^T (right singular vectors transposed, simplified):";
  print_matrix v_t;

  (* Parallel SVD computation *)
  let n_domains = 8
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) ()
  let u_p, s_p, v_t_p = svd_parallel pool a in
  print_endline "Parallel SVD Computation:";
  print_endline "Matrix U (left singular vectors):";
  print_matrix u_p;
  print_endline "Matrix S (singular values, simplified as A^T * A):";
  print_matrix s_p;
  print_endline "Matrix V^T (right singular vectors transposed, simplified):";
  print_matrix v_t_p;
  Task.teardown_pool pool
