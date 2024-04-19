open Domainslib

let dot_product v1 v2 =
  Array.fold_left2 (fun acc x y -> acc +. x *. y) 0.0 v1 v2

let vector_subtract v1 v2 =
  Array.map2 (fun x y -> x -. y) v1 v2

let scalar_multiply scalar v =
  Array.map (fun x -> scalar *. x) v

let vector_norm v =
  sqrt (dot_product v v)

let normalize v =
  scalar_multiply (1.0 /. vector_norm v) v

(* Parallel Gram-Schmidt Orthogonalization *)
let gram_schmidt_parallel pool a =
  let m, n = Array.length a, Array.length a.(0) in
  let q = Array.make_matrix m n 0.0 in
  let r = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    let ai = Array.init m (fun j -> a.(j).(i)) in
    let u = ref ai in
    for j = 0 to i - 1 do
      let qj = Array.init m (fun k -> q.(k).(j)) in
      let rij = Task.run pool (fun () -> dot_product ai qj) in
      r.(j).(i) <- rij;
      let projected = Task.run pool (fun () -> scalar_multiply rij qj) in
      u := Task.run pool (fun () -> vector_subtract !u projected)
    done;
    let ui_norm = Task.run pool (fun () -> vector_norm !u) in
    r.(i).(i) <- ui_norm;
    let qi = Task.run pool (fun () -> normalize !u) in
    Array.iteri (fun j _ -> q.(j).(i) <- qi.(j)) qi
  done;
  q, r

let print_matrix mat =
  Array.iter
    (fun row ->
      Array.iter (fun elem -> Printf.printf "%.2f " elem) row;
      print_newline ())
    mat

let normal () =
  let a = [|[|2.0; 2.0; 1.0|]; [|3.0; 4.0; 1.0|]|] in
  let q, r = gram_schmidt a in
  print_matrix q;
  print_newline ();
  print_matrix r

let parallel () =
  let n_domains = 8  (* Adjust based on your machine's cores *)
  let a = [|[|2.0; 2.0; 1.0|]; [|3.0; 4.0; 1.0|]|] in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let q, r = Task.run pool (fun () -> gram_schmidt_parallel pool a) in
  Task.teardown_pool pool;
  print_matrix q;
  print_newline ();
  print_matrix r
