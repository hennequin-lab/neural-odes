open Printf
open Neural_odes.Typ

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [dir]")
let in_dir = sprintf "%s/%s" dir
let batch_size = Cmdargs.(get_int "-bs" |> default 1)
let eta = Cmdargs.(get_float "-eta" |> default 0.03)
let save_every = Cmdargs.(get_int "-save_every" |> default 100)

module D = Image.Make (struct
  let file = Cmdargs.(get_string "-img" |> force ~usage:"-img path")
end)

let _ = Mat.save_txt (D.sample_batch 1000) (in_dir "true_samples")

module F = Neural_odes.Rbf_flow.Make (struct
  let dim = 2
  let n_points = 50
  let sigma = 0.2
end)

(* forward flow *)
module BD = Neural_odes.Lib.Standard_gaussian_density (struct
  let dim = D.dim
  let sigma = 0.6
end)

module G =
  Neural_odes.Lib.Generative_model
    (struct
      let dim = D.dim
      let abs_tol = 1E-5
      let flow = F.flow
    end)
    (BD)

(* evaluate the density of end-points under the forward model *)
let save_density theta =
  Neural_odes.Planar.save_density ~res:40 (G.log_density ~theta) (in_dir "density")


(* generate data from the model *)
let test_gen theta =
  let samples = G.generate ~theta (`batch_of_size 1000) in
  Mat.save_txt samples (in_dir "samples")


let save_prms theta file =
  Mat.save_txt (AD.unpack_arr theta |> Mat.transpose) (in_dir file)


let theta =
  match Cmdargs.get_string "-restart_from" with
  | Some file -> AD.Arr Mat.(transpose (load_txt file))
  | None -> F.random_prms ()


(* Optimization of the flow *)

module Prms = struct
  type 'a t = { theta : 'a } [@@deriving prms]
end

let f a =
  let theta = a.Prms.theta in
  let minibatch = D.sample_batch batch_size in
  AD.Maths.neg (G.log_likelihood minibatch theta)


module O = Owl_opt.D.Adam.Make (Prms)

let prms0 = Prms.{ theta }
let lr = Owl_opt.Lr.(Fix (eta /. float batch_size))
let session = O.init ~prms0 ~f ()

let stop s =
  Gc.major ();
  let k = O.iter s in
  Printf.printf "\r iter %05i%!" k;
  if k mod save_every = 0
  then (
    let c = O.fv s in
    Printf.printf " -- %05f%!" c;
    Mat.save_txt ~append:true Mat.(create 1 1 c) (in_dir "cost");
    let theta = (O.prms s).theta in
    Mat.save_txt (AD.unpack_arr theta |> Mat.transpose) (in_dir "prms");
    test_gen theta;
    save_density theta);
  false


let best = O.min ~stop ~lr session
