open Printf
open Owl
open Neural_odes.Typ

let dir = Cmdargs.(get_string "-d" |> force ~usage:"-d [dir]")
let in_dir = sprintf "%s/%s" dir

module M = Neural_odes.Lib.Make (struct
  let dim = 1
  let abs_tol = 1E-6
  let flow ?duration:_ ~theta x _t = Algodiff.D.Maths.(neg (theta * x))
end)

let duration = 1.
let z0 = Mat.ones 1 1
let z1_desired = Mat.create 1 1 (exp (-2. *. duration))

(* if you want to optimise a function of the final state z1,
   as a function of the parameters of the flow (and given a certain z0), 
   use `make_diff` to define it and make it differentiable automatically
   ie. adding it to the set of operations that Owl knows how to differentiate) *)
let f =
  M.make_diff ~duration z0 (fun _z0 z1 ->
      let open Algodiff.D in
      Maths.(l2norm_sqr' (z1 - Arr z1_desired)))


(* standard optimization code from here *)

module Prms = struct
  type 'a t = { theta : 'a } [@@deriving prms]
end

module O = Owl_opt_lbfgs.Make (Prms)

let cost prms = f Prms.(prms.theta)

let opt_prms =
  let prms0 = Prms.{ theta = AD.Mat.zeros 1 1 } in
  let s0 = O.init ~prms0 ~f:cost () in
  let stop s = O.iter s > 1000 in
  let s = O.min ~pgtol:0. ~stop s0 in
  (O.prms s).theta |> AD.unpack_arr |> Mat.sum'


let _ = Printf.printf "solution = %f\n%!" opt_prms
