open Owl
open Owl_ode
open Typ

module Make (P : Prms) = struct
  include P

  let solver = Owl_ode_sundials.cvode ~stiff:false ~relative_tol:0. ~abs_tol
  let tspec duration dt = Types.(T1 { t0 = 0.; duration; dt })

  (* assumes x0 and theta are row vectors *)
  let forward ?(duration = 1.) ?(dt = duration) ~z0 ~theta =
    let tspec = tspec duration dt in
    let batch_size = Mat.row_num z0 in
    let dx x t = AD.unpack_arr (flow ~theta (AD.pack_arr x) t) in
    let times, states = Ode.odeint solver dx z0 tspec () in
    let last =
      states |> Arr.get_slice [ [ -1 ] ] |> fun v -> Arr.reshape v [| batch_size; -1 |]
    in
    Gc.minor ();
    times, states, last


  let backward ?(duration = 1.) ~z1 ~adj1 ~theta =
    let tspec = tspec duration duration in
    let batch_size, n = Mat.shape z1 in
    let m, n_prms = AD.Mat.shape theta in
    assert (m = 1);
    assert (Mat.shape z1 = Mat.shape adj1);
    (* define augmented system to be run backwards in time *)
    let pack z adj grad = Mat.concatenate ~axis:1 [| z; adj; grad |] in
    let unpack s =
      let x = Mat.split ~axis:1 [| n; n; n_prms |] s in
      x.(0), x.(1), x.(2)
    in
    (* augmented initial state *)
    let s1 = pack z1 adj1 (Mat.zeros batch_size n_prms) in
    (* augmented flow *)
    let backward_flow s t =
      let t_ = duration -. t in
      let z, adj, _ = unpack s in
      let z_ = AD.pack_arr z in
      let adj_ = AD.pack_arr adj in
      let dz = AD.(unpack_arr (flow ~theta z_ t_)) in
      let da =
        AD.(Maths.neg (jacobianTv (fun z -> flow ~theta z t_) z_ adj_) |> unpack_arr)
      in
      let dg =
        Array.init batch_size (fun i ->
            let zi = AD.Maths.get_slice [ [ i ] ] z_ in
            let adji = AD.Maths.get_slice [ [ i ] ] adj_ in
            AD.(
              Maths.neg (jacobianTv (fun theta -> flow ~theta zi t_) theta adji)
              |> unpack_arr))
        |> Mat.concatenate ~axis:0
      in
      (* neg, because backwards in time *)
      Mat.neg (pack dz da dg)
    in
    let _, states = Ode.odeint solver backward_flow s1 tspec () in
    let s0 =
      states |> Arr.get_slice [ [ -1 ] ] |> fun v -> Arr.reshape v [| batch_size; -1 |]
    in
    let _, _, g = unpack s0 in
    Gc.minor ();
    Mat.sum ~axis:0 g


  (* make a certain (differentiable) function f(z1)
     a fully differentiable function of the parameters of the flow *)
  let make_diff ?duration z0 f =
    let open AD in
    let open AD.Maths in
    let open AD.Builder in
    build_siso
      (module struct
        let label = "ode"
        let ff_f _ = assert false
        let df _cp _ _ = assert false

        let ff_arr theta =
          let _, _, final = forward ?duration ?dt:None ~z0 ~theta:(pack_arr theta) in
          let z1 = pack_arr final in
          f z0 z1


        let dr theta _cp ca =
          let g =
            let _, _, z1 = forward ?duration ?dt:None ~z0 ~theta in
            let adj1 = (grad (f z0)) (pack_arr z1) |> unpack_arr in
            let g = backward ?duration ~z1 ~adj1 ~theta in
            pack_arr g
          in
          !ca * g
      end : Siso)
end

module Generative_model (P : Typ.Prms) (BD : Typ.Base_density) = struct
  include P

  let canonical_basis = Mat.eye dim |> Mat.map_rows (fun r -> r)
  let solver = Owl_ode_sundials.cvode ~stiff:false ~relative_tol:0. ~abs_tol
  let tspec duration = Types.(T1 { t0 = 0.; duration; dt = duration })

  let generate ?(duration = 1.) ~theta batch =
    let tspec = tspec duration in
    let dx x t = AD.unpack_arr (flow ~theta (AD.pack_arr x) t) in
    let z0 =
      match batch with
      | `batch_of_size s -> BD.sample s
      | `init x -> x
    in
    let batch_size, d = Mat.shape z0 in
    let _, states = Ode.odeint solver dx z0 tspec () in
    let states = if d = 1 then Mat.transpose states else states in
    states |> Arr.get_slice [ [ -1 ] ] |> fun v -> Arr.reshape v [| batch_size; -1 |]


  include Make (struct
    include P

    (* density flow *)
    let flow ?(duration = 1.) ~theta s t =
      let t_ = duration -. t in
      let open AD in
      let z = Maths.get_slice [ []; [ 0; pred dim ] ] s in
      let batch_size = (shape z).(0) in
      let x1 = P.flow ~theta z t_ in
      let x2 =
        Array.fold_left
          (fun accu ei ->
            let ei = Arr (Typ.Mat.repeat ei [| batch_size; 1 |]) in
            let jei =
              Maths.(sum ~axis:1 (ei * jacobianv (fun z -> P.flow ~theta z t_) z ei))
            in
            Maths.(accu + jei))
          (Arr (Typ.Mat.zeros batch_size 1))
          canonical_basis
      in
      Maths.(neg (concat ~axis:1 x1 x2))
  end)

  (* evaluate the density of end-points under the forward model *)
  let log_density ?duration ~theta z1 =
    let batch_size = Mat.row_num z1 in
    let s1 = Mat.concat_horizontal z1 Mat.(zeros batch_size 1) in
    let _, _, s0 = forward ?duration ?dt:None ~z0:s1 ~theta in
    let z0 = Mat.get_slice [ []; [ 0; dim - 1 ] ] s0 in
    let delta_logp = Mat.get_slice [ []; [ -1 ] ] s0 in
    let base = AD.unpack_arr (BD.log_base_density (AD.pack_arr z0)) in
    Mat.(delta_logp + base)


  (* data log likelihood as a differentiable function
     of the parameters of the flow *)
  let log_likelihood ?duration minibatch =
    let initial_conditions =
      Mat.concat_horizontal minibatch Mat.(zeros (row_num minibatch) 1)
    in
    let f _ s0 =
      let z0 = AD.Maths.get_slice [ []; [ 0; pred dim ] ] s0 in
      let delta_logp = AD.Maths.(get_slice [ []; [ -1 ] ] s0) in
      AD.Maths.(sum' (delta_logp + BD.log_base_density z0))
    in
    make_diff ?duration initial_conditions f
end

let log_gaussian_density' mu sigma =
  let dim = Mat.numel mu in
  let const = float dim *. log (Const.pi2 *. sigma) in
  fun z ->
    let d = Mat.(l2norm_sqr' (z - mu)) /. sigma /. sigma in
    (d -. const) /. 2.


module Standard_gaussian_density (P : sig
  val dim : int
  val sigma : float
end) =
struct
  let log_base_density =
    let const = AD.F (float P.dim *. log (Const.pi2 *. P.sigma)) in
    let const2 = AD.F (Maths.sqr P.sigma) in
    fun z ->
      let d = AD.Maths.(sum ~axis:1 (sqr z) / const2) in
      AD.Maths.((neg d - const) / F 2.)


  let sample batch_size = Mat.gaussian ~sigma:P.sigma batch_size P.dim
end
