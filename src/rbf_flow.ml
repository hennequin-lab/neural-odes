open Typ

module Make (P : sig
  val dim : int
  val n_points : int
  val sigma : float
end) =
struct
  open P

  let spl = Array.make n_points 1

  let unpack_prms theta =
    let open AD in
    let mus = Maths.get_slice [ []; [ 0; pred (n_points * dim) ] ] theta in
    let weights = Maths.get_slice [ []; [ n_points * dim; -1 ] ] theta in
    let mus = Maths.reshape mus [| n_points; dim |] in
    let weights = Maths.reshape weights [| n_points; dim |] in
    Array.init n_points (fun i -> Maths.get_slice [ [ i ] ] mus), weights


  let random_prms () =
    let mus = Mat.gaussian ~sigma:1. 1 (dim * n_points) in
    let weights = Mat.gaussian ~sigma:0.001 1 (dim * n_points) in
    AD.Arr Mat.(mus @|| weights)


  let sigma2 = AD.F (2. *. sigma *. sigma)

  let flow ?duration:_ ~theta z _t =
    let mus, weights = unpack_prms theta in
    let open AD in
    let batch_size = (shape z).(0) in
    Array.fold_left
      (fun (i, accu) mui ->
        (* pi: 1 x dim *)
        let pi = Maths.get_slice [ [ i ] ] weights in
        let coeffs =
          (* di: batch_size x dim *)
          let di = Maths.(z - mui) in
          (* di: batch_size x 1 *)
          let di = Maths.(sum ~axis:1 (di * di)) in
          (* coeffs: batch_size x 1 *)
          Maths.(exp (neg di / sigma2))
        in
        i + 1, Maths.(accu + (coeffs *@ pi)))
      (0, AD.Mat.zeros batch_size dim)
      mus
    |> snd
end
