open Typ
module Make (P : Prms) : T

module Generative_model (P : sig
  include Typ.Prms

  val base_density_mu : Mat.mat
  val base_density_sigma : float
end) : sig
  include Typ.T

  val generate
    :  ?duration:float
    -> theta:AD.t
    -> [ `batch_of_size of int | `init of Mat.mat ]
    -> Mat.mat

  val log_density : ?duration:float -> theta:AD.t -> Mat.mat -> Mat.mat
  val log_likelihood : ?duration:float -> Mat.mat -> AD.t -> AD.t
end

val log_gaussian_density' : Mat.mat -> float -> Mat.mat -> float
