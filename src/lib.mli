open Typ
module Make (P : Prms) : T

module Generative_model (P : Typ.Prms) (BD : Typ.Base_density) : sig
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

module Standard_gaussian_density (P : sig
  val dim : int
  val sigma : float
end) : Typ.Base_density
