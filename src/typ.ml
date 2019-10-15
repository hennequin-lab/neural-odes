open Owl
module Mat = Dense.Matrix.D
module Arr = Dense.Matrix.D
module AD = Algodiff.D

module type Prms = sig
  val dim : int
  val abs_tol : float
  val flow : ?duration:float -> theta:AD.t -> AD.t -> float -> AD.t
end

module type T = sig
  include Prms

  val forward
    :  ?duration:float
    -> ?dt:float
    -> z0:Mat.mat
    -> theta:AD.t
    -> Mat.mat * Mat.mat * Mat.mat

  val backward : ?duration:float -> z1:Mat.mat -> adj1:Mat.mat -> theta:AD.t -> Mat.mat
  val make_diff : ?duration:float -> Mat.mat -> (Mat.mat -> AD.t -> AD.t) -> AD.t -> AD.t
end
