
type 'a base = ('a -> float) array

module type Base = sig
  type input
  val base : input base
end

module type Interpolation = sig
  type input
  type interpolation

  val get : interpolation -> float array

  val zero : interpolation

  val compute : interpolation -> input -> float

  val compute_coefs : (input * float) array -> interpolation

  val error :  (input * float) array -> interpolation -> float

  val print : out_channel -> interpolation -> unit
end

module Make(B:Base) : Interpolation with type input = B.input
