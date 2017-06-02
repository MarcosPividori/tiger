signature temp = sig
  type label = string
  type temp = string
  val newTemp: unit -> temp
  val newLabel: unit -> label
end
