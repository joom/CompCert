val interactive : bool ref
val run_function :
  Csyntax.coq_function Ctypes.program ->
  BinNums.positive ->
  Values.coq_val list ->
  Memory.Mem.mem' -> Memory.Mem.mem' * Values.coq_val option * int
val run_interactive : Csyntax.coq_function Ctypes.program -> unit
