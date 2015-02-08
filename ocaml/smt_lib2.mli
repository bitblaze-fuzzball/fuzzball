class vine_smtlib_printer : (string -> unit) -> object
  method declare_var : Vine.var -> unit
  method declare_var_value : Vine.var -> Vine.exp -> unit
  method assert_exp : Vine.exp -> unit
end
