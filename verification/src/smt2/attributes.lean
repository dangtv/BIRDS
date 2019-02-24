meta def smt2_attribute : user_attribute :=
{ name := `smt2,
  descr := "Mark a declaration as part of the global environment of the smt2 tactic" }

run_cmd register_attribute `smt2_attribute
