import system.io
import .builder
import .syntax
import .solvers.z3

inductive smt2.response
| sat
| unsat
| unknown
| other : string → smt2.response

meta def parse_smt2_result (str : string) : smt2.response :=
-- Removes trailing white space.
let str := list.as_string (str.to_list.reverse.drop_while
      (λ (c:char), c.is_whitespace ∨ c.to_nat = 13 /- carriage return -/))
      .reverse in
if str = "sat"
then smt2.response.sat
else if str = "unsat"
then smt2.response.unsat
else if str = "unknown"
then smt2.response.unknown
else smt2.response.other str

meta def cmds_to_string (cmds : list smt2.cmd) : string :=
to_string $ format.join $ list.intersperse "\n" $ list.map (λ c, to_fmt c) cmds.reverse

meta def smt2 (build : smt2.builder unit) (log_query : option string := none) : io smt2.response :=
do z3 ← z3_instance.start,
   -- maybe we should have a primitive to go from fmt to char buffer
   let (exc, cmds) := build.run,
   match exc with
   | (except.error e) := io.fail $ "builder failed with: " ++ e -- TODO: better message
   | (except.ok v) :=
     do let query := cmds_to_string cmds,
     match log_query with
     | none := return ()
     | some fn :=
       do file ← io.mk_file_handle fn io.mode.write,
          io.fs.write file (query.to_list.to_buffer)
     end,
     res ← z3.raw query,
     return $ parse_smt2_result res
  end
