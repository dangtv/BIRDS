import system.io
import data.buffer

open io.proc

structure z3_instance : Type :=
  (process : child)

def z3_instance.start  : io z3_instance :=
    z3_instance.mk <$> io.proc.spawn{
       cmd := "z3",
       args := ["-smt2", "-in"],
       stdin := io.process.stdio.piped,
       stdout := io.process.stdio.piped
    }

def z3_instance.raw (z3 : z3_instance) (cmd : string) : io string :=
do let z3_stdin := z3.process.stdin,
   let z3_stdout := z3.process.stdout,
   -- rexpose to_buffer
   let cs := cmd.to_list.to_buffer,
   io.fs.write z3_stdin cs,
   io.fs.close z3_stdin,
   res ← io.fs.read_to_end z3_stdout,
   io.fs.close z3_stdout,
   io.proc.wait z3.process,
   return res.to_string
