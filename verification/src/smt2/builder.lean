import .syntax

@[reducible] def smt2.builder :=
except_t string (state (list smt2.cmd))

instance smt2.builder.monad : monad smt2.builder :=
by apply_instance

meta def smt2.builder.to_format {α : Type} (build : smt2.builder α) : format :=
format.join $ list.intersperse "\n" $ (list.map to_fmt $ (build.run.run []).snd).reverse

meta def smt2.builder.run {α : Type} (build : smt2.builder α) : (except string α × list smt2.cmd) :=
state_t.run (except_t.run build) []

def smt2.builder.fail {α : Type} : string → smt2.builder α :=
fun msg, except_t.mk (state_t.mk (fun s, (except.error msg, s)))

meta instance (α : Type) : has_to_format (smt2.builder α) :=
⟨ smt2.builder.to_format ⟩

namespace smt2

namespace builder

def equals (t u : term) : term :=
term.apply "=" [t, u]

def not (t : term) : term :=
term.apply "not" [t]

def implies (t u : term) : term :=
term.apply "=>" [t, u] -- old version "implies"

def forallq (sym : symbol) (s : sort) (t : term) : term :=
term.forallq [(sym, s)] t

def and (ts : list term) : term :=
term.apply "and" ts

def and2 (t u : term) : term :=
and [t, u]

def or (ts : list term) : term :=
term.apply "or" ts

def or2 (t u : term) : term :=
or [t, u]

def xor (ts : list term) : term :=
term.apply "xor" ts

def xor2 (t u : term) : term :=
xor [t, u]

def iff (t u : term) : term :=
term.apply "=" [t, u] -- old version "iff"

def lt (t u : term) : term :=
term.apply "<" [t, u]

def lte (t u : term) : term :=
term.apply "<=" [t, u]

def gt (t u : term) : term :=
term.apply ">" [t, u]

def gte (t u : term) : term :=
term.apply ">=" [t, u]

def add (t u : term) : term :=
term.apply "+" [t, u]

def sub (t u : term) : term :=
term.apply "-" [t, u]

def mul (t u : term) : term :=
term.apply "*" [t, u]

def div (t u : term) : term :=
term.apply "div" [t, u]

def mod (t u : term) : term :=
term.apply "mod" [t, u]

def neg (t : term) : term :=
term.apply "-" [t]

def concat (t u : term) : term :=
term.apply "str.++" [t, u]

def ite (c t f : term) : term :=
term.apply "ite" [c, t, f]

def int_const (i : int) : term :=
term.const $ special_constant.number i

def string_const (s : string) : term :=
term.const $ special_constant.string s

def bool_const (s : bool) : term :=
term.const $ special_constant.bool s

-- Begin bitvec operations
def bv_const (bitwidth:nat) (i : int) : term :=
term.const $ special_constant.bitvec bitwidth i

def bv_add (t u : term) : term :=
term.apply "bvadd" [t, u]

def bv_sub (t u : term) : term :=
term.apply "bvsub" [t, u]

def bv_mul (t u : term) : term :=
term.apply "bvmul" [t, u]

def bv_udiv (t u : term) : term :=
term.apply "bvudiv" [t, u]

def bv_sdiv (t u : term) : term :=
term.apply "bvsdiv" [t, u]

def bv_urem (t u : term) : term :=
term.apply "bvurem" [t, u]

def bv_smod (t u : term) : term :=
term.apply "bvsmod" [t, u]

def bv_srem (t u : term) : term :=
term.apply "bvsrem" [t, u]

def bv_or (t u : term) : term :=
term.apply "bvor" [t, u]

def bv_and (t u : term) : term :=
term.apply "bvand" [t, u]

def bv_xor (t u : term) : term :=
term.apply "bvxor" [t, u]

def bv_shl (t u : term) : term :=
term.apply "bvshl" [t, u]

def bv_lshr (t u : term) : term :=
term.apply "bvlshr" [t, u]

def bv_ashr (t u : term) : term :=
term.apply "bvashr" [t, u]

def bv_sle (t u : term) : term :=
term.apply "bvsle" [t, u]

def bv_slt (t u : term) : term :=
term.apply "bvslt" [t, u]

def bv_ule (t u : term) : term :=
term.apply "bvule" [t, u]

def bv_ult (t u : term) : term :=
term.apply "bvult" [t, u]

def bv_zext (bitsz : nat) (t : term) : term :=
term.apply2 (term.apply "_"
    [term.qual_id "zero_extend", term.const bitsz])
    [t]

def bv_sext (bitsz : nat) (t : term) : term :=
term.apply2 (term.apply "_"
    [term.qual_id "sign_extend", term.const bitsz])
    [t]

def bv_extract (upper lower : nat) (t : term) : term :=
term.apply2 (term.apply "_" [term.qual_id "extract",
    term.const ↑upper, term.const ↑lower])
    [t]
-- End bitvec operations

def add_command (c : cmd) : builder unit := do
cs ← except_t.lift get,
except_t.lift $ put (c :: cs)

def echo (msg : string) : builder unit :=
add_command (cmd.echo msg)

def check_sat : builder unit :=
add_command cmd.check_sat

def pop (n : nat) : builder unit :=
add_command $ cmd.pop n

def push (n : nat) : builder unit :=
add_command $ cmd.push n

def scope {α} (level : nat) (action : builder α) : builder α :=
do push level,
   res ← action,
   pop level,
   return res

def assert (t : term) : builder unit :=
add_command $ cmd.assert_cmd t

def reset : builder unit :=
add_command cmd.reset

def exit' : builder unit :=
add_command cmd.exit_cmd

def declare_const (sym : string) (s : sort) : builder unit :=
add_command $ cmd.declare_const sym s

def declare_fun (sym : string) (ps : list sort) (ret : sort) : builder unit :=
add_command $ cmd.declare_fun sym ps ret

def declare_sort (sym : string) (arity : nat) : builder unit :=
add_command $ cmd.declare_sort sym arity

end builder

end smt2
