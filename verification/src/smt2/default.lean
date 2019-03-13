import system.io
import .solvers.z3
import .syntax
import .builder
import .tactic
import .attributes
import .lol
import init.data.option.basic

declare_trace smt2

open tactic
open smt2.builder
open native

constant real:Type

meta structure smt2_state : Type :=
(ctxt : lol.context)
(type_map : rb_map expr lol.type)

meta def smt2_state.initial : smt2_state :=
⟨ lol.context.empty, rb_map.mk _ _ ⟩

@[reducible] meta def smt2_m (α : Type) :=
state_t smt2_state tactic α

meta instance tactic_to_smt2_m (α : Type) : has_coe (tactic α) (smt2_m α) :=
⟨ fun tc, state_t.mk (fun s, do res ← tc, return (res, s)) ⟩

namespace smt2

meta def trace_smt2 (msg : string) : smt2_m unit :=
  tactic.when_tracing `smt2 (tactic.trace msg)

meta def fail {α : Type} (msg : string) : smt2_m α :=
tactic.fail $ "smt2_tactic: " ++ msg

meta def mangle_name (n : name) : string :=
"lean_" ++ n^.to_string_with_sep "-"

meta def insert_type (n : string) (ty : expr) (lty : lol.type) : smt2_m unit :=
do st ← get,
   put ⟨
     st.ctxt.declare_type n lty,
     st.type_map.insert ty lty
   ⟩

meta def fn_type : expr → (list expr × expr)
| (expr.pi _ _ ty rest) :=
    let (args, rt) := fn_type rest
    in (ty :: args, rt)
| rt := ([], rt)

-- Currently we only support first order fn types
meta def compile_arrow_type (ty : expr) (cb : expr → smt2_m lol.type) : smt2_m lol.type :=
let (args, rt) := fn_type ty
in lol.type.fn <$> monad.mapm cb args <*> cb rt

meta def compile_type : expr → smt2_m lol.type :=
fun ty,
do st ← get,
   match st.type_map.find ty with
   | some lty := return lty
   | none := do
     lty ← match ty with
     | `(int) := pure $ lol.type.int
     | `(real) := pure $ lol.type.any
     | `(string) := pure $ lol.type.string
     | `(nat) := pure $ lol.type.refinement lol.type.int (fun x, lol.term.lte (lol.term.int 0) (lol.term.var x))
     | `(bool) := pure $ lol.type.bool
     | `(Prop) := pure $ lol.type.bool
     | _ := if ty.is_arrow
            then compile_arrow_type ty compile_type
            else if ty.is_constant
            then do insert_type (mangle_name ty.const_name) ty (lol.type.fn [] (lol.type.var $ mangle_name ty.const_name)),
                 return $ (lol.type.fn [] (lol.type.var $ mangle_name ty.const_name))
            else fail $ "unsupported type: " ++ to_string ty
     end,
     -- insert_type ty lty,
     return lty
   end

meta def add_decl (n : name) (ty : expr) : smt2_m unit :=
  do st ← get,
     ct ← compile_type ty,
     let d := lol.decl.fn (mangle_name n) ct none,
     put { st with ctxt := st.ctxt.declare d }

-- meta def ensure_constant (e : expr) (n : name) : smt2_m lol.decl :=
--   do ty ← infer_type e,
--    let (arg_tys, ret_ty) := fn_type ty,
--    let mangled_name := mangle_name n,
--    arg_sorts ← monad.mapm compile_type arg_tys,
--    ret_sort ← compile_type ret_ty,
--    -- ensure_constant_core e (return $ (mangled_name, arg_sorts, ret_sort)),
--    return $ lol.decl.fn mangled_name arg_sorts ret_sort

-- meta def formula_type_from_arrow (n : name) (e : expr) : smt2_m formula_type :=
-- do (lol.decl.fn _ arg_sorts ret_sort) ← ensure_constant e n,
--    return $ formula_type.fn n arg_sorts ret_sort

-- /-- The goal of this function is to categorize the set of formulas in the hypotheses,
--     and goal. We want to narrow down from the full term language of Lean to a fragment
--     of formula's we suppose. The below code makes some assumptions:

--    A local constant of the form `(P : Prop)`, must be reflected as declaration
--    in SMT2 that is `(declare-const P Bool)`.

--    An occurence of a proof of `P`, `(p : P)`, must be transformed into
--    `(assert P)`. If P is a formula, not an atom, we must transform P into a corresponding
--    SMT2 formula and `(assert P)`.
-- -/

meta def extract_coe_args (args : list expr) : smt2_m (expr × expr × expr) :=
match args with
| (source :: target :: inst :: e :: []) := return (source, target, e)
| _ := fail "internal tactic error expected `coe` to have exactly 4 arguments"
end

meta def reflect_coercion (source target e : expr) (callback : expr → smt2_m lol.term) : smt2_m lol.term :=
if source = `(nat) ∧ target = `(int)
then callback e
else fail $ "unsupported coercion between " ++ "`" ++ to_string source ++ "` and `" ++ to_string target ++ "`"

meta def reflect_application (fn : expr) (args : list expr) (callback : expr → smt2_m lol.term) : smt2_m lol.term :=
    if fn.is_constant
    then if fn.const_name = `coe
          then do (source, target, e) ← extract_coe_args args,
                   reflect_coercion source target e callback
          else do ty ← infer_type fn,
                  let mangled := (mangle_name fn.const_name),
                  add_decl fn.const_name ty,
                  lol.term.apply mangled <$> monad.mapm callback args
    else if fn.is_local_constant
    then lol.term.apply (mangle_name fn.local_uniq_name) <$> monad.mapm callback args
    else fail $ "unsupported head symbol `" ++ to_string fn ++ "`"

-- meta def is_supported_head_symbol (e : expr) : bool := true

meta def is_supported_numeric_ty (ty : expr) : bool :=
(ty = `(int) ∨ ty = `(nat))

meta def is_supported_string_ty (ty : expr) : bool :=
(ty = `(string))

-- /-- This function is the meat of the tactic, it takes a propositional formula in Lean, and transforms
--    it into a corresponding term in SMT2. -/
meta def reflect_arith_formula (reflect_base : expr → smt2_m lol.term) : expr → smt2_m lol.term
| `(%%a + %%b) := lol.term.add <$> reflect_arith_formula a <*> reflect_arith_formula b
| `(%%a - %%b) := lol.term.sub <$> reflect_arith_formula a <*> reflect_arith_formula b
| `(%%a * %%b) := lol.term.mul <$> reflect_arith_formula a <*> reflect_arith_formula b
| `(%%a / %%b) := lol.term.div <$> reflect_arith_formula a <*> reflect_arith_formula b
| `(%%a % %%b) := lol.term.mod <$> reflect_arith_formula a <*> reflect_arith_formula b
| `(- %%a) := lol.term.neg <$> reflect_arith_formula a
-- /- Constants -/
| `(has_zero.zero _) := lol.term.int <$> eval_expr int `(has_zero.zero int)
| `(has_one.one _) := lol.term.int <$> eval_expr int `(has_one.one int)
| `(bit0 %%Bits) :=
  do ty ← infer_type Bits,
     if is_supported_numeric_ty ty
     then lol.term.int <$> eval_expr int `(bit0 %%Bits : int)
     else if (ty = `(nat))
     then lol.term.int <$> int.of_nat <$> eval_expr nat `(bit0 %%Bits : nat)
     else fail $ "unknown numeric literal: " ++ (to_string ```(bit0 %%Bits : int))
| `(bit1 %%Bits) :=
  do ty ← infer_type Bits,
     if is_supported_numeric_ty ty
     then lol.term.int <$> eval_expr int `(bit1 %%Bits : int)
     else if (ty = `(nat))
     then lol.term.int <$> (int.of_nat <$> eval_expr nat `(bit1 %%Bits : nat))
     else fail $ "unknown numeric literal: " ++ (to_string `(bit1 %%Bits : int))
| a :=
    if a.is_local_constant
    then return $ lol.term.var (mangle_name a.local_uniq_name)
    else if a.is_constant
    then return $ lol.term.var (mangle_name a.const_name)
    else if a.is_app
    then reflect_application (a.get_app_fn) (a.get_app_args) reflect_base
    else fail $ "unsupported arithmetic formula: " ++ to_string a

meta def reflect_string_formula (reflect_base : expr → smt2_m lol.term) : expr → smt2_m lol.term
| `(%%a ++ %%b) := lol.term.concat <$> reflect_string_formula a <*> reflect_string_formula b
| a := 
    if a.is_local_constant
    then return $ lol.term.var (mangle_name a.local_uniq_name)
    else if a.is_constant
    then return $ lol.term.var (mangle_name a.const_name)
    else
    lol.term.string <$> eval_expr string `(to_string a:string)

-- /-- Check if the type is an `int` or logically a subtype of an `int` like nat. -/
meta def is_int (e : expr) : tactic bool :=
do ty ← infer_type e,
   return $ (ty = `(int)) || (ty = `(nat))

meta def unsupported_ordering_on {α : Type} (elem : expr) : tactic α :=
do ty ← infer_type elem,
   tactic.fail $ "unable to translate orderings for values of type: " ++ to_string ty

meta def reflect_ordering (reflect_arith : expr → smt2_m lol.term) (R : lol.term → lol.term → lol.term) (P Q : expr) : smt2_m lol.term :=
do is ← is_int P, -- NB: P and Q should have the same type.
   if is
   then R <$> (reflect_arith P) <*> (reflect_arith Q)
   else unsupported_ordering_on P

meta def supported_pi_binder (ty : expr) : bool :=
match ty with
| `(int) := tt
| `(nat) := tt
| `(Prop) := tt
| `(string) := tt
| `(bool) := tt
| _ := if ty.is_constant
       then tt
       else ff
end

meta def add_assertion (t : lol.term) : smt2_m unit :=
  do st ← get,
     put { st with ctxt := st.ctxt.assert t }

meta def compile_pi (e : expr) (cb : expr → smt2_m lol.term) : smt2_m lol.term :=
if supported_pi_binder e.binding_domain
then do loc ← tactic.mk_local' e.binding_name e.binding_info e.binding_domain,
        lol.term.forallq
          (mangle_name $ loc.local_uniq_name) <$>
          (compile_type $ e.binding_domain) <*>
          (cb (expr.instantiate_var (e.binding_body) loc))
else fail $ "arbitrary Π types are not supported, unable to translate term: `" ++ to_string e ++ "`"

meta def reflect_prop_formula' : expr → smt2_m lol.term
| `(¬ %%P) := lol.term.not <$> (reflect_prop_formula' P)
| `(%%P = %% Q) := lol.term.equals <$> (reflect_prop_formula' P) <*> (reflect_prop_formula' Q)
| `(%%P ∧ %%Q) := lol.term.and <$> (reflect_prop_formula' P) <*> (reflect_prop_formula' Q)
| `(%%P ∨ %%Q) := lol.term.or <$> (reflect_prop_formula' P) <*> (reflect_prop_formula' Q)
| `(%%P ↔ %%Q) := lol.term.iff <$> (reflect_prop_formula' P) <*> (reflect_prop_formula' Q)
| `(%%P < %%Q) := reflect_ordering (reflect_arith_formula reflect_prop_formula') lol.term.lt P Q
| `(%%P <= %%Q) := reflect_ordering (reflect_arith_formula reflect_prop_formula') lol.term.lte P Q
| `(%%P > %%Q) := reflect_ordering (reflect_arith_formula reflect_prop_formula') lol.term.gt P Q
| `(%%P >= %%Q) := reflect_ordering (reflect_arith_formula reflect_prop_formula') lol.term.gte P Q
| `(true) := return $ lol.term.true
| `(false) := return $ lol.term.false
| e := do ty ← infer_type e,
       if e.is_local_constant
       then pure $ lol.term.var (mangle_name e.local_uniq_name)
       else if e.is_arrow
       then lol.term.implies <$> (reflect_prop_formula' e.binding_domain) <*> (reflect_prop_formula' e.binding_body )
       else if e.is_pi
       then compile_pi e reflect_prop_formula'
       else if is_supported_numeric_ty ty
       then reflect_arith_formula reflect_prop_formula' e
       else if is_supported_string_ty ty
       then reflect_string_formula reflect_prop_formula' e
       else if e.is_app
       then reflect_application (e.get_app_fn) (e.get_app_args) reflect_prop_formula'
       else tactic.fail $ "unsupported propositional formula : " ++ to_string e

meta def reflect_prop_formula (e : expr) : smt2_m unit :=
reflect_prop_formula' e >>= add_assertion

-- meta def warn_unable_to_trans_local (e : expr) : smt2_m (builder unit) := do
--   trace_smt2 $ "unable to translate local variable: " ++ to_string e,
--   return $ return ()

meta def is_builtin_type : expr → bool
| `(string) := tt
| `(bool) := tt
| `(int) := tt
| `(Prop) := tt
| `(nat) := tt
| _ := ff

meta def unsupported_formula (e : expr) : smt2_m unit :=
fail $ "unsupported formula: " ++ to_string e

meta def compile_local (e : expr) : smt2_m unit :=
do ty ← infer_type e,
   prop_sorted ← is_prop ty,
   if e.is_local_constant
   then if is_builtin_type ty
        then add_decl e.local_uniq_name ty
        else if ty.is_arrow
        then add_decl e.local_uniq_name ty
        else if prop_sorted
        then reflect_prop_formula ty
        else unsupported_formula ty
   else if e.is_constant
   then if is_builtin_type ty ∨ ty.is_arrow
        then add_decl e.const_name ty
        else if prop_sorted
        then reflect_prop_formula ty
        else unsupported_formula ty
   else if (ty = `(Prop))
   then reflect_prop_formula e
   else unsupported_formula e

meta def reflect_attr_decl (n : name) : smt2_m unit :=
do exp ← mk_const n,
   compile_local exp

/- Reflect the environment consisting of declarations with the `smt2` attribute. -/
meta def reflect_environment : smt2_m unit :=
do decls ← attribute.get_instances `smt2,
   bs ← monad.mapm reflect_attr_decl decls.reverse,
   return ()

meta def reflect_context : smt2_m unit :=
 do ls ← local_context,
    bs ← monad.mapm (fun e, compile_local e) ls,
    return ()

meta def reflect_goal : smt2_m unit :=
  do tgt ← target,
     -- SMT solvers are looking for satisfiabiltiy, so we must negate to check validity.
     reflect_prop_formula `(_root_.not %%tgt),
     return ()

meta def reflect : smt2_m (builder unit) :=
do reflect_environment,
   reflect_context,
   reflect_goal,
   st ← get,
   return $ (lol.to_builder (lol.smt2_compiler_state.mk (rb_map.mk _ _) st.ctxt []) lol.compile >> check_sat)

end smt2

universe u

@[smt2] lemma int_of_nat_is_pos :
  forall (n : nat), 0 <= int.of_nat n :=
begin
  intros, trivial
end

axiom proof_by_z3 (A : Sort u) : A

meta def z3 (log_file : option string := none) : tactic unit :=
do (builder, _) ← smt2.reflect.run smt2_state.initial,
   resp ← unsafe_run_io (smt2 builder log_file),
   match resp with
   | smt2.response.sat := fail "z3 was unable to prove the goal"
   | smt2.response.unknown := fail "z3 was unable to prove the goal"
   | smt2.response.other str := fail $ "z3 communication error, unexpected response:\n\n" ++ str ++ "\n"
   | smt2.response.unsat := do
        tgt ← target,
        sry ← to_expr $ ``(proof_by_z3 %%tgt),
        exact sry
   end
