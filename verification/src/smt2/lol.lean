import smt2.syntax
import smt2.builder
open native
namespace lol

structure refinement (T : Type) :=
 (refinment : T → T)

mutual inductive type, term
with type : Type
| any : type -- universe, we do not need to care about it
| string : type
| bool : type
| int : type
| var : string → type
| fn : list type → type → type
| refinement : type → (string → term) → type
with term : Type
-- TODO: eventually allow for term in head position, and generalize in trans
-- TODO: stratify this so that Prop > Ordering > Arith
| apply : string → list term → term
| true : term
| false : term
| var : string → term
| not : term → term
| equals : term → term → term
| and : term → term → term
| or : term → term → term
| iff : term → term → term
| implies : term → term → term
| add : term → term → term
| sub : term → term → term
| mul : term → term → term
| div : term → term → term
| mod : term → term → term
| lt : term → term → term
| lte : term → term → term
| gt : term → term → term
| gte : term → term → term
| neg : term → term
| concat : term → term → term
| int : int → term
| string : string → term
| bool : bool → term
| forallq : string → type → term → term

meta def term.subst (n : string) (subst : term) : term → term
| (term.apply f args) := term.apply f (list.map term.subst args)
| term.true := term.true
| term.false := term.false
| (term.var s) :=
    if s = n
    then subst
    else term.var s
| (term.not t) := term.subst t
| (term.equals t u) :=
    term.equals (term.subst t) (term.subst u)
| (term.and t u) :=
    term.and (term.subst t) (term.subst u)
| (term.or t u) :=
    term.or (term.subst t) (term.subst u)
| (term.iff t u) :=
    term.iff (term.subst t) (term.subst u)
| (term.implies t u) :=
    term.implies (term.subst t) (term.subst u)
| (term.add t u) :=
    term.add (term.subst t) (term.subst u)
| (term.sub t u) :=
    term.sub (term.subst t) (term.subst u)
| (term.mul t u) :=
    term.mul (term.subst t) (term.subst u)
| (term.div t u) :=
    term.div (term.subst t) (term.subst u)
| (term.mod t u) :=
    term.mod (term.subst t) (term.subst u)
| (term.lt t u) :=
    term.lt (term.subst t) (term.subst u)
| (term.lte t u) :=
    term.lte (term.subst t) (term.subst u)
| (term.gt t u) :=
    term.gt (term.subst t) (term.subst u)
| (term.gte t u) :=
    term.gte (term.subst t) (term.subst u)
| (term.neg t) :=
    term.neg (term.subst t)
| (term.concat t u) :=
    term.concat (term.subst t) (term.subst u)
| (term.int i) := term.int i
| (term.string i) := term.string i
| (term.bool i) := term.bool i
| (term.forallq n ty body) := term.forallq n ty body
    -- term.forallq (term.subst t) (term.subst u)

-- meta def term.ordering_nat : term → nat
-- | term.true := 1
-- | term.false := 2
-- | (term.apply f args) := 1 + f.length + list.foldl (+) 0 (list.map term.ordering_nat args)
-- | (term.var s) :=
--     if s = n
--     then subst
--     else term.var s
-- | (term.not t) := term.subst t
-- | (term.equals t u) :=
--     term.equals (term.subst t) (term.subst u)
-- | (term.and t u) :=
--     term.and (term.subst t) (term.subst u)
-- | (term.or t u) :=
--     term.or (term.subst t) (term.subst u)
-- | (term.iff t u) :=
--     term.iff (term.subst t) (term.subst u)
-- | (term.implies t u) :=
--     term.implies (term.subst t) (term.subst u)
-- | (term.add t u) :=
--     term.add (term.subst t) (term.subst u)
-- | (term.sub t u) :=
--     term.sub (term.subst t) (term.subst u)
-- | (term.mul t u) :=
--     term.mul (term.subst t) (term.subst u)
-- | (term.div t u) :=
--     term.div (term.subst t) (term.subst u)
-- | (term.add t u) :=
--     term.add (term.subst t) (term.subst u)
-- | (term.sub t u) :=
--     term.sub (term.subst t) (term.subst u)
-- | (term.mul t u) :=
--     term.mul (term.subst t) (term.subst u)
-- | (term.lt t u) :=
--     term.lt (term.subst t) (term.subst u)
-- | (term.lte t u) :=
--     term.lte (term.subst t) (term.subst u)
-- | (term.gt t u) :=
--     term.gt (term.subst t) (term.subst u)
-- | (term.gte t u) :=
--     term.gte (term.subst t) (term.subst u)
-- | (term.neg t u) :=
--     term.neg (term.subst t) (term.subst u)
-- | (term.int t u) :=
--     term.int (term.subst t) (term.subst u)
-- | (term.forallq n ty body) := term.forallq n ty body

-- meta instance term.has_ordering : has_ordering term :=
-- ⟨ fun a b, ordering.lt ⟩
-- Jared, has_ordering is obsolete, we use has_lt to define maps.
-- I'm confused by the instance above. It is a constant function function?!?!?
-- So, I defined the following instance using a trivial predicate.

meta instance term.has_lt : has_lt term :=
⟨λ a b, true⟩

meta instance term.decidable_lt : decidable_rel ((<) : term → term → Prop) :=
λ a b, is_true trivial

mutual def type.to_string, list_map
with type.to_string : type → string
| (type.any) := "any"
| (type.string) := "string"
| (type.int) := "int"
| (type.bool) := "bool"
| (type.var s) := s
| (type.refinement t ref) := type.to_string t ++ " { x | }" -- thid doesn't work
| (type.fn args rt) := string.join (list_map args) ++ (type.to_string rt)
with list_map : list type → list string
| [] := []
| (t :: ts) := type.to_string t :: (list_map ts)
-- with term.to_string : term → string
-- | _ := "term"

instance type.has_to_string : has_to_string type :=
⟨ type.to_string ⟩
inductive decl
| fn : string → type → (option term) → decl

def decl.name : decl → string
| (decl.fn n _ _) := n

meta structure context :=
(type_decl : rb_map string type)
(decls : rb_map string decl)
(assertions : list term)

meta def context.empty : context :=
⟨ rb_map.mk _ _ , rb_map.mk _ _ , [] ⟩

meta def context.declare_type : context → string → type → context
| ⟨ type_decl, decls, assertions ⟩ n ty :=
match ty with
| type.fn _ _  := ⟨ type_decl.insert n ty,  decls, assertions ⟩
| _ := ⟨ type_decl, decls, assertions ⟩
end

meta def context.assert : context → term → context
| ⟨ type_decl, decls, assertions ⟩ t := ⟨ type_decl, decls, t :: assertions ⟩

meta def context.declare : context → decl → context
| ctxt decl :=
    { ctxt with decls := ctxt.decls.insert decl.name decl }

meta def context.lookup_type : context → string → option type
| ctxt n :=
    do d ← ctxt.decls.find n,
    match d with
    | (decl.fn _ ty _) := some ty
    end

open smt2.builder

meta structure smt2_compiler_state :=
(refinement_map : rb_map lol.term unit)
(ctxt : context)
(commands : list smt2.cmd)

meta def smt2_compiler := except_t string (state smt2_compiler_state)

meta instance smt2_compiler.monad : monad smt2_compiler :=
begin
dunfold smt2_compiler, apply_instance
end

meta def add_command (c : smt2.cmd) : smt2_compiler unit :=
do st ← except_t.lift get,
   except_t.lift $ put { st with commands := c :: st.commands }

meta def declare_fun (sym : string) (ps : list smt2.sort) (ret : smt2.sort) : smt2_compiler unit :=
add_command $ smt2.cmd.declare_fun sym ps ret

meta def declare_sort (sym : string) (arity : nat) : smt2_compiler unit :=
add_command $ smt2.cmd.declare_sort sym arity

meta def assert (t : smt2.term) : smt2_compiler unit :=
add_command $ smt2.cmd.assert_cmd t

meta def smt2_compiler.fail {α : Type} : string → smt2_compiler α :=
fun msg, except_t.mk (state_t.mk (fun s, (except.error msg, s)))

private meta def compile_type_simple : type → smt2_compiler smt2.sort
| (type.any) := return "Any"
| (type.string) := return "String"
| (type.bool) := return "Bool"
| (type.int) := return "Int"
| (type.var s) := return s
| (type.fn [] rt) := compile_type_simple rt
-- There is a bug here.
| (type.refinement t _) := compile_type_simple t
| t := smt2_compiler.fail $ "compile_simple_type: unsupported" ++ to_string t

@[reducible] meta def refinements : Type := string → smt2_compiler unit

meta def refinements.empty : refinements :=
fun _, return ()

meta def get_refinement : (term × type) → option term
| (t, type.refinement ty refn) := some $ term.subst "__bogus__" t (refn "__bogus__")
| _ := none

def filter_map {A B : Type} (f : A → option B) : list A → list B
| [] := []
| (x :: xs) :=
    match f x with
    | some r := r :: filter_map xs
    | none := filter_map xs
    end

meta def compile_type'
(compile_term : lol.term → smt2_compiler smt2.term) : type → smt2_compiler ((list smt2.sort) × smt2.sort × refinements)
| (type.any) := return ([], "Any", refinements.empty)
| (type.string) := return ([], "String", refinements.empty)
| (type.bool) := return ([], "Bool", refinements.empty)
| (type.int) := return ([], "Int", refinements.empty)
| (type.fn args ret) :=
    do args' ← monad.mapm compile_type_simple args,
       ret' ← compile_type_simple ret,
       return (args', ret', refinements.empty)
| (type.var s) := return ([], s, refinements.empty)
| (type.refinement t ref) :=
    let rs := (λ x,
        do let t := ref x,
           ct ← compile_term t,
           assert ct) in
    do sort ← compile_type_simple t,
       pure $ ([], sort, rs)

meta def inst_ref (f : string → term) : term → term :=
    fun t, term.subst "__bogus__" t (f "__bogus__")

meta def unless_cached (t : term) (action : smt2_compiler unit) : smt2_compiler unit :=
do st ← except_t.lift get,
   match st.refinement_map.find t with
   | none :=
     do let st' := { st with refinement_map := st.refinement_map.insert t () },
        except_t.lift $ put st',
        action
   | some _ := return ()
   end

private meta def add_refinement_post_cond
(compile_term : lol.term → smt2_compiler smt2.term) : string → lol.type → list lol.term → smt2_compiler unit
| n ty [] := return ()
| n (type.fn arg_tys ret_ty) args :=
    match get_refinement (term.apply n args, ret_ty) with
    | none := return ()
    | some ret_ref :=
    let refns := filter_map get_refinement (list.zip args arg_tys) in
    if refns.length = 0
    then return ()
    else do s_refns ← monad.mapm compile_term refns,
            s_ret_ref ← compile_term ret_ref,
            assert (smt2.builder.implies (smt2.builder.and s_refns) s_ret_ref)
    end
| _ _ _ := return ()

private meta def compile_term_app
(compile_term : lol.term → smt2_compiler smt2.term)
(t : string)
(us : list term) : smt2_compiler smt2.term :=
do args ← monad.mapm compile_term us,
   st ← except_t.lift get,
    match st.ctxt.lookup_type t with
    | none := smt2_compiler.fail "unknown function"
    | some ty :=
         do -- unless_cached (term.apply t us) (add_refinement_post_cond compile_term t ty us),
            pure $ smt2.term.apply t args
    end

private meta def compile_term : lol.term → smt2_compiler smt2.term
| (term.apply t us) := compile_term_app compile_term t us
| (term.true) := pure $ smt2.term.qual_id "true"
| (term.false) := pure $ smt2.term.qual_id "false"
| (term.var str) := pure $ smt2.term.qual_id str
| (term.not t) := smt2.builder.not <$> compile_term t
| (term.equals t u) := smt2.builder.equals <$> compile_term t <*> compile_term u
| (term.and t u) := smt2.builder.and2 <$> compile_term t <*> compile_term u
| (term.or t u) := smt2.builder.or2 <$> compile_term t <*> compile_term u
| (term.implies t u) := smt2.builder.implies <$> compile_term t <*> compile_term u
| (term.iff t u) := smt2.builder.iff <$> compile_term t <*> compile_term u
| (term.add a b) := smt2.builder.add <$> compile_term a <*> compile_term b
| (term.sub a b) := smt2.builder.sub <$> compile_term a <*> compile_term b
| (term.mul a b) := smt2.builder.mul <$> compile_term a <*> compile_term b
| (term.div a b) := smt2.builder.div <$> compile_term a <*> compile_term b
| (term.lt a b) := smt2.builder.lt <$> compile_term a <*> compile_term b
| (term.lte a b) := smt2.builder.lte <$> compile_term a <*> compile_term b
| (term.gt a b) := smt2.builder.gt <$> compile_term a <*> compile_term b
| (term.gte a b) := smt2.builder.gte <$> compile_term a <*> compile_term b
| (term.mod a b) := smt2.builder.mod <$> compile_term a <*> compile_term b
| (term.int i) := return $ smt2.builder.int_const i
| (term.string i) := return $ smt2.builder.string_const i
| (term.bool i) := return $ smt2.builder.bool_const i
| (term.forallq n ty body) := smt2.builder.forallq n <$> (compile_type_simple ty) <*> compile_term body
| (term.neg a) := smt2.builder.neg <$> compile_term a
| (term.concat t u) := smt2.builder.concat <$> compile_term t <*> compile_term u

-- TODO fix me
meta def compile_type := compile_type' compile_term

private meta def compile_types : list (string × type) → smt2_compiler unit
| [] := return ()
| ((n, ty) :: decls) :=
do match ty with
   | type.fn args ret :=
      declare_sort n 0
   | type.any := return ()
   | type.string := return ()
   | type.int := return ()
   | type.bool := return ()
   | type.var _ := return ()
   | type.refinement t ref := return ()
   end,
   compile_types decls.

private meta def compile_decl : decl → smt2_compiler unit
| (decl.fn n ty none) :=
    do (args, rt, ref) ← compile_type ty,
        declare_fun n args rt,
        ref n -- add refinments, revisit this after internship
| _ := return () -- TODO: fix me

private meta def compile_decls : list (string × decl) → smt2_compiler unit
| [] := return ()
| ((n, d) :: rs) := do compile_decl d, compile_decls rs

private meta def compile_assertions : list term → smt2_compiler unit
| [] := return ()
| (t :: ts) :=
  do t' ← compile_term t,
     assert t',
     compile_assertions ts

meta def compile : smt2_compiler unit :=
do st ← except_t.lift get,
    declare_sort "Any" 0,
   compile_types st.ctxt.type_decl.to_list,
   compile_decls st.ctxt.decls.to_list,
   compile_assertions st.ctxt.assertions

meta def to_builder {α : Type} (st : smt2_compiler_state) : smt2_compiler α → smt2.builder α
| action := let (exc, st') := action.run.run st in
    except_t.mk (state_t.mk (fun cs, (exc, cs ++ st'.commands)))

end lol
