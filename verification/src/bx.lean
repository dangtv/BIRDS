import logic.basic
import tactic.basic
import tactic.finish
import tactic.interactive
import super
import smt2

open tactic
open auto
section propositional
variables {a b c d : Prop}
run_cmd mk_simp_attr `bx_simp

attribute [bx_simp] false_or
attribute [bx_simp] or_false 
attribute [bx_simp] false_and
attribute [bx_simp] and_false
attribute [bx_simp] true_and
attribute [bx_simp] and_true
attribute [bx_simp] true_or
attribute [bx_simp] and_true
attribute [bx_simp] not_and_self
attribute [bx_simp] and_not_self
attribute [bx_simp] not_not
attribute [bx_simp] not_true
attribute [bx_simp] not_false

@[bx_simp] theorem peirce_s : ((a → b) → a) ↔ a :=
by {intros, apply iff.intro,intro h1, apply or.elim (classical.em a), simp *, intro h2, 
simp [h2] at h1, contradiction, intros h1 h2,assumption}

@[bx_simp] theorem peirce_with_not : ((b → ¬ a) → a) ↔ a :=
by {intros, apply iff.intro,intro h1, apply or.elim (classical.em a), simp *, intro h2, 
simp [h2] at h1, contradiction, intros h1 h2,assumption}

@[bx_simp] lemma not_and_self_and : (¬a ∧ a ∧ b) ↔ false :=
iff_false_intro (λ h, and.elim h (λ h₁ h₂, and.elim h₂ (λ h₂ h₃, absurd h₂ h₁) ))

@[bx_simp] lemma and_not_seft_and : (a ∧ ¬a ∧ b) ↔ false :=
iff_false_intro (λ h, and.elim h (λ h₁ h₂, and.elim h₂ (λ h₂ h₃, absurd  h₁ h₂) ))

@[bx_simp] lemma and_seft_and : (a ∧ a ∧ b) ↔ a ∧ b :=
begin
apply iff.intro,
    intro h, cases h, assumption,
intro h, split, cases h, assumption, assumption,
end

@[bx_simp] lemma or_seft_or : (a ∨ a ∨ b) ↔ a ∨ b :=
begin
apply iff.intro,
    intro h, cases h, left, assumption, assumption,
intro h,
    cases h, left, assumption,
    right,right,assumption,
end

lemma or_self_and : (a ∨ a ∧ b) ↔ a :=
begin
apply iff.intro,
    intro h,
    cases h, assumption,
    exact h.left,
intro h,
left, assumption,
end

lemma and_self_or : (a ∧ (a ∨ b)) ↔ a :=
begin
apply iff.intro,
    intro h,
    exact h.left,
intro h,
split,
    assumption,
left, assumption,
end

end propositional

section quantifiers
variables {α : Sort*} {p q : α → Prop} {b : Prop}

-- @[simp] theorem forall_imp_iff_right (ha : ∀ x:α, p x) [decidable b] : ((∀ x:α, p x) → b) ↔ b:=
-- ⟨λf, f ha, imp_intro⟩

theorem exists_of_non : Exists p  ↔ ∃ x, p x :=
begin
intros,
apply iff.intro,
    intros,
    cases a with x a,
    existsi x,
    assumption,
intros,
cases a with x a,
existsi x,
assumption,
end

theorem forall_or_distrib_right {q : Prop} {p : α → Prop} [decidable q] [inhabited α] :
  (∀x, p x ∨ q) ↔ (∀x, p x) ∨ q :=
by simp [forall_or_distrib_left, or_comm]

theorem forall_and_distrib_right {q : Prop} {p : α → Prop} [decidable q] [inhabited α] :
  (∀x, p x ∧ q) ↔ (∀x, p x) ∧ q :=
by simp [forall_and_distrib]

theorem forall_and_distrib_left {q : Prop} {p : α → Prop} [decidable q] [inhabited α] :
  (∀x, q ∧ p x) ↔ q ∧ (∀x, p x) :=
by simp [forall_and_distrib]

theorem exists_or_distrib_left  {q : Prop} {p : α → Prop} [decidable q] [inhabited α]:
  (∃x, q ∨ p x) ↔ (q ∨ (∃x, p x)) :=
by simp [exists_or_distrib, exists_const]

theorem exists_or_distrib_right {q : Prop} {p : α → Prop} [decidable q] [inhabited α] :
  (∃x, p x ∨ q) ↔ (∃x, p x) ∨ q :=
by simp [exists_or_distrib, exists_const]

end quantifiers

-- disjunctive existial normal form
meta def denf_normalize : tactic unit :=
`[ intro h, 
    try{dsimp at h},
    try{simp only [not_and_distrib, not_or_distrib, exists_or_distrib,and_or_distrib_left, or_and_distrib_right, and_assoc, or_assoc] with bx_simp at h} ]

-- conjunctive existial normal form
meta def cenf_normalize : tactic unit :=
`[ try{dsimp},
    simp only [not_and_distrib, not_or_distrib, exists_or_distrib, and_or_distrib_right, or_and_distrib_left, and_assoc, or_assoc] with bx_simp]

meta def preprocess_only_hyps: tactic unit :=
`[  normalize_hyps {},
    repeat {do_substs <|> split_hyps <|> eelim /-<|> self_simplify_hyps-/} ]

-- meta def z3
-- meta def z3_smt : tactic unit := 
-- `[try{simp only [iff_def],},
-- try{simp only [not_forall_not.symm, not_not],},
-- z3 "temp.smt2"]

meta def z3_smt : tactic unit := 
`[try{simp only [iff_def],},
try{simp only [not_forall_not.symm, not_not],},
z3]