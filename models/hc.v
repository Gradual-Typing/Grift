
Require Import Coq.Init.Datatypes.

Definition blame_info : Type := nat. 

Inductive Ty_b :=
| Int : Ty_b
| Bool : Ty_b.

Inductive Ty_d : Type :=
| Dyn : Ty_d
| Static : Ty_s -> Ty_d
with
Ty_s :=
| Base : Ty_b -> Ty_s
| Composite : Ty_c -> Ty_s
with
Ty_c :=
| Ref : Ty_d -> Ty_c          
| Arr : Ty_d -> Ty_d -> Ty_c.
Hint Constructors Ty_b.
Hint Constructors Ty_d.
Hint Constructors Ty_s.
Hint Constructors Ty_c.

Scheme tyd_ind :=
       Induction for Ty_d Sort Prop
with tys_ind :=
  Induction for Ty_s Sort Prop
with tyc_ind :=
       Induction for Ty_c Sort Prop.

Notation "x → y" := (Arr x y) (at level 60, right associativity).
Definition Ty : Type := Ty_d.

Definition tyb_eq (x y : Ty_b) : bool :=
  match x, y with
    | Int, Int => true
    | Bool, Bool => true
    | _, _ => false
  end.

Fixpoint tyd_eq (t1 t2 : Ty_d) : bool :=
  match t1, t2 with
    | Dyn, Dyn => true
    | Static s1, Static s2 => tys_eq s1 s2
    | _, _ => false
  end
with tys_eq (t1 t2 : Ty_s) : bool :=
       match t1, t2 with
         | Base b1, Base b2 => tyb_eq b1 b2
         | Composite c1, Composite c2 =>
           tyc_eq c1 c2
         | _, _ => false
       end
with tyc_eq (t1 t2 : Ty_c)  : bool :=
       match t1, t2 with
         | t11 → t12, t21 → t22 =>
           andb (tyd_eq t11 t21) (tyd_eq t12 t22)
         | Ref t1, Ref t2 => tyd_eq t1 t2
         | _, _ => false
       end.



Check tyd_ind.

Lemma andb_true_conj :
  forall b1 b2: bool,
    (andb b1 b2) = true ->
    b1 = true /\ b2 = true.
Proof. intros [][]; auto. Qed. 

Ltac ifcase :=
  match goal with
    | [ |- context[if ?X then _ else _] ] => destruct X
  end.

Ltac ifcaseH :=
  match goal with
    | [ H : context[if ?X then _ else _] |- _ ] => destruct X
  end.

Ltac tyd_eq_is_eq_tac :=
  match goal with
    | [ H : ?P |- ?P ] => exact H
    | [H1 : forall x, tyd_eq ?t1 x = true -> _,
       H2 : tyd_eq ?t2 ?t3 = true |- _ ] =>
      apply H1 in H2
    | [H1 : forall x, ?P x -> _,
       H2 :           ?P ?t |- _ ] =>
      apply H1 in H2
    | [ |- forall x, _ ] => intro
    | [ |- ?x = ?x] => reflexivity 
    | [ H: ?x = ?y |- _ ] =>
      discriminate || congruence
    | [ H: _ = true |- _ ] =>
      simpl in H;
        discriminate
        || (apply andb_true_conj in H; destruct H)
    | [x : Ty_s |- _ ] => destruct x
    | [x : Ty_c |- _ ] => destruct x
    | [x : Ty_b |- _ ] => destruct x               | [H : tyd_eq _ ?x = true |- _ ] =>
       destruct x       
  end.

Lemma tyd_eq_is_eq : forall t1 t2, tyd_eq t1 t2 = true -> t1 = t2.
Proof.
  intros t1;
  elim t1 using tyd_ind with
  (P0 := fun ts:Ty_s => 
           forall t2,
             tyd_eq (Static ts) t2 = true -> (Static ts) = t2)
  (P1 := fun tc:Ty_c => 
           forall t2,
             tyd_eq (Static (Composite tc)) t2 = true ->
             (Static (Composite tc)) = t2).
  - repeat tyd_eq_is_eq_tac.   
  - repeat tyd_eq_is_eq_tac.  
  - repeat tyd_eq_is_eq_tac.
  - repeat tyd_eq_is_eq_tac.
  - repeat tyd_eq_is_eq_tac.
  - repeat tyd_eq_is_eq_tac. 
Qed. 
      
Inductive CTy :=
| CArr : Ty_d -> Ty_d -> CTy.

Notation "x ⇒ y" := (CArr x y) (at level 70, right associativity).

(* The main definition in Question *) 
Inductive hyper_coercion : Type := 
| HC : hc_p -> hc_m -> hc_i -> hyper_coercion
| Fail : hc_p -> Ty -> blame_info -> hyper_coercion
with hc_m : Type :=
| ι : Ty -> hc_m
| Med : Ty_c -> hc_s -> Ty_c -> hc_m
with hc_s : Type :=
| Fn_hc  : hyper_coercion -> hyper_coercion -> hc_s
| Ref_hc : hyper_coercion -> hyper_coercion -> hc_s
with hc_p : Type :=
| prj_mt : hc_p
| prj    : blame_info -> hc_p
with hc_i : Type :=
| inj_mt : hc_i
| inj    : hc_i.
         
(* Sugar *) 

Notation "t →hc g" := (Fn_hc t g) (at level 70).
Notation "⟨ t1 , s , t2 ⟩" := (Med t1 s t2) (at level 70).

Inductive hc_m2 : Type :=
| Id  : hc_m2 
| Through : hc_s -> hc_m2
| Bot : hc_m2.

Inductive hyper_coercion_wt : hyper_coercion -> CTy -> Prop :=
| hc_wt : forall t1 t2 t3 t4 p m i,
    hc_p_wt p (t1 ⇒ t2) ->
    hc_m_wt m (t2 ⇒ t3) ->
    hc_i_wt i (t3 ⇒ t4) ->
    hyper_coercion_wt (HC p m i) (t1 ⇒ t4)
| fail_wt : forall t1 t2 t3 p l,
    hc_p_wt p (CArr t1 t2) ->
    hyper_coercion_wt (Fail p t2 l) (t1 ⇒ t3)
with
hc_m_wt : hc_m -> CTy -> Prop :=
(* Having two id rules simplifies compose on this first pass *)
| Id_Dyn_wt  : hc_m_wt (ι Dyn) (Dyn ⇒ Dyn)
| Id_Base_wt : forall b,
   hc_m_wt (ι (Static (Base b)))
           ((Static (Base b)) ⇒ (Static (Base b)))
| Med_wt : forall t1 t2 s,
   hc_s_wt s ((Static (Composite t1))
                ⇒
                (Static (Composite t2))) ->
   hc_m_wt (Med t1 s t2)
           ((Static (Composite t1))
              ⇒
              (Static (Composite t2)))
with
hc_s_wt : hc_s -> CTy -> Prop :=
| Fn_hc_s_wt : forall t1 t2 t3 t4 c1 c2,
   hyper_coercion_wt c1 (t1 ⇒ t2) ->
   hyper_coercion_wt c2 (t3 ⇒ t4) ->
   hc_s_wt (c1 →hc c2)
           ((Static (Composite (t2 → t3)))
              ⇒
              (Static (Composite (t1 → t4))))
| Ref_hc_s_wt : forall t1 t2 c1 c2,
   hyper_coercion_wt c1 (t1 ⇒ t2) ->
   hyper_coercion_wt c2 (t2 ⇒ t1) ->
   hc_s_wt (Ref_hc c1 c2)
           ((Static (Composite (Ref t1)))
              ⇒
              (Static (Composite (Ref t2))))
with
hc_p_wt : hc_p -> CTy -> Prop :=
| prj_mt_wt : forall t, hc_p_wt prj_mt (t ⇒ t)
| prj_wt : forall n t,
   hc_p_wt (prj n)
           (Dyn ⇒ (Static (Composite t)))
with
hc_i_wt : hc_i  -> CTy -> Prop :=
| inj_mt_wt : forall t, hc_i_wt inj_mt (t ⇒ t)
| inj_c_wt : forall t,
  hc_i_wt inj ((Static (Composite t)) ⇒ Dyn).
Hint Constructors hc_i_wt hc_p_wt hc_s_wt hc_m_wt hyper_coercion_wt. 


Scheme hc_ind :=
  Induction for hyper_coercion Sort Prop
  with hcm_ind :=
  Induction for hc_m Sort Prop
  with hcs_ind :=
    Induction for hc_s Sort Prop.

Fixpoint comp_n n (c1 c2 : hyper_coercion): hyper_coercion :=
  match n with
    | S n' => 
      match c1, c2 with
        | HC prj_mt (ι _) inj_mt, c2 => c2
        | c1, HC prj_mt (ι _) inj_mt => c1
        | HC p1 m1 inj_mt, HC prj_mt m2 i2 =>
          match m1, m2 with
            | ι t1, ι t2 => HC p1 (ι t1) i2
            | Med t1 (c11 →hc c12) _, Med _  (c21 →hc c22) t2 =>
              HC p1 (Med t1 ((comp_n n' c21 c11) →hc (comp_n n' c12 c22)) t2) i2
            | Med t1 (Ref_hc c11 c12) _, Med _ (Ref_hc c21 c22) t2 =>
              HC p1 (Med t1 (Ref_hc (comp_n n' c11 c21) (comp_n n' c22 c12)) t2) i2
            | _ , _ => c2 (* Shouldn't be possible in this case *)
          end
        | HC p1 m1 inj, HC (prj l2) m2 i2 =>
          match m1, m2 with
            | ι t1, ι t2 =>
              if tyd_eq t1 t2
              then HC p1 (ι t1) i2
              else Fail p1 t2 l2
            | Med t1 (c11 →hc c12) _, Med _  (c21 →hc c22) t2 => 
              HC p1 (Med t1 ((comp_n n' c21 c11) →hc (comp_n n' c12 c22)) t2) i2
            | Med t1 (Ref_hc c11 c12) _, Med _ (Ref_hc c21 c22) t2 =>
              HC p1 (Med t1 (Ref_hc (comp_n n' c11 c21) (comp_n n' c22 c12)) t2) i2
            | Med t1 _ _,_ =>
              Fail p1 (Static (Composite t1)) l2
            | ι t1, _ => Fail p1 t1 l2
          end
        | _, _ => c2
      end
    | O => c2
  end.

Fixpoint height_hc c :=
  match c with
    | HC _ (ι _) _ => 1
    | HC _ (Med _ (c1 →hc _) _) _ => height_hc c1
    | HC _ (Med _ (Ref_hc c1 _) _) _ => height_hc c1
    | Fail _ _ _ => 1
  end.

Definition comp c1 c2 := comp_n (height_hc c1) c1 c2. 



Check hc_ind. 

Ltac comp_pres_wt_tac :=
  constructor ||
  reflexivity ||
  discriminate ||             
  match goal with
    | |- forall x, _ => intro x
    | |- _ -> _  => intro
    | |- hyper_coercion_wt _ _ =>
      eapply hc_wt ||
             eapply fail_wt
    | |- hc_p_wt _ _ =>
      apply prj_mt_wt || eapply prj_wt
    | |- hc_i_wt _ _ =>
      apply inj_mt_wt || eapply inj_c_wt
    | |- hc_m_wt _ _ =>
      apply Id_Dyn_wt ||
            eapply Id_Base_wt ||
            eapply Med_wt
    | |- hc_s_wt _ _ =>
      apply Ref_hc_s_wt || apply Fn_hc_s_wt
    | [H: _ |- _ ] =>
      solve[inversion H; subst; discriminate]     
  end.
    

Theorem comp_pres_wt :
  forall c1 c2 t1 t2 t3,
    (hyper_coercion_wt c1 (t1 ⇒ t2)) ->
    (hyper_coercion_wt c2 (t2 ⇒ t3)) ->
    (hyper_coercion_wt (comp c1 c2) (t1 ⇒ t3)).
Proof.
  intros c1.
  elim c1 using hc_ind 
  with
  (P0 := fun m:hc_m =>
           forall p i c2 t1 t2 t3,
             (hyper_coercion_wt (HC p m i) (t1 ⇒ t2)) ->
             (hyper_coercion_wt c2 (t2 ⇒ t3)) ->
             (hyper_coercion_wt (comp (HC p m i) c2) (t1 ⇒ t3)))
  (P1 := fun s:hc_s =>
           forall p t11 t12 i c2 t1 t2 t3,
             (hyper_coercion_wt (HC p (Med t11 s t12) i) (t1 ⇒ t2)) ->
             (hyper_coercion_wt c2 (t2 ⇒ t3)) ->
             (hyper_coercion_wt (comp (HC p (Med t11 s t12) i) c2) (CArr t1 t3))).
  repeat comp_pres_wt_tac.
  intro x. 
  progress destruct (HC h h0 h1).
  progress destruct (HC h h0 h1).


  match goal with
    
  end.

  comp_pres_wt_tac.
    intros p m IH1 i c2 t1 t2 t3 wtl wtr. 
    destruct p; destruct m; destruct i; destruct c2 as [p2 m2 i2 | p2 i2 l2].
    destruct p2; destruct m2; destruct i2. 
    + inversion wtl; inversion wtr. inversion H5. inversion H13. 
      destruct t1; destruct t3;
      repeat comp_pres_wt_tac.
      destruct t1; destruct t3;
      repeat comp_pres_wt_tac.
      

      subst.constructor. eapply inj_mt_wt. comp_pres_wt_tac.  
      
|  : forall t1 t2 s,
      end.
      apply (hc_wt Dyn Dyn Dyn). 
      apply (prj_mt_wt). 
      apply (
      destruct H3.
      destruct H5.
      destruct H6. 
      destruct H13. 
      destruct H11.
      destruct H14.
      compute.
      exact (hc_wt hc 
      subst. 
      exact (hc_wt {t2=Dyn}_ _ _). 
      inversion wtl. inversion H5. inversion wtr. discriminate h8. 
    destruct t. 
    inversion H3. 
    inversion 

      match goal with
      | |- _ /\
    end. simpl.
    
    simpl.


    constructor.
    auto. 
    inversion H6.
    inversion H5.
    inversion wtl.
    apply IH1 in wtl
                 with p := inj_mt.
    discriminate.
    
    case m. 



  Theorem hc_to_c_pres_comp :
    forall hc1 hc2,
      hc_to_c(comp_hc hc1 hc2) = comp_c (hc_to_c hc1) (hc_to_c hc2).

    (* todo implement regular coercions *)
    (* On hold until implementation is done.
     (* todo iso morphism test *)
     (* prove hyper_coercions now *)*)
    
(*b
Prove: 
hyper-coercion = compact-reference = coercions
Under Composition.
(* todo define isomorphism between hyper_coercions (hc), coercions (c))
(* hc_to_c_pres_comp hc_to_c(hc1;hc2) = hc_to_c(hc1);hc_to_c(hc2))
*)


