
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

Hint Constructors Ty_d Ty_b Ty_s Ty_c.


Scheme tyd_ind :=
  Induction for Ty_d Sort Prop
with tys_ind :=
  Induction for Ty_s Sort Prop
with tyc_ind :=
  Induction for Ty_c Sort Prop.

Notation "x → y" := (Arr x y) (at level 60, right associativity).
Definition Ty : Type := Ty_d.

Fixpoint ty_depth t : nat :=
  match t with
  | (Static (Composite (t1 → t2))) =>
    max (ty_depth t1) (ty_depth t2)
  | (Static (Composite (Ref t1)))  => ty_depth t1
  | _ => 1
  end.

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
with
tys_eq (t1 t2 : Ty_s) : bool :=
  match t1, t2 with
  | Base b1, Base b2 => tyb_eq b1 b2
  | Composite c1, Composite c2 =>
    tyc_eq c1 c2
  | _, _ => false
  end
with
tyc_eq (t1 t2 : Ty_c)  : bool :=
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
    | [x : Ty_b |- _ ] => destruct x
    | [H : tyd_eq _ ?x = true |- _ ] =>
       destruct x       
  end.

Lemma tyd_eq_is_eq : forall t1 t2,
    tyd_eq t1 t2 = true -> t1 = t2.
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
(* When we get to it...
   work on compose suggests 
   Fail : hc_p -> Ty -> blame_info -> 
          blame_info -> Ty -> hc_i
   Is the symetric version of Failure needed for
   Compact References
*)
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

Notation "t →hc g" := (Fn_hc t g) (at level 70).

Fixpoint hc_depth c : nat :=
  match c with
  | HC _ (Med _ (c1 →hc c2) _) _ =>
    max (hc_depth c1) (hc_depth c2)
  | HC _ (Med _ (Ref_hc c1 c2) _) _ =>
    max (hc_depth c1) (hc_depth c2)
  | _ => 1
  end. 

Definition hc_ty_depth c : nat :=
  match c with
  | HC _ (ι t) _ => ty_depth t
  | HC _ (Med t1 _ t2) _ =>
    max (ty_depth (Static (Composite t1)))
        (ty_depth (Static (Composite t2)))
  | Fail _ t _ => ty_depth t
  end.


Fixpoint mk_hc n t1 t2 l : hyper_coercion :=
  match n with
  | 0 => Fail prj_mt Dyn 0 (* This is jiberish *)
  | S n' =>
    match t1, t2 with
    | Dyn, Dyn => HC prj_mt (ι Dyn) inj_mt
    | Dyn, (Static (Base b)) =>
      HC (prj l) (ι (Static (Base b))) inj_mt
    | Dyn, (Static (Composite ((t21 → t22) as t2_c))) =>
      HC (prj l)
         (Med t2_c
              (mk_hc n' t21 t21 l →hc mk_hc n' t22 t22 l)
              t2_c)
         inj_mt
    | Dyn, (Static (Composite ((Ref t21) as t2_c))) =>
      HC (prj l)
         (Med t2_c
              (Ref_hc (mk_hc n' t21 t21 l)
                      (mk_hc n' t21 t21 l))
              t2_c)
         inj_mt
    | (Static (Base b)), Dyn =>
      HC prj_mt (ι (Static (Base b))) inj
    | (Static (Composite ((t21 → t22) as t2_c))), Dyn =>
      HC prj_mt
         (Med t2_c
              (mk_hc n' t21 t21 l →hc mk_hc n' t22 t22 l)
              t2_c)
         inj
    | (Static (Composite ((Ref t21) as t2_c))), Dyn =>
      HC prj_mt
         (Med t2_c
              (Ref_hc (mk_hc n' t21 t21 l)
                      (mk_hc n' t21 t21 l))
              t2_c)
         inj
    | (Static (Base b1)), (Static (Base b2)) =>
      if tyb_eq b1 b2
      then HC prj_mt (ι (Static (Base b2))) inj_mt
      else Fail prj_mt (Static (Base b1)) l 
    | Static (Composite ((t11 → t12) as t1_c)),
      Static (Composite ((t21 → t22) as t2_c)) =>
      HC prj_mt
         (Med t1_c
              (mk_hc n' t12 t11 l →hc mk_hc n' t21 t22 l)
              t2_c)
         inj
    | Static (Composite ((Ref t11) as t1_c)),
      Static (Composite ((Ref t21) as t2_c)) =>
      HC prj_mt
         (Med t1_c
              (Ref_hc (mk_hc n' t11 t21 l)
                      (mk_hc n' t21 t11 l))
              t2_c)
         inj_mt
    | _, _ => Fail prj_mt t1 l 
    end
  end.

Definition make_hc (t1 t2 : Ty) l : hyper_coercion :=
  mk_hc (max (ty_depth t1) (ty_depth t2)) t1 t2 l. 

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

Fixpoint comp n c1 c2 : hyper_coercion :=
  match n with
  | O => Fail prj_mt Dyn 0
  | S n' => 
    match c1, c2 with
    | HC prj_mt (ι _) inj_mt, c2 => c2
    | c1, HC prj_mt (ι _) inj_mt => c1
    (* empty inj/prj no fails *)
    | HC p1 m1 inj_mt, HC prj_mt m2 i2 =>
      match m1, m2 with
      | ι t1, ι t2 => HC p1 (ι t1) i2
      | Med t1 (c11 →hc c12) _,
        Med _  (c21 →hc c22) t2 =>
        HC p1
           (Med t1
                ((comp n' c21 c11) →hc (comp n' c12 c22))
                t2)
           i2
      | Med t1 (Ref_hc c11 c12) _,
        Med _  (Ref_hc c21 c22) t2 =>
        HC p1
           (Med t1
                (Ref_hc (comp n' c11 c21)
                        (comp n' c22 c12))
                t2)
           i2
      (* Shouldn't be possible if input is well typed *)
      | _ , _ => Fail prj_mt Dyn 0
      end
    (* empty inj/prj fails on second *)
    | HC p1 m inj_mt, Fail prj_mt t2 l =>
      match m with
      | (ι t1) => Fail p1 t1 l
      | (Med t1 _ _) => Fail p1 (Static (Composite t1)) l
      end                                
    (* full  inj/prj no failure *) 
    | HC p1 m1 inj, HC (prj l) m2 i2 =>
      match m1, m2 with
      | ι t1, ι t2 =>
        if tyd_eq t1 t2
        then HC p1 (ι t1) i2
        else Fail p1 t2 l
      | Med t1 _ t2, Med t3 _ t4 => 
        comp n'
             (HC p1 m1 inj_mt)
             (comp n'
                   (make_hc (Static (Composite t2))
                            (Static (Composite t3))
                            l)
                   (HC prj_mt m2 i2))
      (* These rules indicate that composite types have
         no direct identities *)
      | Med t1 _ _, _ =>
        Fail p1 (Static (Composite t1)) l
      | ι t1, _ => Fail p1 t1 l
      end
    (* full  inj/prj failure in second *)
    | HC p1 m inj_mt, Fail (prj l1) t3 l2 =>
      let (t1, t2) :=
          match m with
          | (ι t1) => (t1, t1)
          | (Med t1 _ t2) =>
            (Static (Composite t1), Static (Composite t2))
          end in
      if tyd_eq t2 t3
      then Fail p1 t1 l2
      else Fail p1 t1 l1
               
                            

    | Fail p1 t12 l, _ => Fail p1 t12 l
    (* Everything else not well typed *)
    | _, _ => Fail prj_mt Dyn 0
    end
  end.

      (* alternatively with type-constructor identities
      | Med t1 s1 t2, Med t3 s2 t4 => 
        match make_hc_c n t2 t3 l with 
        | Id        => HC p1 (Med t1 (comp_s n' s1 s2) t4) i2
        | Through s => HC p1 (Med t1 (comp_s n' s1 (comp_s n' s s2)) t4) i2
        | Failed    => Fail p1 t1 l
        end    
      | ι t1, Med t3 s2 t4 =>
        match make_hc_c n t1 t3 l with
        | Id        => HC p1 (Med t1 s2 t4) i1
        | Through s => HC p1 (Med t1 (comp_s n' s s2) t2) i1
        | Failed    => Fail p1 t1 l     
        end
      | Med t1 s1 t2, ι t4 =>
        match make_hc_c n t2 t3 l with
        | Id        => HC p1 (Med t1 s1 t4) i1
        | Through s => HC p1 (Med t1 (comp_s n' s1 s) t2) i1
        | Failed    => Fail p1 t1 l     
        end
      *) (* But this is just duplicating code and not compressing identities *)
      (*
      | Med t1 s1 t2, Med t3 s2 t4 =>
        fl n' p1 t1 m1 (fr n' (make_hc (Static (Comp t2)) (Static (Comp t4)) l) m2 i2)
      | Med t1 s1 t2, ι t4         => 
        fl n' p1 t1 m1 (make_hc (Static (Comp t2)) (Static (Comp t4)) l) (* this drops i *)
      | ι t1        , Med t3 s2 t4 =>
        fl n' (make_hc (Static t1) (Static (Comp t4)) l) m2 i2
       (* 
       with 
       fl (n : Nat) (p : hc_p) (t1 : Ty_c) (m : hc_m) (c : hyper_coercion) :=
          match c with
          | HC _ (ι t2) i => HC p (Med t1 s t2) i
          | HC _ (Med t2 s t3) i =>
            match comp_m n m (Med t2 s t3) with
            | None => Fail p t1 l
            | Some (ι t5) => HC p (i t5) i
            | Some (Med t4 s t5) => HC p (Med t4 s t5) i
          | Fail _ t2 l => Fail p t1 l
          end.
       with
       f2 (n : Nat) (c : hyper_coercion) (m : hc_m) (i : hc_i) :=
          match c with
           | HC p (ι t2) _ =>  HC p (Med t1 s t2) i
           | HC p (Med t2 s t3) _ =>
             match comp_s n s m with
             | None => Fail p t1 l
             | Some (ι t5) => HC p (i t3) i
             | Some (Med t4 s t5) => HC p (Med t4 s t5) i
          | Fail p t2 l => Fail p t1 l
          end.
       with
       comp_m n t1 m1 m2 t2 :=
         match
       *)
       *)


              


Definition compose c1 c2 :=
  comp (2 * (max (hc_ty_depth c1) (hc_ty_depth c2)))
       c1 c2. 



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
    (hyper_coercion_wt (compose c1 c2) (t1 ⇒ t3)).
Proof.
  intros c1.
  elim c1 using hc_ind 
  with
  (P0 := fun m:hc_m =>
           forall p i c2 t1 t2 t3,
             (hyper_coercion_wt (HC p m i) (t1 ⇒ t2)) ->
             (hyper_coercion_wt c2 (t2 ⇒ t3)) ->
             (hyper_coercion_wt (compose (HC p m i) c2) (t1 ⇒ t3)))
  (P1 := fun s:hc_s =>
           forall p t11 t12 i c2 t1 t2 t3,
             (hyper_coercion_wt (HC p (Med t11 s t12) i) (t1 ⇒ t2)) ->
             (hyper_coercion_wt c2 (t2 ⇒ t3)) ->
             (hyper_coercion_wt (compose (HC p (Med t11 s t12) i) c2) (CArr t1 t3))).
  repeat comp_pres_wt_tac.
  destruct c2.
  match goal with
  | [ |-
  | [ H1: hyper_coercion_wt _ _ ,
      H2: hyper_coercion_wt _ _
      |- hyper_coercion_wt
           (compose (HC ?p1 ?m1 ?i1) (HC ?p2 ?m2 ?i2))
           _ ] =>
    destruct p1;
      destruct p2;
      destruct m1;
      destruct m2;
      destruct i1;
      destruct i2;
      inversion H1;
      inversion H2
  end.
  destruct t; destruct t0.
  subst.
  inversion H5.
  inversion H7.
  inversion H8.
  inversion H13.
  inversion H15.
  inversion H16.
  subst.
  subst.
  repeat comp_pres_wt_tac.
  
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


