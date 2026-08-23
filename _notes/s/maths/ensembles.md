---
layout: post
title: Ensembles
date: 2026-05-03 02:51
modified_date: 2026-08-22 22:42
categories:
lang: fr
---

## A
### L'axiome d'extensionalité
deux ensembles qui ont les mêmes éléments sont égaux.

(Ɐx, x∈E ⇔ x∈F) ⇒ E=F

ça veut dire qu'un ensemble qui contient des éléments donnés est unique (un "autre" ensemble qui contient les mêmes éléments va être le même ensemble). autrement dit, il n'existe pas deux ensembles différents qui contiennent les mêmes éléments.
### L'axiome de séparation
### Divers
- ∃!E : Ɐx, x∉E
  + il existe un ensemble unique qui n'admet aucun élément.
- x∈{a,b} ⇔ (x=a ou x=b) ⇔ (x=b ou x=a) ⇔ x∈{b,a}
  + {a,b} est une paire. à ne pas confondre avec le couple (a,b) ; on n'a (a,b) = (b,a) que si a=b.
- ∅ ≠ {∅}
- {uₙ | n∈ℕ} : ensemble d'une suite
  + l'ensemble des uₙ tels que n appartient à ℕ
  + suite : (uₙ)_{n∈ℕ}
- il n'existe pas d'ensemble de tous les ensembles
- l'ensemble vide est un sous-ensemble de tout ensemble
- tout ensemble est sous-ensemble de lui-même (⊂ est réflexive)
- ⊂ est aussi transitive et antisymétrique
- E ∪ F ≔ {x | x ∈ E ou x ∈ F}
  + union / réunion
  + "ou" non exclusif
  + E ∪ (F ∪ G) = (E ∪ F) ∪ G  (associativité)
  + E ∪ F = F ∪ E                (commutativité)
  + E ∪ ∅ = ∅ ∪ E = E           (neutralité de l'ensemble vide)
  + E ∪ E = E                     (idempotence)
  + E ⊂ F ⇔ E ∪ F = F
    - (F contient tous les éléments de E, donc l'ensemble où tout x est dans E ou F est l'ensemble F)
    - preuve par double inclusion : ① x∈(E∪F) ⇔ (x∈E ou x∈F). on sait que E⊂F donc si x∈E, x∈F. dans les deux cas alors on a x∈F. ça veut dire E∪F est un sous-ensemble de F ; (E∪F)⊂F. tout élément de F est aussi dans E∪F par définition, donc F est un sous-ensemble de E∪F ; F⊂(E∪F). ((E∪F)⊂F et F⊂(E∪F)) ⇔ E∪F = F. ② maintenant il faut démontrer l'inverse, partir de E∪F = F et arriver à E⊂F. E∪F = F implique que tout élément de E est aussi dans F, puisque E∪F inclut les deux. ça veut dire que E est un sous-ensemble de F ; E⊂F.
- E ∩ F ≔ {x | x ∈ E et x ∈ F}
  + intersection
  + E et F sont disjoints si leur intersection est vide
  + E ∩ (F ∩ G) = (E ∩ F) ∩ G    (associativité)
  + E ∩ F = F ∩ E                (commutativité)
  + E ∩ ∅ = ∅ ∩ E = E           (neutralité de l'ensemble vide)
  + E ∩ E = E                     (idempotence)
  + E ⊂ F ⇔ E ∩ F = E
    - (F contient tous les éléments de E, donc l'ensemble où tout x est dans E et F tous deux est l'ensemble E)
    - preuve par double inclusion : ① E∩F est par définition un sous-ensemble de E puisqu'il contient uniquement les éléments inclus dans les deux ensembles ; (E∩F)⊂E. on a aussi E⊂(E∩F) car E⊂F et E⊂E. ((E∩F)⊂E et E⊂(E∩F)) ⇔ E∩F = E. ② E∩F = E implique que E ne contient pas d'éléments qui ne sont pas aussi dans F, ce qui veut dire qu'il est un sous-ensemble ; E⊂F.
  + E ∩ (F ∪ G) = (E ∩ F) ∪ (E ∩ G)  (distributivité)
  + E ∪ (F ∩ G) = (E ∪ F) ∩ (E ∪ G)  (^)
- E ∖ F ≔ {x | x ∈ E et x ∉ F}
  + différence
  + lorsque F ⊂ E, l'ensemble E ∖ F est appelé complémentaire de F dans E et noté ∁_EF
    - (E contient tous les éléments de F, et E ∖ F est E sans les éléments de F)
## Réfs
- _Mathématiques Tout-en-un pour la Licence 1_ 4e 2022

{% include fin.html %}
