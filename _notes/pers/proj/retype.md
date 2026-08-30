---
layout: post
title:  "retype"
date:   2026-01-21 03:05
modified_date: 2026-08-27 06:47
categories: projet
lang: fr
---
## Sélection
- id:re2 hack insertion C-S-u sans ibus/fcitx

## Notes
### Avant une release
- tests automatiques
  + `pytest -s`. `-s` permet de voir stdout et traceback
- **changer bien la version**
  + (bump version file)
- vérification manuelle :
  + raccourcis par défaut
  + taper tous les livres inclus
  + ajouter un library path, taper un livre importé
  + finir un livre
  + bookview : URLs
  + bookview : toolbar actions
  + configuration : tous les paramètres
  + configuration : restore defaults
  + configuration : revert
  + configuration : thèmes
  + jeux : sténo, typespeed
  + console : commandes
  + about dialog : tous les onglets
  + stats dock
- si besoin de sauts de lignes dans l'annotation du tag : f12 et changer l'input en textarea (cf log 2025-10-21)

## Vrac
§ 2024-2025
- {id:re-1} 100 % de couverture de tests
  + comment contourner les problèmes pyqt avec les tests unitaires ?
- {id:re-2} [perf] mémoïsation : mise en cache des données des livres traités
  + les fichiers pourraient changer alors peut-être invalider le cache si la date de modification du fichier a changé.
- {id:re-3} [perf] chargement non bloquant des livres
  + montrer une barre de chargement ou une autre indication.
- {id:re-4} [err] json parse failure boîte de dialogue d'erreur lors du chargement config/save, comme mwin
- {id:re-5} corriger stats dock idle timer reset pour que ce soit moins brusque
- {id:re-6} [ui] la barre latérale de customisationdialog a une apparence différente source vs build
- {id:re-7} [conf] police de modeline y compris la taille
- {id:re-8} [ui] intégration wiktionary, wikipedia, google translate ; comme foliate.
- {id:re-9} [docs] mettre à jour la documentation (evergreen)
- {id:re-10} [conf] intervalle de autosave où 0 veut dire désactiver cette fonctionnalité
- {id:re-11} [ci] modifier workflow pour utiliser la version de python la plus récente ?
- ✓ {id:re-12} ajouter une licence (GPLv3 ?)
  + vérifier compatibilité
  + 2026-05-21 je crois que puisque j'utilise du code GPLv3 (calibre par exemple) je dois l'utiliser, même si j'aurais voulu LGPL / une licence libre qui permet n'importe quelle utilisation pour mon propre code.
  + pourrais-je dire : "the project is licenced GPL3 because it must be for licence-compatibility reasons, but any code contributed by me, plu5, and not marked as taken from somewhere else, is do-whatever-the-fuck-you-want-with-it licence" ?
  + c'est bien compliqué. pq si je le mets sous GPL ça ne résout pas tout, car à priori si on modifie du code LGPL il faut publier ça sous LGPL
  + 2026-05-27 j'ai fini par recenser tous les fichiers et leur provenance, contributeurs, licences, mis mes parties sous CC0 / domaine public, et la distribution entière sous GPLv3 car c'est ce qui applique selon mes dépendances. (le tout dans un fichier COPYING).
- {id:re-13} [err] top-level try/catch avec boîte de dialogue d'erreur qui permet de copier les détails pour faciliter le reporting
- {id:re-14} [bug] macos configuration dialog hauteur inappropriée des widgets intérieurs
- {id:re-15} [ui] barre de navigation rapide comme foliate a en bas (apparaît au survol)
- {id:re-16} importer une page web (conversion en epub)
  + importer du texte depuis le presse-papiers
- {id:re-17} [docs] vidéo de démonstration du logiciel
- {id:re-18} [ci] faire échouer le workflow de release si le tag existe déjà (éviter de remplacer une release existante)
- {id:re-19} plu5/retype#42 [ui] triage des livres

§ 2026-01-02
- {id:re0} plu5/retype#46 investiguer pourquoi le highlighting est inversé dans des livres RTL

§ 2026-01-14
- {id:re1} dépôt retype-extended-library avec d'autres livres comme ceux inclus
  + possibilitié de les ajouter facilement depuis l'interface utilisateur
  + classés par langue

§ 2026-01-18
- {id:re2} hack similaire à my/insert-unicode-char pour faire marcher l'insertion C-S-u même sans ibus/fcitx

§ 2026-01-20
- {id:re3} [old] [s] étudier ebookreader calibre3.48

§ 2026-02-22
- {id:re4} [s] étudier [speed-type](https://github.com/dakra/speed-type) (melpa)

§ 2026-05-15
- ✓ {id:re5} raccourcis clavier configurables pour les commandes de console ([retype#47](https://github.com/plu5/retype/issues/47))
- {id:re6} aliases configurables pour les commandes de console (retype#47)

§ 2026-05-16
- ✓ {id:re7} [bug] skipline commande de console executée rapidement désynchronise du début de la ligne ([retype#48](https://github.com/plu5/retype/issues/48))
- ✓ {id:re8} [feat] Customisation Dialog page pour customiser les raccourcis clavier (retype#48)
- ✓ {id:re9} [feat] possibilité d'assigner des raccourcis clavier aux commandes (retype#48)
- ✓ {id:re10} [feat] possibilité d'assigner des raccourcis clavier aux commandes avec arguments, par ex. `chapter 0` pour avoir un raccourci pour passer au premier chapitre (retype#48)
- ✓ {id:re11} [bug] commande `cursor` supprime du texte (retype#48)
- ✓ {id:re12} commandes tests unitaires (retype#48)
  + il y en a déjà. et les commandes dans commandservice ne font que invoquer une fonction ailleurs, alors ce n'est pas trop utile
- {id:re13} [feat] commande remplir le caractère suivant si tout est correct jusqu'ici ([retype#49](https://github.com/plu5/retype/pull/49))
- {id:re14} [feat] mode de saisie caractère par caractère vs ligne par ligne (retype#49)
- {id:re15} [qol] si la fenêtre est caché et restauré, la position de défilement est incorrecte

§ 2026-05-17
- {id:re16} [bug] gotoCursorPosition ne défile pas à la bonne position s'il y a des images

§ 2026-05-20
- {id:re17} [feat] customisation aliases (#48)
  + doublon de {id:re6}
- {id:re18} [old] traductions
- {id:re19} [ci] supprimer l'entrée de la version et la prendre directement du versionfile, pour éviter que j'oublie de le mettre à jour (ça échouerait si la version a déjà une release)

§ 2026-05-22
- ✓ {id:re20} [feat] setChapter -1 pour passer au dernier chapitre
  + en fait ça marche déjà

§ 2026-05-27
- ✓ {id:re21} [qol] raccourcis pour naviguer entre les chapitres (C-left C-right ?)
  + non car conflit avec next/prev word non ? sauf que ça ne marcherait que si BookView est en focus... ce qui n'est quasiment jamais le cas car on tape dans la console. on devrait faire en sorte que les raccourcis BookView marchent dans la console alors ? en fait c'est déjà le cas pour certains raccourcis (C-l, C-n, ...)
  + PgUp PgDown, et avec ctrl pour déplacer le curseur
  + 2026-05-28 implementé

§ 2026-08-11
- menu add : local file, web page, interlacer

{% include fin.html %}
