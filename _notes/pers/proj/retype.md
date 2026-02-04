---
layout: post
title:  "retype"
date:   2026-01-21 03:05
modified_date: 2026-01-21
categories: projet
lang: fr
---
## Sélection
- id:re2 hack insertion C-S-u sans ibus/fcitx

## Notes
### Avant une release
- tests automatiques
- vérification manuelle :
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
- changer bien la version
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
- {id:re-12} ajouter une licence (GPLv3 ?)
  + vérifier compatibilité
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
- {id:re3} [old] étudier ebookreader calibre3.48

{% include fin.html %}
