---
layout: post
title: Git
date: 2026-04-21 22:15
modified_date: 2026-05-05 10:13
categories: git
lang: fr
---

## Commandes de base
- `git init`
- `git status`
  + `git status | less` : avec pagination (sous unix)
- `git log --pretty=format:"%h%x09%ad%x09%an%x09%s"`
  + liste abrégée des commits (1 ligne par commit) ([SE](https://stackoverflow.com/questions/1441010/the-shortest-possible-output-from-git-log-containing-author-and-date))
- `git add fichier` : stage
  + `git add -N fichier` : ajouter le fichier sans son contenu
    - utile pour quand tu veux ajouter seulement certaines lignes ou certains hunks. ajouter le fichier avec `add -N` d'abord, puis ajouter ce que tu veux avec `add -i` ou avec magit. ([SE](https://stackoverflow.com/questions/6436681/how-to-stage-only-part-of-a-new-file-with-git))
    - avec des fichiers déjà existants c'est possible d'ajouter les changements en entier puis unstage certaines lignes / certains hunks, mais avec un nouveau fichier ça provoque l'erreur "error: new file fichier depends on old contents".
- `git restore --staged fichier` : unstage
- `git rm --cached nomdufichier` : untrack
  + attention ! sans le `--cached` ça va supprimer le fichier du disque en même temps
  + `git rm --cached -r nomdudossier` : untrack un dossier avec tout ce qu'il y a dedans
    - utile quand je stage un dossier trop gros à un point où magit devient inutilisable. faut ajouter des trucs à .gitignore d'abord
- `git mv ancien nouveau` : déplacer/renommer un fichier
  + et il faut committer ce changement seul (sans modifier le contenu du fichier) pour ne pas casser l'historique
- `git commit` / `git commit -m "message"`
- `git commit --amend`
- `git clone https://github.com/{utilisateur}/{projet}.git`
- `git reset HEAD~` : annuler le commit précédent
  + il m'est arrivé de casser un dépôt avec ça, je pense que ce dépôt était déjà corrompu / reflog déglingué, mais n'empêche que je préfère de nos jours si besoin de modifier le dernier commit d'utiliser `git commit --amend` ou juste faire un autre commit et accepter que l'erreur va être dans l'historique.
- `git push` / `pull`

## Branches
- `git checkout -b nouvellebranche`
  + sans `-b` pour passer à une branche existante
  + `-d` pour supprimer

## Conventional commits
[conventionalcommits.org/fr/v1.0.0](https://www.conventionalcommits.org/fr/v1.0.0/)

- **feat** :: une nouvelle fonctionnalité
- **fix** :: une correction de bug
- **refactor** :: refactorisation
- **docs** :: documentation
- **perf** :: un changement pour améliorer les performances
- **test** :: ajout ou correction de tests automatiques
- **build** :: un changement lié au système de build ou les dépendances
- **ci** :: un changement dans la configuration de l'intégration continue (builds automatiques)
- **revert** :: annulation d'un commit
- **chore** :: un changement qui ne change pas les fichiers sources
- **style** :: mise en forme du code sans changer sa signification (linting)
- ajouter **!** en cas d'une modification incompatible
  + ajouter une explication en dernière ligne du message de commit. "BREAKING CHANGE: Describe the change"

Exemples :
- feat(setup): Add bundle build kind
- refactor: Single-source version
- fix(macOS): Qt5 bug workaround

{% include fin.html %}
