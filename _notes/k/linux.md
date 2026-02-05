---
layout: post
title:  "Notes Linux"
date:   2026-01-16 22:01
modified_date: 2026-02-04 12:51
categories: os
lang: fr
---

## Basique mais j'oublie
- `pwd` : chemin actuel
- `time` : avant une commande pour mesurer son temps d'exécution
  + `-v` pour plus d'informations
- `df` : utilisation d'espace disque
  + avec `-H` pour le rendre plus lisible :-p
- `du -hd0 mondossier` : la taille d'un dossier ou fichier
  <br>(`-h` : human readable. `-d0` : depth 0, pareil à `-s` `--summarize`.)
   + `taille=$(du -kd0 "$nom" | cut -f 1)` : si besoin d'avoir juste un chiffre, par ex. pour un script 
     <br>(`-k` : block size 1K, c-à-d kilobits.)
- `stat monfichier` : les dates, taille, permissions d'un fichier
- `file monfichier` : le type d'un fichier
  + `--mime-type` (ex. `text/plain`)
  + `--mime-encoding` (ex. `utf-8`)
- Trouver un processus
  + `pgrep -x monprocessus` : pid d'un processus par nom exact
  + `ps aux | grep [m]onprocessus` : plus d'informations (cf les titres de colonnes `ps aux | head -n1`), possibilité de chercher par commande (possible aussi avec pgrep en utilisant `-af`)
    <br>les crochets autour de la première caractère permettent d'éviter de trouver le processus du grep aussi.
- `pkill monprocessus` : tuer gentillement
- `systemctl list-units --type=service --user --all` : lister services utilisateur
  <br>(c'est agaçant mais faut se souvenir du --user. de même pour `systemctl status monservice` et `journalctl -u monservice`)
- option `-f` (`--follow`) avec journalctl pour voir les messages en direct

### Pipes
- `.. | head -n1` : première ligne
- `.. | tail -n1` : dernière ligne
- `.. | less` : pager (permet de naviguer et chercher la sortie)
  + <kbd>/</kbd> : chercher
- `.. | awk '{print $1}'` : extraire une donnée dans position 1 quand tu as une ligne de sortie avec des données séparées par espace ou tab (avec plusieurs lignes ça extrait la donnée en position 1 de chacune)
- `echo "hi123" | sed 's/.\{3\}$//'` → hi
  <br>(traduction : remplacer 3 caractères avant la fin de ligne par rien)
  <br>version sans besoin d'échapper les accolades (extended regex) : `sed -E 's/.{3}$//`
- `.. | grep -e option1 -e option2` : grep OR
  <br>ou `.. | grep 'option1\|option2`, les guillemets sont nécessaire.
  <br>(cf [n0tes.fr Grep OR AND NOT](https://www.n0tes.fr/2022/10/16/Grep-OR-AND-NOT/))

### Redirection
- `.. > fichier 2>&1` : redirige stdout et stderr à un fichier
  + `.. > /dev/null 2>&1` : utilisé quand on ne veut pas de sortie

### Expansion / substitution
- [POSIX] `$((1 + 1))` : expansion arithmétique
- Bash : `$((++i))` ou `$((i++))`. POSIX : `i=$((i + 1))`
  + pas de `$` dedans. si on en ajoute ça s'expand avant l'expansion arithmétique : `i=1; $(($i++))` → `$((1++))` → erreur
- [POSIX] `${#var}` : longueur d'une variable
- [POSIX] `${var-valeur}` : valeur par défaut si `var` est non définie
  + `:-` : .. vide ou non définie
  + `=` : .. non définie et réaffecte la variable dans ce cas
  + `:=` : .. vide ou non définie et réaffecte la variable
  + `+` : l'inverse de `-`. "valeur" si `var` est définie.
  + `:+` : l'inverse de `:-`. "valeur" si `var` est définie et non vide
  + `?` : erreur si `var` est non définie
  + `:?` : erreur si `var` est vide ou non définie
  + on peut désaffecter une variable (la rendre non définie à nouveau) avec `unset var`
- [POSIX] `${var#motif}` `${var%motif}` : supprime préfixe/suffixe
  + `##` : plus long préfixe correspondant. `a=/par/exemple; echo ${a##/*}` : "exemple"
  + `%%` : plus long suffixe correspondant
- [Bash] `${var:2}` : valeur de `var` du 2e caractère jusqu'à la fin
- [Bash] `${var:2:5}` : valeur de `var` du 2e caractère jusqu'au 5e
  + on peut aussi donner des valeurs négatives pour indiquer une position par rapport à la fin. `-5:-2` : du caractère 5 avant la fin jusqu'à 2 avant la fin. mais attention, si le premier `:` est suivi par `-` ça va être pris comme `:-` (valeur par défaut ou non définie) alors il faut les séparer : `${var: -5:-2}`
    - ça ne marche pas pareil avec les args (`$@`) et les tableaux. avec une variable, `0:-1` donne tous les caractères sauf le dernier. avec args, `1:-1` ou `0:-1` entraîne une erreur "substring expression < 0", car là le deuxième paramètre est pris comme la longueur et devrait en conséquence être positif.
- [Bash] `${var/ancien/nouveau}` : remplacement d'une partie de la valeur, comme sed.
  + `//` : remplacer toutes les occurrences
  + `/#` : préfixe
  + `/%` : suffixe
- [Bash] `${var^} ${var,}` : conversion en majuscules ou miniscules, ou seulement une partie si suivi par un motif
  + `^^` `,,` : remplacer toutes les occurrences
  + [POSIX] `tr '[:lower:]' '[:upper:]'`
- [Bash] `${var@U}` : conversion en majuscules
  + `u` : conversion du premier caractère en majucule
  + `L` : conversion en miniscules
  + `Q` : quoted. comme `printf "'%s\n'" "$var"`
  + `K` `k` : comme `Q` mais pour un tableau donne aussi les indices
  + `E` : expansion des séquences d'échappement, comme si la valeur était entre `$''`
  + `P` : expansion des [séquences d'échappement du prompt](https://www.gnu.org/software/bash/manual/html_node/Controlling-the-Prompt.html)
  + `A` : donne la commande pour recréer la variable
  + `a` : attributs bash
    - rien : variable normale, `a` : tableau indexé, `A` : tableau associatif, `i` : int / un entier (mais seulement si déclarée explicitement avec `declare -i`), `x` : exportée (transmise aux processus fils comme env), `r` : readonly. une variable pourrait avoir plusieurs attributs.
    - `declare -ir var=3; echo ${var@a}` : ir
- [Bash] `${!var}` : expansion indirecte ; la valeur de `var` est prise comme le nom d'une autre variable, et le résultat est la valeur de celle-ci.
- [Bash] `${!a*}` / `${!a@}` : liste les variables dont le nom commence par "a"
- [Bash] `${!arr[@]}` / `${!arr[*]}` : indices du tableau `arr`
- [Bash] `${@:1:3}` : arguments 1-3
- [Bash] `${@:$#}` : dernier argument
  + (`$@` : tous les paramètres [cf aussi `$*`], `$#` : le nombre de paramètres)
  + [POSIX] `eval last="\$$#"` ([Anders](https://stackoverflow.com/a/54271792/18396947))
- [Bash] `${@:1:$# - 1}` : tous les arguments sauf le dernier
  + l'espace entre le `#` et `-` est nécessaire pour zsh
  + les args commencent par 1 car 0 est le nom du script
  + [Bash] `${*%${!#}}` : souvent le même résultat mais ça ne fait pas la même chose. supprime la valeur du dernier argument de la fin de tous les arguments. `1 2 3 4` → `1 2 3`, `14 24 34 4` → `1 2 3`.
  + [POSIX] ``eval set -- `awk 'BEGIN{for(i=1;i<'$#';i++) printf " \"$%d\"",i;}'` ``, puis `$@` va avoir tous les arguments sauf le dernier ([Anders](https://stackoverflow.com/a/54271792/18396947))

### Boucles
- Bash : `for ((i=0; i<10; i++)); do echo $i; done`  
  POSIX : `i=0; while [ $i -lt 10 ]; do echo $i; i=$((i+1)); done`

### Pas POSIX
Tableaux, exposants, modulo, certaines substitutions de variables (syntaxe `:1:2` [slicing], `/` [remplacement], `^` [conversion en majuscules], `,` [.. miniscules], `!` [expansion indirecte])

Pas de regex, mais il y a les motifs de globbing `*` `?` `[abc]` `[!abc]`.

Il y a heredocs (`<<`) mais pas de herestrings (`<<<`)

## Débogage
- `journalctl --no-pager --since "1 hour ago"`
- `strings -n 8` : pour voir des journals tronqués
- `sudo dmesg -HP` : messages du noyau (kernel ring buffer) human readable, no pager
- `coredumpctl list --no-pager` : liste de crashs. même si la génération de coredumps est désactivée.
  <br>l'affichage dépend de la taille de la fenêtre alors je mets mon terminal en plein écran avant.

### Xorg
- Les logs pour une session non-root se trouve dans `~/.local/share/xorg`. Si X est en cours, `Xorg.0.log` est le log de la session actuelle, et `Xorg.0.log.old` de la précédente.
  <br>Ou un autre numéro au lieu de 0 ; [ArchWiki](https://wiki.archlinux.org/title/Xorg) : «The logfiles are of the form Xorg.n.log with n being the display number.»
- Il y a aussi la sortie stdout et stderr à considérer. Certaines distributions la mettent dans `~/.xsession-errors`. La plupart des messages qui y apparaissent figurent également dans le log Xorg. Ce qui est important de vérifier est le début et la fin.
  <br>`startx > ~/.xsession-errors 2>&1`

## Gestion des processus
- pkill onedrive
- pstree | grep onedrive

## Fichiers
- Commande pour lister les fichiers avec date de création et modification, triés par création (birth) :
  ```bash
  alias lshorodatage='shopt -s dotglob && stat * --format "%.16w %.16y %n" | sort -n'
  ```

## Termes
- PAM : [Linux Pluggable Authentication Modules](https://wiki.archlinux.org/title/PAM). En dehors de Linux c'est un terme plus général qui veut dire gestion des accès privilégiés (Privileged Access Management)
- seat : TODO. qqch à voir avec VTs et qu'est-ce qui peut utiliser un tel ou tel VT ?
  <br>[article qui l'explique](https://dvdhrm.wordpress.com/2013/08/24/session-management-on-linux/) mais je ne comprends pas tout à fait
- VT vs tty ?
- DRM
- fbdev
- evdev
- fd : file descriptor
- ioctl
- sysfs
- dbus
- ibus

### Pas spécifique à Linux
...

## Recettes
### Utilisateur
#### Faire fonctionner le bouton d'alimentation
1. `sudo nano /etc/systemd/logind.conf`
2. Décommenter `HandlePowerKey=poweroff`
   - <kbd>Ctrl F</kbd> HandlePowerKey
   - Supprimer le `#` au début de la ligne
3. Sauvegarder (<kbd>Ctrl O</kbd>)

Le bouton d'alimentation n'est pas traité sur Arch Linux par défaut et il est important de le configurer, car un arrêt brutal peut corrompre des données. Ça arrive de devoir appuyer sur le bouton à cause d'un blocage autrement irrécupérable.

C'est systemd qui le gère.

Dans `/etc/systemd/logind.conf` tout est commenté par défaut. Décommente `HandlePowerKey=poweroff`. Tu y peux aussi configurer ce qui arrive avec d'autres boutons de contrôle système (Reboot, Suspend, Hibernate) et LidSwitch.

cf [ArchWiki : Power managemnt : ACPI events](https://wiki.archlinux.org/title/Power_management#ACPI_events)

#### Déplacer une fenêtre avec précision
```bash
# la fenêtre active
xdotool getwindowfocus windowmove 635 339
# ou :
wmctrl -r :ACTIVE: -e 0,500,500,-1,-1
# (les -1 veulent dire ne pas changer la taille)

# fenêtre par classe ou nom, où Peek est le nom de ma fenêtre
xdotool search --class Peek windowmove %@ 635 339 windowsize %@ 657 377
# ou :
wmctrl -r Peek -e 0,635,339,657,377

# liste des fenêtres avec IDs, classes, titres
wmctrl -lx
```
et avec `-lG` il y a aussi la géometrie de fenêtres, mais ce n'est pas exactes sous bspwm, cf [bspwm: La position d'une fenêtre](#la-position-dune-fenêtre)

#### Symlink
Voici quelques symlinks que j'utiliser pour pouvoir syncer facilement des données importantes avec [onedrive](https://github.com/abraunegg/onedrive) :
``` bash
# Dossier Dropbox sur un dual boot (changes nom d'utilisateur `pm`)
sudo ln -s /media/Windows/Users/pm/Dropbox/Apps ~/OneDrive/pnotes

# Backups de favoris firefox (change le chemin avec ton propre que tu peux voir dans `about:profiles` sur firefox)
sudo ln -s /home/pm/.mozilla/firefox/4afxl8gu.default-release/bookmarkbackups/ ~/OneDrive/backups/bookmarkbackups

# Site, pour ne pas perdre des changements en cours
sudo ln -s /media/Windows/Users/pm/dev/reps/plu5.github.io ~/OneDrive/backups/reps/plu5.github.io
```

{% include note.html content="
> [!NOTE]
> Je garde pas mal de mes données sur une partition ntfs pour pouvoir les accéder depuis n'importe quel système d'exploitation, car Linux supporte ntfs mais Windows ne supporte pas ext4.
>
> Il faut aussi noter que ça pourrait arriver qu'un dossier créé sous Windows ne soit pas accessible sous Linux. Ça arrive quand il y a une étiquette particulière [TODO: écrire davantage, cf log 4e]. ça m'a arrivé avec mon dossier Dropbox après une mise à jour de ce dernier. Ma solution était de déconnecter et réconnecter et le laisser renommer le dossier et en créer un nouveau, puis quitter avant que ça ne sync, déplacer le contenu du vieux dossier dans le nouveau, et relancer. Il se rend compte assez rapidement que tout est là et revient en mode veille. Il y a aussi un plugin pour pouvoir accéder des dossiers avec cette étiquette quand même [TODO mettre un lien] mais il n'est pas maintenu.
" %}

### bspwm
#### La position d'une fenêtre
flottante :
```bash
# où "Peek" est le nom de ma fenêtre
wid=$(wmctrl -l | grep Peek | awk '{print $1}'); bspc query --node $wid --tree | jq .client.floatingRectangle
```
si non flottante, juste `.rectangle`

### Script
#### Faire quelque chose seulement si l'utilisateur a la commande
``` bash
has_paplay=$(command -v paplay >/dev/null 2>&1 && echo true)
[ $has_paplay = true ] && paplay "/usr/share/sounds/freedesktop/stereo/complete.oga"
```

#### Parcourir les lignes d'un fichier
```bash
while read line; do
  echo $line
done < .bashrc  # ou n'importe quel fichier après le <
```

#### Parcourir les lignes de sortie d'une commande
```bash
ls -a | while read line; do   # ou n'importe quelle commande avant le |
  echo $line
done
```

#### Parcourir les noms de fichiers
```bash
for f in *; do
  echo $f
done
```
exclure les sous dossiers en testant la condition `[ -f "$f" ]`

Pour le faire recursivement, utiliser `find` :
```bash
find . | while read f; do
  echo $f
done
```

## Problèmes avec mon système
### X
- Figement irrécuperable si je change à un autre tty puis reviens au tty de X
- `xrdb ~/.Xresources` rarement tue le serveur X
- `sudo udevadm trigger` tue le serveur X

## Références
- [GNU Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)

{% include fin.html %}
