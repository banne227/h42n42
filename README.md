# H42N42

Simulation d'une population de Creets menacées par le virus H42N42, écrite en OCaml côté client et compilée en JavaScript avec js_of_ocaml.

## Règles du jeu

- Les Creets se déplacent et rebondissent sur les bords de la fenêtre.
- La rivière en haut rend malade les Creets qui la touchent (elles ralentissent de 15 %).
- Une Creet malade contamine les Creets saines à son contact (2 % par itération).
- Une Creet malade peut devenir **Berserk** (10 %) : elle grossit jusqu'à quadrupler sa taille.
- Une Creet malade peut devenir **Mean** (10 %) : elle rétrécit et court après les Creets saines.
- Le joueur attrape une Creet avec la souris et la dépose dans la zone grise du bas. Une Creet malade déposée à l'hôpital est guérie.
- Une Creet saine se reproduit toutes les quelques secondes.
- La vitesse augmente avec le temps.
- La partie se termine quand il ne reste plus aucune Creet saine (GAME OVER).

## Prérequis

- [opam](https://opam.ocaml.org/doc/Install.html), le gestionnaire de paquets OCaml
- Un switch OCaml récent (OCaml 5.x)
- Un navigateur web moderne

## Installation

Crée un switch si tu n'en as pas encore :

```bash
opam switch create 5.2.0
eval $(opam env)
```

Installe les dépendances :

```bash
opam install dune js_of_ocaml js_of_ocaml-ppx js_of_ocaml-lwt lwt tyxml
```

Vérifie que les bibliothèques sont bien présentes :

```bash
ocamlfind list | grep js_of_ocaml
```

## Compilation

Depuis la racine du projet :

```bash
dune build
mkdir -p dist
cp client/index.html dist/
cp _build/default/client/main.bc.js dist/
```

## Lancement

Ouvre le fichier `dist/index.html` dans ton navigateur.

Pour voir les messages de débogage, ouvre la console du navigateur (F12, onglet Console).

## Structure du projet

```
h42n42/
├── dune-project
├── README.md
└── client/
    ├── dune          # configuration de la compilation JavaScript
    ├── index.html    # page qui charge le jeu
    ├── creet.ml      # modèle et comportement d'une Creet
    └── main.ml       # création des Creets, souris, fin de partie
```

## Dépannage

- **`Unbound module Lwt_js`** : utilise `open Js_of_ocaml_lwt` au lieu de `Lwt_js` directement.
- **Page blanche** : vérifie que `main.bc.js` est bien copié dans `dist/` à côté de `index.html`, puis recharge la page.
- **Anciens changements non visibles** : relance `dune build` puis recopie `main.bc.js` dans `dist/`.