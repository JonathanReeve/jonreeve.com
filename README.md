# Ema Template

This repository represents a simple example of [Ema](https://ema.srid.ca/) — it generates a basic site with sidebar from a directory of Markdown files using Pandoc, Blaze HTML & TailwindCSS 3.x — and as such acts as a **template repository** to use for bootstrapping your next static site using Ema.

The generated HTML site can be previewed here: https://srid.github.io/jonreeve/

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` (`bin/run-via-tmux` if you have tmux installed) in terminal, to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Instructions

To run locally,

```
bin/run
```

To generate static site

```
nix build
./result/bin/joereeve gen /tmp
```
