## Installing nix on MacOS

Determinate Nix (https://determinate.systems/install) enables flakes by default,
so no nix.conf editing is needed after install.

Recommended (macOS) — graphical installer:
https://install.determinate.systems/determinate-pkg/stable/Universal

Alternative — terminal:
```
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Custom config: /etc/nix/nix.custom.conf (do not edit the generated /etc/nix/nix.conf).

## Installing home-manager

guide: https://nix-community.github.io/home-manager/index.xhtml#sec-flakes-standalone

```
nix run home-manager/release-26.05 -- init --switch
```

How to apply changes:

```
home-manager build 
home-manager switch
```

How to update home-manager:

```
cd ~/.config/home-manager/
nix flake update
home-manager build
home-manager switch
```
