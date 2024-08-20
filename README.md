## Installing nix on MacOS

guide: https://nixos.org/download/#nix-install-macos
```
$ sh <(curl -L https://nixos.org/nix/install)
```

#### Installing flakes

Add the following to the ~/.config/nix/nix.conf or /etc/nix/nix.conf files:
```
experimental-features = nix-command flakes
```

## Installing home-manager

guide: https://nix-community.github.io/home-manager/index.xhtml#sec-flakes-standalone

```
nix run home-manager/release-24.05 -- init --switch
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
