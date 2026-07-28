{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # AI coding agents (formerly numtide/nix-ai-tools).
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs = { nixpkgs, home-manager, llm-agents, ... }:
    let
      inherit (nixpkgs) lib;
      system = "aarch64-darwin";
      username = "<USERNAME>";
    in {
      homeConfigurations."${username}" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};

        modules = [
          {
            # Allow only the unfree AI tools; everything else stays
            # allowUnfree = false by default (the nixpkgs default).
            nixpkgs.config.allowUnfreePredicate = pkg:
              builtins.elem (lib.getName pkg) [
                "claude-code"
              ];

            # Exposes pkgs.llm-agents.<name> in home.nix.
            nixpkgs.overlays = [
              llm-agents.overlays.shared-nixpkgs
            ];
          }
          ./home.nix
        ];
      };
    };
}
