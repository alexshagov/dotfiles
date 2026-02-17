{
  description = "Pinned nix-ai-tools for claude-code";

  inputs = {
    nix-ai-tools.url = "github:numtide/nix-ai-tools";
  };

  outputs = { self, nix-ai-tools, ... }@inputs:
    let
      system = "aarch64-darwin";  # Change to your system: aarch64-linux, x86_64-darwin, or aarch64-darwin
    in
    {
      packages.${system} = {
        crush = nix-ai-tools.packages.${system}.claude-code;
        default = nix-ai-tools.packages.${system}.claude-code;
      };
    };
}
