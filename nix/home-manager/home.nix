{ config, pkgs, nix-devin-cli, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "<USERNAME>";
  home.homeDirectory = "/Users/<USERNAME>";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "26.05"; # Please read the comment before changing.

  home.packages = with pkgs; [
    git
    emacs
    bash
    direnv
    ripgrep
    htop
    inetutils
    util-linux
    jq
    fd
    fzf
    bat
    tree
    ffmpeg

    tmux

    # AI coding agents (provided by the llm-agents overlay in flake.nix).
    # binaries installed: claude, opencode
    llm-agents.claude-code
    llm-agents.opencode

    # Devin CLI (provided by the nix-devin-cli flake in flake.nix).
    nix-devin-cli.packages.${pkgs.system}.default
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/<USERNAME>/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = "emacs";
  };

  programs.ghostty = {
    enable = true;
    package = pkgs.ghostty-bin;

    settings = {
      macos-option-as-alt = true;
      theme = "Arthur";
      font-size = 14;
      font-thicken = true;

      keybind = [
	"shift+enter=text:\\x1b\\r"
      ];
    };
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    
    shellAliases = {
      gs = "git status";
      gc = "git checkout";
      be = "bundle exec";
    };

    # Additional Zsh configuration
    initContent = ''

      # Private ENV vars configuration
      if [ -f ~/.bashrc_private ]; then
        . ~/.bashrc_private
      fi

      # ========= Prompt with branch ========
      function git_branch_name() {
	       git rev-parse --abbrev-ref HEAD 2>/dev/null || echo '='
      }
      autoload -Uz colors && colors
      setopt prompt_subst
      prompt='%F{yellow}%~/%f [%F{green}$(git_branch_name)%f] > '
    '';
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    historyLimit = 100000;
    mouse = true;
    keyMode = "emacs";
    prefix = "C-z";
    baseIndex = 1;
    escapeTime = 0;
    shell = "${pkgs.zsh}/bin/zsh";

    extraConfig = ''
      set -g default-shell "${pkgs.zsh}/bin/zsh"
      set -g default-command "exec ${pkgs.zsh}/bin/zsh"

      set -g set-clipboard on
      set -g focus-events on

      bind c new-window
      bind n next-window
      bind p previous-window

      bind s choose-tree -Zw
      bind-key -T choose-tree d kill-window

      bind | split-window -h
      bind - split-window -v

      set -g default-terminal "tmux-256color"
      set-option -sa terminal-features ",tmux-256color:RGB"
      set-option -sa terminal-overrides ",xterm-256color:Tc"

      set -s extended-keys on
      set -s extended-keys-format csi-u
      set -as terminal-features ",xterm-ghostty:extkeys"

      set -g display-panes-time 1000
      set -g buffer-limit 20
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
