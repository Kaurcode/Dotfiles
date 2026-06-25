$env.config.show_banner = false
$env.config.edit_mode = "vi"
$env.config.buffer_editor = "nvim"

# History:
# - sqlite gives better multi-session behavior
# - sync_on_enter shares history across running shells
# - isolation: false keeps history shared rather than session-isolated
$env.config.history = {
    file_format: sqlite
    max_size: 5000
    sync_on_enter: true
    isolation: false
}

# Completion / menus
$env.config.completions = {
    case_sensitive: false
    quick: true
    partial: true
    algorithm: "prefix"
    external: {
        enable: true
        max_results: 100
        completer: null
    }
}

# ----------------------------
# PATH
# ----------------------------

$env.PATH = ($env.PATH | prepend $"($env.HOME)/.local/bin")

# ----------------------------
# Yazi integration
# ----------------------------

def --env y [...args] {
    let tmp = (mktemp -t "yazi-cwd.XXXXXX")

    ^yazi ...$args --cwd-file $tmp

    let cwd = (open $tmp | str trim)

    if ($cwd | is-not-empty) and $cwd != $env.PWD {
        cd $cwd
    }

    rm -f $tmp
}

# ----------------------------
# Aliases
# ----------------------------

# Keep Nu-native ls, so no alias for ls.
# Also keeping Nu's built-in open/table-oriented tools instead of replacing cat.

alias diff = ^diff --color=auto
alias grep = ^grep --color=auto
alias ip = ^ip -c=auto

alias upgrade-system = sudo guix system -L /villa/kivilaak/Dotfiles reconfigure /villa/kivilaak/Dotfiles/xiug/config/systems/(^hostname | str trim).scm
alias update-system = guix pull --channels=/villa/kivilaak/Dotfiles/xiug/config/channels/channels.scm
alias upgrade-home = guix home -L /villa/kivilaak/Dotfiles reconfigure /villa/kivilaak/Dotfiles/xiug/config/home/home-config.scm

# ----------------------------
# External tool init
# ----------------------------

# zoxide:
source ~/.config/nushell/zoxide.nu
