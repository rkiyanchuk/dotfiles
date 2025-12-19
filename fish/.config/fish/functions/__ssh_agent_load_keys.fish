# Automatically load SSH key before running git commands
#
# Configuration:
#   Set SSH_AUTO_LOAD_KEY environment variable to specify a custom key path.
#   Default: ~/.ssh/id_ed25519

function __ssh_agent_load_keys --on-event fish_preexec --argument cmdline
    # Only run if command starts with git
    string match -qr '^git\s' -- $cmdline; or return

    # Only run on macOS
    test (uname) = "Darwin"; or return

    # Get key path from environment variable or use default
    set -l key_path (set -q SSH_AUTO_LOAD_KEY; and echo $SSH_AUTO_LOAD_KEY; or echo $HOME/.ssh/id_ed25519)

    # Check if key file exists
    test -f $key_path; or return

    # Check if key is already loaded
    ssh-add -l 2>/dev/null | grep -q (basename $key_path); and return

    # Add key with macOS keychain integration
    ssh-add --apple-use-keychain $key_path 2>/dev/null
end
