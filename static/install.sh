#!/bin/sh
# BHC Installer Script
# https://bhc.raskell.io
#
# Usage:
#   curl -fsSL https://bhc.raskell.io/install.sh | sh
#   curl -fsSL https://bhc.raskell.io/install.sh | sh -s -- --uninstall
#
# Options:
#   --uninstall      Remove BHC and optionally clean shell config
#   --help           Show this help message
#
# Environment variables:
#   BHC_INSTALL_DIR  - Installation directory (default: ~/.bhc)
#   BHC_VERSION      - Specific version to install (default: latest)

set -e

# Configuration
GITHUB_REPO="raskell-io/bhc"
INSTALL_DIR="${BHC_INSTALL_DIR:-$HOME/.bhc}"
BIN_DIR="$INSTALL_DIR/bin"
LIB_DIR="$INSTALL_DIR/lib"
EXAMPLES_DIR="$INSTALL_DIR/examples"

# Colors (disabled if not a terminal)
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    BOLD='\033[1m'
    RESET='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    BOLD=''
    RESET=''
fi

info() {
    printf "${BLUE}info${RESET}: %s\n" "$1"
}

success() {
    printf "${GREEN}success${RESET}: %s\n" "$1"
}

warn() {
    printf "${YELLOW}warning${RESET}: %s\n" "$1"
}

error() {
    printf "${RED}error${RESET}: %s\n" "$1" >&2
    exit 1
}

# Detect OS
detect_os() {
    case "$(uname -s)" in
        Linux*)  echo "linux" ;;
        Darwin*) echo "darwin" ;;
        MINGW*|MSYS*|CYGWIN*) echo "windows" ;;
        *) error "Unsupported operating system: $(uname -s)" ;;
    esac
}

# Detect architecture
detect_arch() {
    case "$(uname -m)" in
        x86_64|amd64)  echo "x86_64" ;;
        aarch64|arm64) echo "aarch64" ;;
        *) error "Unsupported architecture: $(uname -m)" ;;
    esac
}

# Check for required commands
check_dependencies() {
    for cmd in curl tar; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            error "Required command not found: $cmd"
        fi
    done
}

# Get the latest version from GitHub (includes prereleases)
get_latest_version() {
    curl -fsSL "https://api.github.com/repos/$GITHUB_REPO/releases" \
        | grep '"tag_name"' \
        | head -1 \
        | sed -E 's/.*"([^"]+)".*/\1/'
}

# Download and install BHC
install_bhc() {
    local os="$1"
    local arch="$2"
    local version="$3"

    # Construct download URL
    local filename="bhc-${arch}-${os}.tar.gz"
    local url="https://github.com/$GITHUB_REPO/releases/download/${version}/${filename}"

    info "Downloading BHC ${version} for ${os}/${arch}..."

    # Create installation directory
    mkdir -p "$BIN_DIR"

    # Download and extract
    local tmp_dir
    tmp_dir=$(mktemp -d)
    trap "rm -rf '$tmp_dir'" EXIT

    if ! curl -fsSL "$url" -o "$tmp_dir/bhc.tar.gz"; then
        error "Failed to download BHC from $url"
    fi

    info "Extracting..."
    tar -xzf "$tmp_dir/bhc.tar.gz" -C "$tmp_dir"

    # Find and install the binary
    if [ -f "$tmp_dir/bhc" ]; then
        mv "$tmp_dir/bhc" "$BIN_DIR/bhc"
    elif [ -f "$tmp_dir/bhc-${arch}-${os}/bhc" ]; then
        mv "$tmp_dir/bhc-${arch}-${os}/bhc" "$BIN_DIR/bhc"
    else
        # Try to find bhc binary anywhere in extracted files
        local bhc_bin
        bhc_bin=$(find "$tmp_dir" -name "bhc" -type f | head -1)
        if [ -n "$bhc_bin" ]; then
            mv "$bhc_bin" "$BIN_DIR/bhc"
        else
            error "Could not find bhc binary in archive"
        fi
    fi

    chmod +x "$BIN_DIR/bhc"

    # Install RTS library if present
    mkdir -p "$LIB_DIR"
    if [ -d "$tmp_dir/lib" ]; then
        cp -r "$tmp_dir/lib/"* "$LIB_DIR/"
        info "RTS library installed to $LIB_DIR"
    elif [ -f "$tmp_dir/libbhc_rts.a" ]; then
        cp "$tmp_dir/libbhc_rts.a" "$LIB_DIR/"
        info "RTS library installed to $LIB_DIR"
    fi

    # Install examples if present
    if [ -d "$tmp_dir/examples" ]; then
        mkdir -p "$EXAMPLES_DIR"
        cp -r "$tmp_dir/examples/"* "$EXAMPLES_DIR/"
        info "Examples installed to $EXAMPLES_DIR"
    fi

    success "BHC ${version} installed to $BIN_DIR/bhc"
}

# Get shell config file and export syntax
get_shell_config() {
    local shell_name
    shell_name=$(basename "$SHELL")

    case "$shell_name" in
        bash)
            if [ -f "$HOME/.bashrc" ]; then
                SHELL_RC="$HOME/.bashrc"
            elif [ -f "$HOME/.bash_profile" ]; then
                SHELL_RC="$HOME/.bash_profile"
            else
                SHELL_RC="$HOME/.bashrc"
            fi
            PATH_EXPORT="export PATH=\"$BIN_DIR:\$PATH\""
            LIB_EXPORT="export LIBRARY_PATH=\"$LIB_DIR:\$LIBRARY_PATH\""
            ;;
        zsh)
            SHELL_RC="$HOME/.zshrc"
            PATH_EXPORT="export PATH=\"$BIN_DIR:\$PATH\""
            LIB_EXPORT="export LIBRARY_PATH=\"$LIB_DIR:\$LIBRARY_PATH\""
            ;;
        fish)
            SHELL_RC="$HOME/.config/fish/config.fish"
            PATH_EXPORT="set -gx PATH $BIN_DIR \$PATH"
            LIB_EXPORT="set -gx LIBRARY_PATH $LIB_DIR \$LIBRARY_PATH"
            ;;
        *)
            SHELL_RC=""
            PATH_EXPORT="export PATH=\"$BIN_DIR:\$PATH\""
            LIB_EXPORT="export LIBRARY_PATH=\"$LIB_DIR:\$LIBRARY_PATH\""
            ;;
    esac
}

# Check if BHC is already configured in shell
is_shell_configured() {
    if [ -z "$SHELL_RC" ] || [ ! -f "$SHELL_RC" ]; then
        return 1
    fi
    grep -q "$BIN_DIR" "$SHELL_RC" 2>/dev/null
}

# Configure shell automatically
configure_shell() {
    if [ -z "$SHELL_RC" ]; then
        return 1
    fi

    # Create parent directory if needed (for fish)
    mkdir -p "$(dirname "$SHELL_RC")"

    # Add BHC configuration block
    {
        echo ""
        echo "# BHC - Basel Haskell Compiler"
        echo "$PATH_EXPORT"
        echo "$LIB_EXPORT"
    } >> "$SHELL_RC"

    return 0
}

# Prompt user to configure shell
prompt_shell_config() {
    get_shell_config

    # If already configured, skip
    if is_shell_configured; then
        success "Shell already configured in $SHELL_RC"
        return
    fi

    echo ""

    # If not a terminal, just show manual instructions
    if [ ! -t 0 ]; then
        show_manual_instructions
        return
    fi

    # Interactive prompt
    if [ -n "$SHELL_RC" ]; then
        printf "${BOLD}Would you like to add BHC to your shell config?${RESET} [Y/n] "
        read -r response
        case "$response" in
            [nN][oO]|[nN])
                show_manual_instructions
                ;;
            *)
                if configure_shell; then
                    success "Added BHC to $SHELL_RC"
                    echo ""
                    info "Restart your shell or run:"
                    echo ""
                    printf "    ${BOLD}source %s${RESET}\n" "$SHELL_RC"
                else
                    warn "Could not configure shell automatically"
                    show_manual_instructions
                fi
                ;;
        esac
    else
        show_manual_instructions
    fi
}

# Show manual configuration instructions
show_manual_instructions() {
    get_shell_config
    echo ""
    if [ -n "$SHELL_RC" ]; then
        info "Add BHC to your PATH and LIBRARY_PATH by running:"
        echo ""
        printf "    ${BOLD}echo '%s' >> %s${RESET}\n" "$PATH_EXPORT" "$SHELL_RC"
        printf "    ${BOLD}echo '%s' >> %s${RESET}\n" "$LIB_EXPORT" "$SHELL_RC"
        echo ""
        info "Then restart your shell or run:"
        echo ""
        printf "    ${BOLD}source %s${RESET}\n" "$SHELL_RC"
    else
        info "Add BHC to your PATH and LIBRARY_PATH:"
        echo ""
        printf "    ${BOLD}%s${RESET}\n" "$PATH_EXPORT"
        printf "    ${BOLD}%s${RESET}\n" "$LIB_EXPORT"
    fi
    echo ""
}

# Check if BHC is already in PATH
check_existing_install() {
    if command -v bhc >/dev/null 2>&1; then
        local existing_path
        existing_path=$(command -v bhc)
        local existing_version
        existing_version=$(bhc --version 2>/dev/null | head -1 || echo "unknown")
        warn "BHC is already installed at $existing_path ($existing_version)"
        echo ""
    fi
}

# Show help
show_help() {
    cat << 'EOF'
BHC Installer - Basel Haskell Compiler
https://bhc.raskell.io

USAGE:
    curl -fsSL https://bhc.raskell.io/install.sh | sh
    curl -fsSL https://bhc.raskell.io/install.sh | sh -s -- [OPTIONS]

OPTIONS:
    --uninstall     Remove BHC installation
    --help          Show this help message

ENVIRONMENT VARIABLES:
    BHC_INSTALL_DIR     Installation directory (default: ~/.bhc)
    BHC_VERSION         Specific version to install (default: latest)

EXAMPLES:
    # Install latest version
    curl -fsSL https://bhc.raskell.io/install.sh | sh

    # Install specific version
    BHC_VERSION=v0.1.0 curl -fsSL https://bhc.raskell.io/install.sh | sh

    # Uninstall
    curl -fsSL https://bhc.raskell.io/install.sh | sh -s -- --uninstall
EOF
}

# Remove BHC config from shell rc file
remove_shell_config() {
    local rc_file="$1"
    if [ -f "$rc_file" ]; then
        # Create temp file without BHC lines
        grep -v "# BHC - Basel Haskell Compiler" "$rc_file" | grep -v "\.bhc" > "$rc_file.tmp" 2>/dev/null || true
        mv "$rc_file.tmp" "$rc_file"
        return 0
    fi
    return 1
}

# Uninstall BHC
uninstall_bhc() {
    echo ""
    printf "${BOLD}BHC Uninstaller${RESET}\n"
    printf "Basel Haskell Compiler - https://bhc.raskell.io\n"
    echo ""

    if [ ! -d "$INSTALL_DIR" ]; then
        warn "BHC is not installed at $INSTALL_DIR"
        exit 0
    fi

    # Show what will be removed
    info "This will remove:"
    echo "    - $INSTALL_DIR"
    echo ""

    # Prompt for confirmation if interactive
    if [ -t 0 ]; then
        printf "${BOLD}Are you sure you want to uninstall BHC?${RESET} [y/N] "
        read -r response
        case "$response" in
            [yY][eE][sS]|[yY])
                ;;
            *)
                info "Uninstall cancelled"
                exit 0
                ;;
        esac
    fi

    # Remove installation directory
    rm -rf "$INSTALL_DIR"
    success "Removed $INSTALL_DIR"

    # Offer to clean shell config if interactive
    if [ -t 0 ]; then
        echo ""
        printf "${BOLD}Would you like to remove BHC from your shell config?${RESET} [y/N] "
        read -r response
        case "$response" in
            [yY][eE][sS]|[yY])
                local cleaned=0
                for rc in "$HOME/.bashrc" "$HOME/.bash_profile" "$HOME/.zshrc" "$HOME/.config/fish/config.fish"; do
                    if remove_shell_config "$rc"; then
                        info "Cleaned $rc"
                        cleaned=1
                    fi
                done
                if [ "$cleaned" -eq 0 ]; then
                    info "No shell config files needed cleaning"
                fi
                ;;
            *)
                info "Shell config left unchanged"
                warn "You may want to manually remove BHC entries from your shell config"
                ;;
        esac
    else
        echo ""
        warn "You may want to manually remove BHC entries from your shell config"
    fi

    echo ""
    success "BHC has been uninstalled"
    echo ""
}

# Main install function
do_install() {
    echo ""
    printf "${BOLD}BHC Installer${RESET}\n"
    printf "Basel Haskell Compiler - https://bhc.raskell.io\n"
    echo ""

    check_dependencies
    check_existing_install

    local os arch version
    os=$(detect_os)
    arch=$(detect_arch)

    info "Detected platform: ${os}/${arch}"

    if [ -n "$BHC_VERSION" ]; then
        version="$BHC_VERSION"
        info "Using specified version: ${version}"
    else
        info "Fetching latest version..."
        version=$(get_latest_version)
        if [ -z "$version" ]; then
            error "Could not determine latest version"
        fi
        info "Latest version: ${version}"
    fi

    install_bhc "$os" "$arch" "$version"

    # Check if already in PATH
    if echo "$PATH" | tr ':' '\n' | grep -q "^$BIN_DIR$"; then
        success "BHC is ready! Run 'bhc --version' to verify."
    else
        prompt_shell_config
        info "After updating your PATH, run 'bhc --version' to verify."
    fi

    echo ""
}

# Main entry point
main() {
    # Parse arguments
    while [ $# -gt 0 ]; do
        case "$1" in
            --uninstall)
                uninstall_bhc
                exit 0
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                error "Unknown option: $1. Use --help for usage."
                ;;
        esac
        shift
    done

    # Default action: install
    do_install
}

main "$@"
