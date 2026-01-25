#!/bin/sh
# BHC Installer Script
# https://bhc.raskell.io
#
# Usage:
#   curl -fsSL https://bhc.raskell.io/install.sh | sh
#
# Environment variables:
#   BHC_INSTALL_DIR  - Installation directory (default: ~/.bhc)
#   BHC_VERSION      - Specific version to install (default: latest)

set -e

# Configuration
GITHUB_REPO="raskell-io/bhc"
INSTALL_DIR="${BHC_INSTALL_DIR:-$HOME/.bhc}"
BIN_DIR="$INSTALL_DIR/bin"

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

# Get the latest version from GitHub
get_latest_version() {
    curl -fsSL "https://api.github.com/repos/$GITHUB_REPO/releases/latest" \
        | grep '"tag_name"' \
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

    success "BHC ${version} installed to $BIN_DIR/bhc"
}

# Detect shell and suggest PATH addition
suggest_path() {
    local shell_name
    shell_name=$(basename "$SHELL")

    local path_export="export PATH=\"$BIN_DIR:\$PATH\""
    local rc_file=""

    case "$shell_name" in
        bash)
            if [ -f "$HOME/.bashrc" ]; then
                rc_file="$HOME/.bashrc"
            elif [ -f "$HOME/.bash_profile" ]; then
                rc_file="$HOME/.bash_profile"
            fi
            ;;
        zsh)
            rc_file="$HOME/.zshrc"
            ;;
        fish)
            path_export="set -gx PATH $BIN_DIR \$PATH"
            rc_file="$HOME/.config/fish/config.fish"
            ;;
    esac

    echo ""
    if [ -n "$rc_file" ]; then
        info "Add BHC to your PATH by running:"
        echo ""
        printf "    ${BOLD}echo '%s' >> %s${RESET}\n" "$path_export" "$rc_file"
        echo ""
        info "Then restart your shell or run:"
        echo ""
        printf "    ${BOLD}source %s${RESET}\n" "$rc_file"
    else
        info "Add BHC to your PATH:"
        echo ""
        printf "    ${BOLD}%s${RESET}\n" "$path_export"
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

# Main
main() {
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
        suggest_path
        info "After updating your PATH, run 'bhc --version' to verify."
    fi

    echo ""
}

main "$@"
