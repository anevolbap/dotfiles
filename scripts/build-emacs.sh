#!/bin/bash
set -euo pipefail

EMACS_SRC="${EMACS_SRC:-$HOME/src/emacs}"
EMACS_BRANCH="${EMACS_BRANCH:-emacs-31}"

echo "🔨 Rebuilding Emacs ($EMACS_BRANCH) for XFCE/X11..."

# Install dependencies (build-essential first so gcc is available for version detection)
echo "📦 Installing build toolchain..."
sudo apt-get update
sudo apt-get install -y build-essential

GCC_MAJOR=$(gcc -dumpversion | cut -d. -f1)
echo "📦 Installing remaining dependencies (matching libgccjit-${GCC_MAJOR}-dev)..."
sudo apt-get install -y \
    libgtk-3-dev \
    libgnutls28-dev \
    libtiff-dev \
    libgif-dev \
    libjpeg-dev \
    libpng-dev \
    libxpm-dev \
    libncurses-dev \
    texinfo \
    libharfbuzz-dev \
    "libgccjit-${GCC_MAJOR}-dev" \
    libtree-sitter-dev \
    libsqlite3-dev \
    libwebkit2gtk-4.1-dev \
    librsvg2-dev \
    liblcms2-dev \
    libsystemd-dev \
    libxml2-dev \
    zlib1g-dev

# Navigate to source
[ -d "$EMACS_SRC" ] || { echo "missing $EMACS_SRC"; exit 1; }
cd "$EMACS_SRC"

# Update
echo "🔄 Updating source..."
git fetch --tags
git switch "$EMACS_BRANCH"
git pull --ff-only

# Clean (fix any root-owned leftovers from previous sudo make install, then wipe)
echo "🧹 Cleaning previous build artifacts..."
sudo chown -R "$(id -un):$(id -gn)" .
git clean -fdx

# Generate
./autogen.sh

# Configure - X11/GTK3 build (perfect for XFCE)
echo "⚙️  Configuring for X11/XFCE..."
./configure \
    --with-native-compilation=aot \
    --with-tree-sitter \
    --with-gnutls \
    --with-zlib \
    --with-modules \
    --with-threads \
    --with-xwidgets \
    --with-x-toolkit=gtk3 \
    --with-xft \
    --with-cairo \
    --with-rsvg \
    --with-gif \
    --with-jpeg \
    --with-png \
    --with-tiff \
    --with-xpm \
    --with-xml2 \
    --with-sqlite3 \
    --with-lcms2 \
    --with-harfbuzz \
    --with-libsystemd \
    --without-compress-install \
    --prefix=/usr/local

# Build
echo "🔨 Building with $(nproc) cores..."
time make -j"$(nproc)"

# Install
echo "📦 Installing..."
sudo make install

# Verify
echo "✅ Build complete!"
emacs --version
echo ""
echo "Features:"
emacs --batch --eval '
(dolist (check (list
  (cons "Native compilation" (and (fboundp (quote native-comp-available-p)) (native-comp-available-p)))
  (cons "Tree-sitter" (treesit-available-p))
  (cons "JSON" (fboundp (quote json-parse-string)))
  (cons "SQLite" (fboundp (quote sqlite-open)))
  (cons "Xwidgets" (fboundp (quote make-xwidget)))))
  (princ (format "%s: %s\n" (car check) (if (cdr check) "✓" "✗"))))'
