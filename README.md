# bhc.raskell.io

Website for [BHC](https://github.com/raskell-io/bhc) — the Basel Haskell Compiler.

**Live site:** https://bhc.raskell.io

## Overview

This repository contains the source for the BHC product website. Built with [Zola](https://www.getzola.org/) using the `brutalist-blueprint` theme.

BHC is an alternative compiler for Haskell: compatibility-first, with a modern runtime, multiple targets, and opt-in extensions.

## Development

### Prerequisites

- [Zola](https://www.getzola.org/documentation/getting-started/installation/) (0.18+)
- Or use [mise](https://mise.jdx.dev/) to manage the environment:
  ```bash
  mise install
  ```

### Local Development

```bash
# Start development server with live reload
zola serve

# Build for production
zola build

# Check for errors
zola check
```

The development server runs at `http://127.0.0.1:1111` by default.

## Project Structure

```
.
├── config.toml          # Zola configuration
├── content/             # Markdown content pages
├── static/              # Static assets (images, fonts, etc.)
├── templates/           # HTML templates (if overriding theme)
└── themes/
    └── brutalist-blueprint/  # Custom brutalist theme
        ├── sass/             # SCSS stylesheets
        └── templates/        # Theme templates
```

## Brand Guidelines

- **Project name:** Basel Haskell Compiler
- **Abbreviation:** BHC
- **Typeface:** Clash Display (brand), JetBrains Mono (code)

BHC is a compiler *for* Haskell, not a new language. Content should reflect compatibility-first positioning and respect for the Haskell community.

## Deployment

The site is automatically deployed on push to the main branch.

## License

MIT
