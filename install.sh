#!/bin/bash

# List of all packages
PACKAGES=("emacs" "gitignore" "tmux" "zsh")
FLAGS="--verbose --no-fold --dotfiles --target=${HOME}"

# Function to restow all packages
restow_all() {
  echo "Restowing all packages: ${PACKAGES[*]}"
  stow ${FLAGS}  --restow "${PACKAGES[@]}"
}

# Function to delete all packages
delete_all() {
  echo "Deleting all packages: ${PACKAGES[*]}"
  stow ${FLAGS} --delete "${PACKAGES[@]}"
}

# Function to restow a single package
restow_package() {
  local package="$1"
  echo "Restowing package: ${package}"
  stow ${FLAGS} --restow "${package}"
}

# Function to delete a single package
delete_package() {
  local package="$1"
  echo "Deleting package: ${package}"
  stow ${FLAGS} --delete "${package}"
}

# Display usage information
usage() {
  echo "Usage: $0 {all|delete|PACKAGE|PACKAGE-delete}"
  echo ""
  echo "Commands:"
  echo "  all                  Restows all packages"
  echo "  delete               Deletes all packages"
  for pkg in "${PACKAGES[@]}"; do
    echo "  $pkg                 Restows the $pkg package"
   echo "  $pkg-delete          Deletes the $pkg package"
  done
  exit 1
}

# Main script logic
if [[ $# -eq 1 ]]; then
  case "$1" in
    all)
      restow_all
      ;;
    delete)
      delete_all
      ;;
    *)
      for pkg in "${PACKAGES[@]}"; do
        if [[ "$1" == "$pkg" ]]; then
          restow_package "$pkg"
          exit 0
        elif [[ "$1" == "$pkg-delete" ]]; then
         delete_package "$pkg"
          exit 0
        fi
      done
      usage
      ;;
  esac
else
  usage
fi
