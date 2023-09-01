#!/usr/bin/env bash

set -e

# Source directory
src="$1"
# Destination directory
dst="$2"

# Ensure destination exists
mkdir -p "$dst"

# Recursive function to handle and process the symlinks and directories
process_dir() {
    local current_src="$1"
    local current_dst="$2"

    for item in "$current_src"/*; do

        local relpath=${item#$src}
        local target="$current_dst/$relpath"

        # Handle type 1 links (with "target" files)
        if [[ -e "$item/target" ]]; then
            local tgt_path=$(cat "$item/target")
            ln -sf "$tgt_path" "$target"

        # Handle type 2 links (with "encodings/identity" files)
        elif [[ -e "$item/encodings/identity" ]]; then
            local identity_path=$(readlink -f "$item/encodings/identity")
            ln -sf "$identity_path" "$target"

        elif [[ -d "$item" ]]; then
          local relpath=${item#$src}
          local target_dir="$current_dst/$relpath"
          mkdir -p "$target_dir"

          # Recurse into this directory
          process_dir "$item" "$current_dst"
        fi

    done
}

# Start processing from the source directory
process_dir "$src" "$dst"

echo "Done!"
