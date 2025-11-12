#!/bin/bash
# ---------------------------------------------------------------------
# clean_nfs.sh — safely remove orphaned .nfs* files on CHPC systems
# ---------------------------------------------------------------------
# Usage:
#   bash clean_nfs.sh /path/to/folder
# Example:
#   bash clean_nfs.sh /uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/Vulcan/VULCAN_WGS84_ncfiles
# ---------------------------------------------------------------------

TARGET_DIR=${1:-$(pwd)}   # default: current directory
echo "🔍 Scanning directory: $TARGET_DIR"

# Find all .nfs files
NFS_FILES=$(find "$TARGET_DIR" -type f -name ".nfs*" 2>/dev/null)

if [ -z "$NFS_FILES" ]; then
  echo "✅ No .nfs files found — all clean!"
  exit 0
fi

echo "⚠️ Found the following .nfs files:"
echo "$NFS_FILES"
echo ""

# Loop through each .nfs file
for FILE in $NFS_FILES; do
  echo "Processing: $FILE"

  # Check which process is using it
  PID=$(lsof "$FILE" 2>/dev/null | awk 'NR==2 {print $2}')

  if [ -n "$PID" ]; then
    echo "  → File is held by process PID: $PID"
    PROC_NAME=$(ps -p "$PID" -o comm=)
    echo "    Process: $PROC_NAME"
    echo "    Attempting to kill..."
    kill -9 "$PID" 2>/dev/null && echo "    ✅ Process $PID killed." || echo "    ⚠️ Failed to kill process."
  else
    echo "  → No active process found holding the file."
  fi

  # Try to delete file
  rm -f "$FILE" && echo "    🗑️ Removed $FILE" || echo "    ❌ Could not remove $FILE"
  echo ""
done

echo "✅ Cleanup complete."
