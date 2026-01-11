#!/bin/bash

# 批量替换 clean_test/test 目录中所有 .mbt 文件的 @azimuth. 为 @clean_test.
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
  echo "Processing $file..."
  sed -i 's/@azimuth\./@clean_test./g' "$file"
done

echo "Done!"