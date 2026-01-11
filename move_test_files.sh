#!/bin/bash

# 查找并移动包含 assert_eq 的测试文件到正确的测试目录

echo "查找并移动测试文件..."

# 查找 src/azimuth 目录中（不在 test 子目录）的包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src/azimuth -maxdepth 1 -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    filename=$(basename "$file")
    echo "移动文件: $file -> test/$filename"
    mv "$file" "/home/runner/work/Azimuth/Azimuth/src/azimuth/test/$filename"
done

# 查找 src/clean_test 目录中（不在 test 子目录）的包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src/clean_test -maxdepth 1 -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    filename=$(basename "$file")
    echo "移动文件: $file -> test/$filename"
    mv "$file" "/home/runner/work/Azimuth/Azimuth/src/clean_test/test/$filename"
done

echo "文件移动完成"