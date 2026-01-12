#!/bin/bash

# 批量修复测试文件中的函数调用
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

# 遍历所有.mbt文件
for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "simple_test_new.mbt" ] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    sed -e 's/if \([0-9-]*\) != \([0-9-]*\) + \([0-9-]*\) {/if \1 != azimuth.add(\2, \3) {/g' \
        -e 's/if \([0-9-]*\) != \([0-9-]*\) \* \([0-9-]*\) {/if \1 != azimuth.multiply(\2, \3) {/g' \
        -e 's/if "Hello, \([^!]*\)!" != "Hello, " + "\([^"]*\) + "!"/if "Hello, \1!" != azimuth.greet("\2")/g' \
        -e 's/if \([^ ]*\) != \([^ ]*\) + \([^ ]*\) {/if \1 != azimuth.add(\2, \3) {/g' \
        -e 's/if \([^ ]*\) != \([^ ]*\) \* \([^ ]*\) {/if \1 != azimuth.multiply(\2, \3) {/g' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "All test files have been processed."
