#!/bin/bash

# 修复所有测试文件中的断言函数

cd /home/runner/work/Azimuth/Azimuth

# 创建一个简单的示例文件，展示如何替换断言函数
cat > fix_assertions.py << 'EOF'
import sys
import re

def fix_assertions(content):
    # 替换 assert_eq
    content = re.sub(r'azimuth::assert_eq\(([^,]+),\s*([^)]+)\)', r'if \1 != \2 {\n    @builtin.panic()\n  }', content)
    
    # 替换 assert_eq_string
    content = re.sub(r'azimuth::assert_eq_string\(([^,]+),\s*([^)]+)\)', r'if \1 != \2 {\n    @builtin.panic()\n  }', content)
    
    # 替换 assert_true
    content = re.sub(r'azimuth::assert_true\(([^)]+)\)', r'if !\1 {\n    @builtin.panic()\n  }', content)
    
    # 替换 assert_false
    content = re.sub(r'azimuth::assert_false\(([^)]+)\)', r'if \1 {\n    @builtin.panic()\n  }', content)
    
    return content

# 读取文件
with open(sys.argv[1], 'r') as f:
    content = f.read()

# 修复断言
content = fix_assertions(content)

# 写回文件
with open(sys.argv[1], 'w') as f:
    f.write(content)
EOF

# 查找所有测试文件
TEST_FILES=$(find src -name "*.mbt")

for file in $TEST_FILES; do
  # 检查文件是否包含断言函数
  if grep -q "azimuth::assert_eq" "$file" || grep -q "azimuth::assert_eq_string" "$file" || grep -q "azimuth::assert_true" "$file" || grep -q "azimuth::assert_false" "$file"; then
    echo "Fixing assertions in $file..."
    python3 fix_assertions.py "$file"
  fi
done

rm fix_assertions.py

echo "Fixed assertions in all test files"