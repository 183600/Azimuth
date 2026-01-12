#!/bin/bash

# 更新 moon.pkg.json 文件以包含所有测试文件
echo "Updating moon.pkg.json files..."

# 更新 azimuth/test/moon.pkg.json
cd src/azimuth/test

# 获取所有 .mbt 文件
files=$(ls *.mbt | sort | tr '\n' ' ')

# 创建新的 moon.pkg.json
cat > moon.pkg.json << EOF
{
  "import": ["moonbitlang/core/builtin"],
  "test-import": ["azimuth"],
  "export": [],
  "files": [$(echo "$files" | sed 's/[^ ]\+/"&"/g' | sed 's/ "/, "/g')]
}
EOF

echo "Updated azimuth/test/moon.pkg.json"

# 更新 clean_test/test/moon.pkg.json
cd ../../clean_test/test

# 获取所有 .mbt 文件
files=$(ls *.mbt | sort | tr '\n' ' ')

# 创建新的 moon.pkg.json
cat > moon.pkg.json << EOF
{
  "import": ["moonbitlang/core/builtin"],
  "test-import": ["clean_test"],
  "export": [],
  "files": [$(echo "$files" | sed 's/[^ ]\+/"&"/g' | sed 's/ "/, "/g')]
}
EOF

echo "Updated clean_test/test/moon.pkg.json"

cd ../../..
