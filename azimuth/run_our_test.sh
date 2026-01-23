#!/bin/bash

cd /home/runner/work/Azimuth/Azimuth/azimuth
echo "Running azimuth_concise_tests.mbt..."

# 创建一个临时的 moon.pkg.json，只包含我们的测试文件
cp test/moon.pkg.json test/moon.pkg.json.backup

# 创建新的 moon.pkg.json 只包含我们的测试文件
cat > test/moon.pkg.json << EOF
{
  "import": [
    "azimuth"
  ],
  "export": [],
  "test-import": [
    "moonbitlang/core/builtin"
  ],
  "test": [
    "azimuth_concise_tests.mbt"
  ]
}
EOF

# 运行测试
/home/runner/work/Azimuth/Azimuth/moon test

# 恢复原始的 moon.pkg.json
mv test/moon.pkg.json.backup test/moon.pkg.json