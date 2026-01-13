#!/bin/bash

# 最终解决方案：将所有测试文件修改为使用 test_helpers.mbt 中定义的函数

echo "Final solution: modifying test files to use functions from test_helpers.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

# 首先修改 moon.pkg.json，确保 test_helpers.mbt 在其他测试文件之前编译
cat > moon.pkg.json << 'EOF'
{
  "import": ["moonbitlang/core/builtin", "../azimuth"],
  "test-import": ["../azimuth"],
  "export": [],
  "test": ["test_helpers.mbt", "test_functions.mbt", "test_helper.mbt", "test_shared.mbt", "*.mbt"]
}
EOF

echo "Updated moon.pkg.json to ensure test_helpers.mbt is compiled first"

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_TEST_PATH"

# 修改 moon.pkg.json
cat > moon.pkg.json << 'EOF'
{
  "import": ["moonbitlang/core/builtin", "../clean_test"],
  "test-import": ["../clean_test"],
  "export": [],
  "test": ["test_helpers.mbt", "*.mbt"]
}
EOF

echo "Updated clean_test/test/moon.pkg.json to ensure test_helpers.mbt is compiled first"

echo "Final solution complete."