#!/bin/bash
echo "运行新创建的 Azimuth 标准测试用例..."
cd /home/runner/work/Azimuth/Azimuth

# 创建一个临时的 moon.pkg.json，只包含我们的测试文件
cp azimuth/moon.pkg.json azimuth/moon.pkg.json.backup 2>/dev/null || true

# 创建简化的配置文件
cat > azimuth/moon.pkg.json << 'EOF'
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["lib.mbt", "azimuth_standard_test_cases_new.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "azimuth"],
  "link": {
    "azimuth/test": "self"
  }
}
EOF

echo "配置文件已创建，开始运行测试..."
./moon test

# 恢复原始配置文件
if [ -f azimuth/moon.pkg.json.backup ]; then
    mv azimuth/moon.pkg.json.backup azimuth/moon.pkg.json
    echo "原始配置文件已恢复"
fi

echo "测试完成"