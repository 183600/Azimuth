#!/bin/bash
echo "运行标准 Azimuth 单元测试..."
cd /home/runner/work/Azimuth/Azimuth

# 创建一个临时的 moon.pkg.json，只包含我们的测试文件
cp azimuth/moon.pkg.json azimuth/moon.pkg.json.backup

# 创建简化的配置文件
cat > azimuth/moon.pkg.json << 'EOF'
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["lib.mbt", "standard_azimuth_unit_tests.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "azimuth"],
  "link": {
    "azimuth/test": "self"
  }
}
EOF

# 运行测试
./moon test

# 恢复原始配置文件
mv azimuth/moon.pkg.json.backup azimuth/moon.pkg.json

echo "测试完成"