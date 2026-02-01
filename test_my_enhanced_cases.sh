#!/bin/bash
echo "运行我新创建的增强测试用例..."
cd /home/runner/work/Azimuth/Azimuth

# 创建一个临时的 moon.pkg.json，只包含我们的测试文件
cat > azimuth/moon.pkg.json << 'EOF'
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["lib.mbt", "azimuth_enhanced_test_cases_new.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "azimuth"],
  "link": {
    "azimuth/test": "self"
  }
}
EOF

echo "配置文件内容："
cat azimuth/moon.pkg.json
echo ""
echo "开始运行测试..."

# 运行测试
./moon test

echo "测试完成"