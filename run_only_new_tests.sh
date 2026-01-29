#!/bin/bash
echo "运行新创建的标准综合测试套件..."
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 创建一个临时的 moon.pkg.json，只包含我们的测试文件
cp moon.pkg.json moon.pkg.json.backup

# 创建简化的配置文件，只包含lib.mbt和我们的测试文件
cat > moon.pkg.json << 'EOF'
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["lib.mbt", "standard_comprehensive_test_suite.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "azimuth"],
  "link": {
    "azimuth/test": "self"
  }
}
EOF

echo "配置文件已更新，内容如下："
cat moon.pkg.json

echo ""
echo "运行测试..."
cd /home/runner/work/Azimuth/Azimuth
./moon test

# 恢复原始配置文件
cd /home/runner/work/Azimuth/Azimuth/azimuth
mv moon.pkg.json.backup moon.pkg.json

echo "测试完成"