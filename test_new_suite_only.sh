#!/bin/bash
echo "测试新创建的标准综合测试套件..."
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 首先检查我们的测试文件是否存在
echo "检查测试文件是否存在："
ls -la standard_comprehensive_test_suite.mbt

echo ""
echo "测试文件内容预览："
head -20 standard_comprehensive_test_suite.mbt

echo ""
echo "创建最小化的测试配置..."
# 备份原始配置
cp moon.pkg.json moon.pkg.json.backup

# 创建最小化的配置，只包含lib.mbt和我们的测试文件
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

echo ""
echo "新的配置文件内容："
cat moon.pkg.json

echo ""
echo "运行测试..."
cd /home/runner/work/Azimuth/Azimuth
./moon test 2>&1 | grep -A 10 "standard_comprehensive_test_suite"

echo ""
echo "恢复原始配置文件..."
cd /home/runner/work/Azimuth/Azimuth/azimuth
mv moon.pkg.json.backup moon.pkg.json

echo "测试完成"