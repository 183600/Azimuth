#!/bin/bash
echo "Running quality enhanced test suite..."
echo "Testing file: quality_enhanced_test_suite.mbt"
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 创建一个临时的包配置文件，只包含我们的测试文件
cat > temp_moon.pkg.json << EOF
{"name": "azimuth", "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"], "files": ["lib.mbt", "quality_enhanced_test_suite.mbt"]}
EOF

# 备份原始配置文件
cp moon.pkg.json moon.pkg.json.backup

# 使用临时配置文件
cp temp_moon.pkg.json moon.pkg.json

# 运行测试
echo "Executing tests..."
./moon test

# 恢复原始配置文件
cp moon.pkg.json.backup moon.pkg.json

# 清理临时文件
rm temp_moon.pkg.json moon.pkg.json.backup

echo "Test completed."