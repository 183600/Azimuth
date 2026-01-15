#!/bin/bash
echo "运行标准 MoonBit 测试套件..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 创建临时配置文件，只包含 lib.mbt 和我们的测试文件
cat > moon.pkg.temp.json << EOF
{
  "name": "azimuth",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false"],
  "files": ["lib.mbt", "standard_moonbit_test_suite.mbt"]
}
EOF

# 备份原始配置文件
cp moon.pkg.json moon.pkg.backup.json

# 使用临时配置文件
cp moon.pkg.temp.json moon.pkg.json

# 运行测试
echo "使用临时配置文件运行测试..."
../../moon test

# 恢复原始配置文件
cp moon.pkg.backup.json moon.pkg.json

# 清理临时文件
rm moon.pkg.temp.json moon.pkg.backup.json

echo "测试完成！"