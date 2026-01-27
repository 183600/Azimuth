#!/bin/bash
echo "运行标准 Azimuth 单元测试..."
cd /home/runner/work/Azimuth/Azimuth

# 创建一个最小化的测试环境
mkdir -p test_temp
cd test_temp

# 创建最小化的 moon.pkg.json
cat > moon.pkg.json << 'EOF'
{
  "name": "test_temp",
  "export": ["add", "multiply", "greet", "assert_eq", "assert_eq_string", "assert_true", "assert_false", "divide_with_ceil", "subtract"],
  "files": ["../azimuth/lib.mbt", "../azimuth/standard_azimuth_unit_tests.mbt"],
  "import": ["moonbitlang/core/builtin", "moonbitlang/core"],
  "test-import": ["moonbitlang/core/builtin", "test_temp"],
  "link": {
    "test_temp/test": "self"
  }
}
EOF

# 复制必要的文件
cp ../azimuth/lib.mbt .
cp ../azimuth/standard_azimuth_unit_tests.mbt .

# 运行测试
echo "运行测试..."
../moon test

# 清理
cd ..
rm -rf test_temp

echo "测试完成"