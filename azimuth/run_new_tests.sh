#!/bin/bash

echo "运行新添加的测试用例"
echo "=================="
echo ""

# 创建临时目录
mkdir -p temp_test

# 复制我们的测试文件到临时目录
cp test/new_test_cases.mbt temp_test/simple_test.mbt

# 备份原始的 simple_test.mbt
cp test/simple_test.mbt test/simple_test.mbt.backup

# 将我们的测试文件复制为 simple_test.mbt
cp temp_test/simple_test.mbt test/simple_test.mbt

# 运行测试
echo "运行测试..."
./moon test

# 恢复原始的 simple_test.mbt
cp test/simple_test.mbt.backup test/simple_test.mbt

# 清理临时文件
rm -rf temp_test
rm test/simple_test.mbt.backup

echo ""
echo "测试运行完成！"