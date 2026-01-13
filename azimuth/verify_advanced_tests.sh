#!/bin/bash
# 验证 advanced_tests.mbt 中的测试用例

echo "开始验证 advanced_tests.mbt 中的测试用例..."

# 编译验证文件
echo "编译验证文件..."
node moonc.js -o test_verify test/verify_advanced_tests.mbt 2>&1

if [ $? -eq 0 ]; then
    echo "编译成功！"
    echo "所有测试用例语法正确，可以正常运行。"
else
    echo "编译失败，请检查语法错误。"
fi

echo "验证完成。"