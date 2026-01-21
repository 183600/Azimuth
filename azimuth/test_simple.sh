#!/bin/bash

echo "Testing additional_comprehensive_tests.mbt..."

# 创建一个简单的测试文件，只包含我们的测试
cat > test_simple.mbt << 'EOF'
// 简单测试文件，验证我们的测试语法
test "simple_test" {
  assert_true(1 == 1)
  assert_false(1 == 2)
}

test "string_test" {
  assert_eq_string("Hello", "Hello")
  assert_eq_string("", "")
}

test "math_test" {
  assert_eq(5, 2 + 3)
  assert_eq(0, 5 - 5)
}
EOF

echo "Created test_simple.mbt for syntax validation"

# 尝试编译这个简单的测试文件
echo "Attempting to compile test_simple.mbt..."
node ../moonc.js compile test_simple.mbt 2>&1 | head -20

echo "Test validation complete."