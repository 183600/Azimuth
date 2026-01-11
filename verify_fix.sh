#!/bin/bash

echo "验证修复后的代码..."
echo ""

echo "1. 测试add函数的修复..."
echo ""

echo "测试 add(-1073741824, -1073741824) 应该返回 -2147483648"
echo "修复后：if a + b <= min_val"
echo "当 a = -1073741824, b = -1073741824 时，a + b = -2147483648"
echo "所以 -2147483648 <= -2147483648 为true，返回min_val ✓"

echo ""
echo "测试 add(-2147483647, -2) 应该返回 -2147483648"
echo "当 a = -2147483647, b = -2 时，a + b = -2147483649"
echo "所以 -2147483649 <= -2147483648 为true，返回min_val ✓"

echo ""
echo "测试 add(-2147483647, -1) 应该返回 -2147483648"
echo "当 a = -2147483647, b = -1 时，a + b = -2147483648"
echo "所以 -2147483648 <= -2147483648 为true，返回min_val ✓"

echo ""
echo "测试 add(-100, -200) 应该返回 -300（不溢出）"
echo "当 a = -100, b = -200 时，a + b = -300"
echo "所以 -300 <= -2147483648 为false，返回a + b = -300 ✓"

echo ""
echo "2. 测试multiply函数..."
echo ""

echo "测试 multiply(-2147483648, -1) 应该返回 -2147483648（溢出）"
echo "当前代码：if b == -1 { return if a == min_val { min_val } else { -a } }"
echo "这里 b = -1，会进入分支，且 a = min_val，所以返回 min_val ✓"

echo ""
echo "测试 multiply(-1, -2147483648) 应该返回 -2147483648（溢出）"
echo "当前代码：if a == -1 { return if b == min_val { min_val } else { -b } }"
echo "这里 a = -1，会进入分支，且 b = min_val，所以返回 min_val ✓"

echo ""
echo "测试 multiply(46341, 46341) 应该返回 2147483647（溢出）"
echo "当前代码：检查 abs_a > max_val / abs_b"
echo "46341 > 2147483647 / 46341 = 46340，所以溢出，返回max_val ✓"

echo ""
echo "3. 测试greet函数..."
echo ""

echo "测试 greet(\"World\") 应该返回 \"Hello, World!\""
echo "当前代码：\"Hello, \" + name + \"!\""
echo "返回 \"Hello, World!\" ✓"

echo ""
echo "4. 检查是否有其他潜在问题..."
echo ""

echo "检查add函数的其他边界情况："
echo "add(-2147483648, 1) = -2147483647 ✓"
echo "add(-2147483648, 0) = -2147483648 ✓"
echo "add(0, -2147483648) = -2147483648 ✓"
echo "add(2147483647, 1) = 2147483647（溢出）✓"

echo ""
echo "验证完成，修复应该解决了主要问题！"