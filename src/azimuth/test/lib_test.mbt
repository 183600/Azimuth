// 综合测试文件
test "add_functions" {
  @assertion.assert_eq(3, add(1, 2))?
  @assertion.assert_eq(0, add(-1, 1))?
  @assertion.assert_eq(-5, add(-2, -3))?
  // 边界情况测试
  @assertion.assert_eq(0, add(0, 0))?
  @assertion.assert_eq(2147483647, add(2147483646, 1))?  // 接近Int最大值
  @assertion.assert_eq(2147483647, add(2147483647, 0))?  // 最大值加0
  @assertion.assert_eq(2147483647, add(2147483647, 1))?  // 溢出情况
  @assertion.assert_eq(2147483647, add(1000000000, 2000000000))?  // 溢出情况
  @assertion.assert_eq(-2147483648, add(-2147483648, 0))?  // 最小值加0
  @assertion.assert_eq(-2147483648, add(-2147483648, -1))?  // 溢出情况
  @assertion.assert_eq(-2147483648, add(-2147483648, -1000000000))?  // 溢出情况
  
  // 新增：正负数混合相加测试
  @assertion.assert_eq(-1, add(2147483647, -2147483648))?  // 最大值 + 最小值
  @assertion.assert_eq(-1, add(-2147483648, 2147483647))?  // 最小值 + 最大值
  @assertion.assert_eq(-2147483647, add(1, -2147483648))?  // 1 + 最小值
  @assertion.assert_eq(-2147483647, add(-2147483648, 1))?  // 最小值 + 1
  @assertion.assert_eq(2147483646, add(2147483647, -1))?  // 最大值 - 1
  @assertion.assert_eq(-2147483647, add(-2147483648, 1))?  // 最小值 + 1
  @assertion.assert_eq(100, add(200, -100))?  // 普通正负数相加
  @assertion.assert_eq(-100, add(-200, 100))?  // 普通负正数相加
  
  // 潜在问题测试
  @assertion.assert_eq(-2147483648, add(-2147483647, -1))?  // 边界负数相加
  @assertion.assert_eq(-2147483648, add(-2147483647, -2))?  // 边界负数相加
  @assertion.assert_eq(-2147483648, add(-2147483647, -3))?  // 边界负数相加
  @assertion.assert_eq(-2147483648, add(-2000000000, -2000000000))?  // 大负数相加
  
  // 综合边界测试
  @assertion.assert_eq(2147483647, add(2147483645, 3))?
  @assertion.assert_eq(2147483647, add(1073741824, 1073741824))?
  @assertion.assert_eq(-2147483648, add(-2147483646, -3))?
  @assertion.assert_eq(-2147483648, add(-1073741824, -1073741824))?
  @assertion.assert_eq(-2147483648, add(-2147483648, -2147483648))?
  @assertion.assert_eq(-2147483648, add(-2147483648, -100))?
  @assertion.assert_eq(0, add(2147483647, -2147483647))?
  @assertion.assert_eq(0, add(-2147483647, 2147483647))?
  
  // 测试正数溢出
  @assertion.assert_eq(2147483647, add(2147483646, 2))?
  @assertion.assert_eq(2147483647, add(2147483647, 1))?
  @assertion.assert_eq(2147483647, add(2147483647, 2147483647))?
  
  // 测试负数溢出
  @assertion.assert_eq(-2147483648, add(-2147483647, -2))?
  @assertion.assert_eq(-2147483648, add(-2147483648, -1))?
  @assertion.assert_eq(-2147483648, add(-2147483648, -2147483648))?
  
  // 测试正负数混合
  @assertion.assert_eq(-1, add(2147483647, -2147483648))?
  @assertion.assert_eq(-1, add(-2147483648, 2147483647))?
  @assertion.assert_eq(0, add(2147483647, -2147483647))?
  @assertion.assert_eq(0, add(-2147483647, 2147483647))?
  
  // 详细测试边界情况（从 add_boundary_check 合并）
  @assertion.assert_eq(-2147483648, add(-2147483640, -20))?  // 边界负数相加
  @assertion.assert_eq(-2147483648, add(-2000000000, -2000000000))?  // 大负数相加
  @assertion.assert_eq(-2147483648, add(-1073741824, -1073741824))?  // 中等负数相加
  @assertion.assert_eq(-2147483648, add(-2147483648, -2147483648))?  // 最小值相加
  
  // 测试不溢出的情况
  // @assertion.assert_eq(-2147483647, add(-2147483647, -1))?  // 刚好不溢出 - 矛盾的测试用例
  // 实际结果应该是 -2147483648
  @assertion.assert_eq(-2147483646, add(-2147483646, -1))?  // 不溢出
  @assertion.assert_eq(-100, add(-50, -50))?  // 普通负数相加
}

test "multiply_functions" {
  @assertion.assert_eq(6, multiply(2, 3))?
  @assertion.assert_eq(0, multiply(5, 0))?
  @assertion.assert_eq(-4, multiply(-2, 2))?
  // 边界情况测试
  @assertion.assert_eq(1, multiply(1, 1))?
  @assertion.assert_eq(0, multiply(0, 0))?
  @assertion.assert_eq(1, multiply(-1, -1))?
  // 最小值相关测试
  @assertion.assert_eq(-2147483648, multiply(-2147483648, 1))?  // 最小值乘1
  @assertion.assert_eq(-2147483648, multiply(1, -2147483648))?  // 1乘最小值
  @assertion.assert_eq(-2147483648, multiply(-2147483648, -1))?  // 最小值乘-1（溢出）
  @assertion.assert_eq(-2147483648, multiply(-1, -2147483648))?  // -1乘最小值（溢出）
  // 大数相乘测试
  @assertion.assert_eq(2147483647, multiply(46341, 46341))?  // 接近溢出的平方根
  @assertion.assert_eq(2147483647, multiply(100000, 30000))?  // 溢出情况
  @assertion.assert_eq(-2147483648, multiply(-100000, 30000))?  // 负溢出情况
  
  // 新增：更多最小值相关测试
  @assertion.assert_eq(-2147483648, multiply(-2147483648, 2))?  // 最小值乘2
  @assertion.assert_eq(-2147483648, multiply(2, -2147483648))?  // 2乘最小值
  @assertion.assert_eq(-2147483648, multiply(-2147483648, -2))?  // 最小值乘-2
  @assertion.assert_eq(-2147483648, multiply(-2, -2147483648))?  // -2乘最小值
  @assertion.assert_eq(-2147483648, multiply(-2147483648, 100))?  // 最小值乘100
  @assertion.assert_eq(-2147483648, multiply(100, -2147483648))?  // 100乘最小值
  @assertion.assert_eq(0, multiply(-2147483648, 0))?  // 最小值乘0
  @assertion.assert_eq(0, multiply(0, -2147483648))?  // 0乘最小值
  
  // 新增：更多边界情况
  @assertion.assert_eq(-2147483647, multiply(-2147483647, 1))?  // 接近最小值乘1
  @assertion.assert_eq(2147483647, multiply(2147483647, 1))?  // 最大值乘1
  @assertion.assert_eq(-2147483647, multiply(2147483647, -1))?  // 最大值乘-1
  @assertion.assert_eq(2147483647, multiply(-2147483647, -1))?  // 接近最小值乘-1
  
  // 综合边界测试
  @assertion.assert_eq(2147483647, multiply(46342, 46342))?
  @assertion.assert_eq(2147483647, multiply(50000, 50000))?
  @assertion.assert_eq(-2147483648, multiply(-46341, 46341))?
  @assertion.assert_eq(-2147483648, multiply(46341, -46341))?
  @assertion.assert_eq(-2147483648, multiply(-50000, 50000))?
  @assertion.assert_eq(-2147483648, multiply(50000, -50000))?
  @assertion.assert_eq(2147483647, multiply(1, 2147483647))?
  @assertion.assert_eq(-2147483647, multiply(-1, 2147483647))?
  @assertion.assert_eq(-2147483647, multiply(1, -2147483647))?
  @assertion.assert_eq(2147483647, multiply(-1, -2147483647))?
  
  // 测试最小值乘法
  @assertion.assert_eq(-2147483648, multiply(-2147483648, 1))?
  @assertion.assert_eq(-2147483648, multiply(1, -2147483648))?
  @assertion.assert_eq(-2147483648, multiply(-2147483648, -1))?
  @assertion.assert_eq(-2147483648, multiply(-1, -2147483648))?
  @assertion.assert_eq(-2147483648, multiply(-2147483648, 2))?
  @assertion.assert_eq(-2147483648, multiply(2, -2147483648))?
  @assertion.assert_eq(0, multiply(-2147483648, 0))?
  @assertion.assert_eq(0, multiply(0, -2147483648))?
  
  // 测试正数溢出
  @assertion.assert_eq(2147483647, multiply(46341, 46341))?
  @assertion.assert_eq(2147483647, multiply(46342, 46342))?
  @assertion.assert_eq(2147483647, multiply(50000, 50000))?
  
  // 测试负数溢出
  @assertion.assert_eq(-2147483648, multiply(-46341, 46341))?
  @assertion.assert_eq(-2147483648, multiply(46341, -46341))?
  @assertion.assert_eq(-2147483648, multiply(-50000, 50000))?
  @assertion.assert_eq(-2147483648, multiply(50000, -50000))?
}

test "greet_function" {
  @assertion.assert_eq("Hello, World!", greet("World"))?
  @assertion.assert_eq("Hello, MoonBit!", greet("MoonBit"))?
  // 边界情况测试
  @assertion.assert_eq("Hello, !", greet(""))?  // 空字符串
  @assertion.assert_eq("Hello, A!", greet("A"))?  // 单字符
  @assertion.assert_eq("Hello, 123!", greet("123"))?  // 数字字符串
}
