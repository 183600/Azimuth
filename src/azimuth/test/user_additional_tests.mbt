// 用户添加的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试基本功能和边界条件

test "addition_with_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "addition_with_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "addition_with_mixed_numbers" {
  // 测试正负数混合相加
  @azimuth.assert_eq(-4, @azimuth.add(3, -7))
  @azimuth.assert_eq(4, @azimuth.add(-3, 7))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "multiplication_with_positive_numbers" {
  // 测试正数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "multiplication_with_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
  @azimuth.assert_eq(-21, @azimuth.multiply(3, -7))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_function_with_different_inputs" {
  // 测试 greet 函数的不同输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值操作
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值操作
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 2
  let d = 4
  
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(a, b), c), -d)
  @azimuth.assert_eq(12, result)  // (5 + 3) * 2 - 4 = 16 - 4 = 12
  
  // 计算面积和周长
  let length = 10
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 15
  let y = 25
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法恒等
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(x, @azimuth.multiply(1, x))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}