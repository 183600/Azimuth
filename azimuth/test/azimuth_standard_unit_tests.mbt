// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(2, 3), 5)
  @azimuth.assert_eq(@azimuth.add(-2, 2), 0)
  @azimuth.assert_eq(@azimuth.add(-2, -3), -5)
  @azimuth.assert_eq(@azimuth.add(50, 50), 100)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(2, 3), 6)
  @azimuth.assert_eq(@azimuth.multiply(2, -3), -6)
  @azimuth.assert_eq(@azimuth.multiply(5, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(10, 10), 100)
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(@azimuth.add(a, 0), a)
  
  // 乘法单位元
  @azimuth.assert_eq(@azimuth.multiply(a, 1), a)
}

test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值运算
  @azimuth.assert_eq(@azimuth.add(max_val, 0), max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  
  // 测试最小值运算
  @azimuth.assert_eq(@azimuth.add(min_val, 0), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 4
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result1, 32)
  
  // a * b + a * c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result2, 35)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积
  let length = 8
  let width = 6
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(area, 48)
  
  // 计算周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(perimeter, 28)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试空字符串
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string(@azimuth.greet("@#$%"), "Hello, @#$%!")
  
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalLength"
  @azimuth.assert_eq_string(@azimuth.greet(long_name), "Hello, VeryLongNameThatExceedsNormalLength!")
}

test "zero_operations" {
  // 测试零相关操作
  let x = 42
  
  // 加零
  @azimuth.assert_eq(@azimuth.add(x, 0), x)
  @azimuth.assert_eq(@azimuth.add(0, x), x)
  
  // 乘零
  @azimuth.assert_eq(@azimuth.multiply(x, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, x), 0)
  
  // 零自身运算
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(0, 0), 0)
}

test "negative_operations" {
  // 测试负数操作
  let a = 10
  let b = -3
  
  // 正负数相加
  @azimuth.assert_eq(@azimuth.add(a, b), 7)
  @azimuth.assert_eq(@azimuth.add(-a, b), -7)
  
  // 正负数相乘
  @azimuth.assert_eq(@azimuth.multiply(a, b), -30)
  @azimuth.assert_eq(@azimuth.multiply(-a, b), 30)
  @azimuth.assert_eq(@azimuth.multiply(a, -b), 30)
  @azimuth.assert_eq(@azimuth.multiply(-a, -b), -30)
}