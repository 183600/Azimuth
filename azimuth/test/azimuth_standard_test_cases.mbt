// Azimuth 标准测试用例 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

// 导入azimuth包的函数
test "add_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
}

test "multiply_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_string_functionality" {
  // 测试问候函数的字符串处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试向上取整除法的正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
}

test "subtract_basic_functionality" {
  // 测试减法函数的基本功能（使用add和multiply模拟）
  @azimuth.assert_eq(2, @azimuth.add(5, @azimuth.multiply(-1, 3)))
  @azimuth.assert_eq(-2, @azimuth.add(3, @azimuth.multiply(-1, 5)))
  @azimuth.assert_eq(0, @azimuth.add(5, @azimuth.multiply(-1, 5)))
  @azimuth.assert_eq(10, @azimuth.add(5, @azimuth.multiply(-1, -5)))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  @azimuth.assert_eq(@azimuth.add(7, 13), @azimuth.add(13, 7))
  @azimuth.assert_eq(@azimuth.multiply(7, 13), @azimuth.multiply(13, 7))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.divide_with_ceil(@azimuth.add(10, 15), 4)
  @azimuth.assert_eq(7, result2)  // ceil(25/4) = 7
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和边缘情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
}

test "real_world_application_scenarios" {
  // 测试真实世界应用场景
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, items_per_box)
  @azimuth.assert_eq(30, total_capacity)
}