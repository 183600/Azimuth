// Azimuth 标准增强测试用例
// 覆盖核心功能：add, multiply, greet, divide_with_ceil

test "add_function_basic_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_boundary_values" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
}

test "multiply_function_basic_operations" {
  // 测试基本乘法运算
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "multiply_function_special_cases" {
  // 测试乘法特殊情况
  @azimuth.assert_eq(10, @azimuth.multiply(1, 10))
  @azimuth.assert_eq(10, @azimuth.multiply(10, 1))
  @azimuth.assert_eq(-10, @azimuth.multiply(-1, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(10, 0))
}

test "greet_function_standard_cases" {
  // 测试标准问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "greet_function_edge_cases" {
  // 测试问候函数边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(25, 5))
}

test "divide_with_ceil_negative_and_zero" {
  // 测试负数和零的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_arithmetic_combinations" {
  // 测试复杂算术组合
  let sum = @azimuth.add(15, 25)
  let product = @azimuth.multiply(sum, 2)
  let result = @azimuth.divide_with_ceil(product, 8)
  @azimuth.assert_eq(10, result)
  
  // 测试嵌套运算
  let nested = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.add(2, 4))
  @azimuth.assert_eq(48, nested)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车总价计算
  let item1 = 199
  let item2 = 299
  let subtotal = @azimuth.add(item1, item2)
  let total_with_tax = @azimuth.multiply(subtotal, 2)
  @azimuth.assert_eq(996, total_with_tax)
  
  // 包装计算
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(5, boxes_needed)
}