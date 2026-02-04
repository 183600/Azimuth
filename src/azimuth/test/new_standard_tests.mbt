// 新增的标准 MoonBit 测试用例
// 包含8个新的测试用例，涵盖零值运算、极值测试、字符串边界、数学性质验证等

test "zero_value_arithmetic_operations" {
  // 测试零值在算术运算中的行为
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(-50, 0))
  
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  @azimuth.assert_eq(-10, @azimuth.subtract(0, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(0, 0))
  
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "extreme_value_calculations" {
  // 测试极值计算
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试大数加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int - 1, 1))
  @azimuth.assert_eq(min_int, @azimuth.add(min_int + 1, -1))
  
  // 测试大数乘法（使用较小的值避免溢出）
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  
  // 测试极值除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_int, max_int))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_int, min_int))
}

test "string_boundary_and_special_cases" {
  // 测试字符串边界和特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
  @azimuth.assert_eq_string("Hello, a_very_long_string_that_tests_length_handling!", @azimuth.greet("a_very_long_string_that_tests_length_handling"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 减法恒等式
  @azimuth.assert_eq(0, @azimuth.subtract(x, x))
  @azimuth.assert_eq(0, @azimuth.subtract(y, y))
  
  // 分配律测试
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 1)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 1)))
}

test "division_edge_cases_and_sign_behavior" {
  // 测试除法边界情况和符号行为
  // 正数除法
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(5, -2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-5, -2))
  
  // 混合符号除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -2))
  
  // 零除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 场景：库存管理和订单处理
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  
  @azimuth.assert_eq(7, boxes_needed)
  
  // 计算剩余空间
  let total_space = @azimuth.multiply(boxes_needed, box_capacity)
  let empty_space = @azimuth.subtract(total_space, total_products)
  @azimuth.assert_eq(11, empty_space)
  
  // 场景：预算分配
  let total_budget = 10000
  let departments = 3
  let base_budget = @azimuth.divide_with_ceil(total_budget, departments)
  let allocated_total = @azimuth.multiply(base_budget, departments)
  
  @azimuth.assert_eq(3334, base_budget)
  @azimuth.assert_eq(10002, allocated_total)
}

test "error_handling_and_robustness" {
  // 测试错误处理和健壮性
  // 测试空字符串和特殊输入
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  
  // 测试数值边界
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  
  // 测试大数组合运算
  let big_num1 = 1000000
  let big_num2 = 2000000
  let big_result = @azimuth.add(@azimuth.multiply(big_num1, 2), big_num2)
  @azimuth.assert_eq(4000000, big_result)
}

test "performance_and_stress_testing" {
  // 测试性能和压力情况
  // 连续运算测试
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4)
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 3), 4), 5)
  @azimuth.assert_eq(120, result2)
  
  // 嵌套运算测试
  let complex_result = @azimuth.add(@azimuth.multiply(10, 5), @azimuth.subtract(20, 8))
  @azimuth.assert_eq(62, complex_result)
  
  // 字符串处理压力测试
  let long_name = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  let expected_greeting = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected_greeting, @azimuth.greet(long_name))
}