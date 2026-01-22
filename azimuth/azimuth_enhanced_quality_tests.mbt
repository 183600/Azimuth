// 高质量 MoonBit 测试用例 - 专门测试边界条件和实际应用场景
// 使用标准 MoonBit 测试语法，确保代码质量和覆盖率

test "add_function_overflow_boundary" {
  // 测试加法函数的边界情况和潜在溢出
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(0, add(-2147483648, 2147483648))
}

test "multiply_function_precision" {
  // 测试乘法函数的精度和边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, multiply(-1, -1))
  assert_eq(2147483647, multiply(46340, 46341))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法的各种情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "greet_function_unicode_comprehensive" {
  // 全面测试 greet 函数的 Unicode 字符处理
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌟🌙!", greet("🌟🌙"))
  assert_eq_string("Hello, 🎯!", greet("🎯"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算管道
  let initial_value = 10
  let step1 = add(initial_value, 5)
  let step2 = multiply(step1, 3)
  let step3 = divide_with_ceil(step2, 4)
  let final_result = add(step3, 2)
  
  // ((10 + 5) * 3) / 4 + 2 = (15 * 3) / 4 + 2 = 45 / 4 + 2 = 12 + 2 = 14
  assert_eq(14, final_result)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, multiply(0, 0))
}

test "performance_related_operations" {
  // 测试性能相关的操作模式
  let base = 100
  let result = add(multiply(base, 2), divide_with_ceil(multiply(base, 3), 4))
  // 100 * 2 + (100 * 3) / 4 = 200 + 300 / 4 = 200 + 75 = 275
  assert_eq(275, result)
}

test "real_world_inventory_calculation" {
  // 现实世界库存计算场景
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 157 / 24 = 6.54，需要7个箱子
  assert_eq(7, boxes_needed)
  
  // 计算剩余空间
  let total_space = multiply(boxes_needed, box_capacity)
  let remaining_space = add(total_space, -total_products)
  assert_eq(11, remaining_space)  // 7*24 - 157 = 168 - 157 = 11
}

test "mathematical_series_calculation" {
  // 数学级数计算
  let n = 5
  let sum = 0
  let sum_step1 = add(sum, 1)
  let sum_step2 = add(sum_step1, 2)
  let sum_step3 = add(sum_step2, 3)
  let sum_step4 = add(sum_step3, 4)
  let sum_step5 = add(sum_step4, 5)
  
  // 1 + 2 + 3 + 4 + 5 = 15
  assert_eq(15, sum_step5)
  
  // 计算平均值的向上取整
  let average_ceil = divide_with_ceil(sum_step5, n)
  assert_eq(3, average_ceil)  // ceil(15/5) = 3
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello, ABCdef!", greet("ABCdef"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced   "))
}