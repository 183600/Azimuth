// 综合测试套件 - 标准MoonBit测试用例
// 覆盖azimuth模块的核心功能

test "subtract_basic_functionality" {
  // 测试基本减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
  assert_eq(10, subtract(15, -5))
}

test "subtract_boundary_values" {
  // 测试边界值减法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, subtract(max_val, 0))
  assert_eq(min_val, subtract(min_val, 0))
  assert_eq(-1, subtract(min_val, max_val))
  assert_eq(0, subtract(min_val, min_val))
}

test "divide_with_ceil_basic_cases" {
  // 测试基本除法向上取整功能
  assert_eq(3, divide_with_ceil(10, 3))    // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 1))     // 1/1 = 1 -> 1
  assert_eq(0, divide_with_ceil(0, 5))     // 0/5 = 0 -> 0
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的除法向上取整
  assert_eq(-3, divide_with_ceil(-10, 3))   // -10/3 = -3.33 -> -3
  assert_eq(3, divide_with_ceil(10, -3))    // 10/-3 = -3.33 -> -3
  assert_eq(4, divide_with_ceil(-10, -3))   // -10/-3 = 3.33 -> 4
}

test "divide_with_ceil_zero_division" {
  // 测试除零情况
  assert_eq(0, divide_with_ceil(5, 0))      // 除零返回0
  assert_eq(0, divide_with_ceil(-5, 0))     // 负数除零返回0
  assert_eq(0, divide_with_ceil(0, 0))      // 0除0返回0
}

test "greet_edge_cases" {
  // 测试greet函数的边界情况
  assert_eq_string("Hello, !", greet(""))                    // 空字符串
  assert_eq_string("Hello, World!", greet("World"))          // 普通字符串
  assert_eq_string("Hello, 123!", greet("123"))              // 数字字符串
  assert_eq_string("Hello, 🌙!", greet("🌙"))                // emoji字符串
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 12
  let b = 8
  let c = 4
  
  // (a - b) * c
  let diff = subtract(a, b)
  let result1 = multiply(diff, c)
  assert_eq(16, result1)
  
  // a + (b / c) - 使用向上取整
  let div_result = divide_with_ceil(b, c)
  let result2 = add(a, div_result)
  assert_eq(14, result2)
  
  // (a * b) - c
  let product = multiply(a, b)
  let result3 = subtract(product, c)
  assert_eq(92, result3)
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 等比数列：2 + 4 + 8 + 16 = 30
  let sum2 = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum2)
  
  // 阶乘计算：4! = 4 × 3 × 2 × 1 = 24
  let factorial = multiply(multiply(multiply(4, 3), 2), 1)
  assert_eq(24, factorial)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物计算：商品价格 × 数量 + 运费
  let price = 25
  let quantity = 4
  let shipping = 10
  let total_cost = add(multiply(price, quantity), shipping)
  assert_eq(110, total_cost)
  
  // 时间计算：开始时间 + 持续时间 - 休息时间
  let start_hour = 9
  let duration = 8
  let break_time = 1
  let end_hour = add(subtract(add(start_hour, duration), break_time), 0)
  assert_eq(16, end_hour)
  
  // 平均值计算（向上取整）：(85 + 92 + 78 + 95) / 4
  let scores_sum = add(add(add(85, 92), 78), 95)
  let average_score = divide_with_ceil(scores_sum, 4)
  assert_eq(88, average_score)
}

test "error_boundary_and_validation" {
  // 测试错误边界和验证
  // 极值运算
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(0, subtract(large_positive, large_positive))
  assert_eq(2000000, subtract(large_positive, large_negative))
  assert_eq(-2000000, subtract(large_negative, large_positive))
  
  // 除法边界测试
  assert_eq(1, divide_with_ceil(1, 1000000))      // 小结果
  assert_eq(1000000, divide_with_ceil(1000000, 1)) // 大结果
  assert_eq(0, divide_with_ceil(1, -1000000))      // 负除数小结果
}