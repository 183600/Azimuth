// 标准增强测试用例
// 为 azimuth 包添加新的测试用例

test "add_function_performance_critical_cases" {
  // 性能关键情况下的加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续操作测试
  let result1 = add(add(1000000, 1000000), add(1000000, 1000000))
  let result2 = add(add(-1000000, 1000000), add(-1000000, 1000000))
  assert_eq(4000000, result1)
  assert_eq(0, result2)
  
  // 边界值连续操作
  let result3 = add(add(max_val - 1000, 500), add(500, 0))
  let result4 = add(add(min_val + 1000, -500), add(-500, 0))
  assert_eq(max_val, result3)
  assert_eq(min_val, result4)
}

test "multiply_function_large_numbers" {
  // 大数乘法测试
  let sqrt_max = 46340
  
  // 接近最大值的乘法
  assert_eq(2147395600, multiply(sqrt_max, sqrt_max))
  assert_eq(2147483647, multiply(sqrt_max, sqrt_max + 1))
  assert_eq(-2147395600, multiply(sqrt_max, -sqrt_max))
  
  // 中等数值的乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(1000, -1000))
  assert_eq(99980001, multiply(9999, 9999))
}

test "greet_function_multilingual_support" {
  // 多语言支持测试
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, español!", greet("español"))
  assert_eq_string("Hello, français!", greet("français"))
}

test "divide_with_ceil_fractional_precision" {
  // 分数精度测试
  // 小余数情况
  assert_eq(2, divide_with_ceil(4, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(3, divide_with_ceil(7, 3))
  
  // 大余数情况
  assert_eq(11, divide_with_ceil(100, 9))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(51, divide_with_ceil(100, 2))
  
  // 负数分数精度
  assert_eq(-1, divide_with_ceil(-2, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "complex_business_logic_simulation" {
  // 复杂业务逻辑模拟
  // 场景：计算订单总价（含税和折扣）
  let base_price = 1000
  let quantity = 5
  let discount_rate = 2  // 50% 折扣 (1/2)
  let tax_rate = 3       // 33% 税率 (1/3)
  
  // 计算原价
  let original_total = multiply(base_price, quantity)
  assert_eq(5000, original_total)
  
  // 应用折扣
  let discounted_price = divide_with_ceil(original_total, discount_rate)
  assert_eq(2500, discounted_price)
  
  // 计算税费
  let tax_amount = divide_with_ceil(discounted_price, tax_rate)
  assert_eq(834, tax_amount)
  
  // 最终总价
  let final_total = add(discounted_price, tax_amount)
  assert_eq(3334, final_total)
}

test "mathematical_sequence_operations" {
  // 数学序列操作
  // 等差数列求和
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 计算等差数列和：n/2 * (2a + (n-1)d)
  let double_first = multiply(first_term, 2)
  let diff_product = multiply(common_diff, num_terms - 1)
  let sum_inside = add(double_first, diff_product)
  let arithmetic_sum = divide_with_ceil(multiply(num_terms, sum_inside), 2)
  assert_eq(100, arithmetic_sum)
  
  // 等比数列求和（简化版）
  let ratio = 2
  let geo_sum = multiply(first_term, multiply(ratio, num_terms))
  assert_eq(20, geo_sum)
}

test "error_boundary_and_recovery" {
  // 错误边界和恢复测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 触发溢出后验证函数仍然可用
  let overflow_result1 = add(max_val, 1)  // 应该返回 max_val
  let overflow_result2 = multiply(max_val, 2)  // 应该返回 max_val
  let overflow_result3 = multiply(-1, min_val)  // 应该返回 min_val
  
  // 验证正常操作仍然工作
  let normal_add = add(100, 200)
  let normal_mul = multiply(10, 20)
  let normal_greet = greet("Test")
  
  assert_eq(max_val, overflow_result1)
  assert_eq(max_val, overflow_result2)
  assert_eq(min_val, overflow_result3)
  assert_eq(300, normal_add)
  assert_eq(200, normal_mul)
  assert_eq_string("Hello, Test!", normal_greet)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 特殊字符组合
  assert_eq_string("Hello, \\n\\t\\r!", greet("\\n\\t\\r"))
  assert_eq_string("Hello, \\0\\1\\2!", greet("\\0\\1\\2"))
  assert_eq_string("Hello, \u{1F600}\u{1F601}\u{1F602}!", greet("\u{1F600}\u{1F601}\u{1F602}"))
  
  // 极长字符串（相对）
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  assert_eq_string("Hello, " + long_name + "!", greet(long_name))
  
  // 混合字符类型
  assert_eq_string("Hello, a1b2c3!", greet("a1b2c3"))
  assert_eq_string("Hello, Test_123!", greet("Test_123"))
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证
  // 测试多个连续操作的性能
  let base = 100
  
  // 连续加法
  let sum_result = add(add(add(add(base, 50), 25), 10), 5)
  assert_eq(190, sum_result)
  
  // 连续乘法
  let product_result = multiply(multiply(multiply(base, 2), 3), 4)
  assert_eq(2400, product_result)
  
  // 混合操作
  let mixed_result = add(multiply(base, 2), divide_with_ceil(multiply(50, 3), 4))
  assert_eq(238, mixed_result)
  
  // 嵌套操作
  let nested_result = multiply(add(divide_with_ceil(100, 3), 5), 2)
  assert_eq(78, nested_result)
}

test "data_type_consistency" {
  // 数据类型一致性测试
  // 确保所有函数返回正确的类型
  
  // Int 类型函数
  let int_result1 = add(5, 3)
  let int_result2 = multiply(5, 3)
  let int_result3 = divide_with_ceil(10, 3)
  
  // String 类型函数
  let string_result = greet("MoonBit")
  
  // Unit 类型函数（断言）
  assert_true(int_result1 == 8)
  assert_true(int_result2 == 15)
  assert_true(int_result3 == 4)
  assert_true(string_result == "Hello, MoonBit!")
  
  // 类型一致性验证
  assert_eq(8, int_result1)
  assert_eq(15, int_result2)
  assert_eq(4, int_result3)
  assert_eq_string("Hello, MoonBit!", string_result)
}