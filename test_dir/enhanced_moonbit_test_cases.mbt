// 增强的 MoonBit 测试用例
// 这些测试用例展示了对 azimuth 模块功能的全面测试

// 导入 azimuth 模块
use azimuth

test "enhanced_arithmetic_operations" {
  // 测试增强的算术运算
  let a = 42
  let b = 17
  
  // 基本加法测试
  assert_eq(59, add(a, b))
  
  // 负数加法测试
  assert_eq(25, add(a, -b))
  
  // 零值加法测试
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 基本乘法测试
  assert_eq(714, multiply(a, b))
  
  // 负数乘法测试
  assert_eq(-714, multiply(a, -b))
  assert_eq(714, multiply(-a, -b))
}

test "enhanced_boundary_value_testing" {
  // 测试增强的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值边界测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 混合边界测试
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "enhanced_string_processing" {
  // 测试增强的字符串处理
  // 基本问候测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 空字符串测试
  assert_eq_string("Hello, !", greet(""))
  
  // 特殊字符测试
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // Unicode 字符测试
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  
  // 数字字符串测试
  assert_eq_string("Hello, 2024!", greet("2024"))
  
  // 混合内容测试
  assert_eq_string("Hello, User123_测试!", greet("User123_测试"))
}

test "enhanced_mathematical_formulas" {
  // 测试增强的数学公式
  let x = 12
  let y = 8
  let z = 5
  
  // 复合运算：(x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(100, result1)
  
  // 分配律验证：x * (y + z) = x * y + x * z
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律验证：(x + y) + z = x + (y + z)
  let associative_left = add(add(x, y), z)
  let associative_right = add(x, add(y, z))
  assert_eq(associative_left, associative_right)
}

test "enhanced_real_world_scenarios" {
  // 测试增强的实际应用场景
  // 购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let discount = 50
  let shipping = 20
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let total = add(add(subtotal, -discount), shipping)
  assert_eq(637, total)
  
  // 温度转换：摄氏度转华氏度（简化版：F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单利息计算：本金 * (1 + 利率 * 时间)
  let principal = 10000
  let rate = 5
  let time = 3
  let interest = multiply(principal, multiply(rate, time))
  let total_amount = add(principal, interest)
  assert_eq(160000, total_amount)
}

test "enhanced_error_handling" {
  // 测试增强的错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护测试
  assert_true(add(max_val, 100) == max_val)
  assert_true(add(min_val, -100) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 边界条件断言
  assert_true(add(0, 0) == 0)
  assert_true(multiply(1, 1) == 1)
  assert_true(multiply(0, 12345) == 0)
}

test "enhanced_performance_calculations" {
  // 测试增强的性能计算
  // 等差数列求和：1 + 2 + ... + 100 = 100 * 101 / 2
  let n = 100
  let arithmetic_sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, arithmetic_sum)
  
  // 等比数列求和：1 + 2 + 4 + 8 + 16 = 31
  let geometric_sum = add(add(add(add(1, 2), 4), 8), 16)
  assert_eq(31, geometric_sum)
  
  // 平方和：1² + 2² + 3² + 4² + 5² = 55
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
  
  // 阶乘计算：5! = 120
  let factorial = multiply(5, multiply(4, multiply(3, multiply(2, 1))))
  assert_eq(120, factorial)
}

test "enhanced_division_operations" {
  // 测试增强的除法运算
  // 基本向上取整除法
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(11, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(4, divide_with_ceil(13, 4))
  
  // 负数除法测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-4, 4))
  assert_eq(0, divide_with_ceil(-3, 4))
  
  // 零除数边界测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数除法测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(500000, divide_with_ceil(1000000, 2))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "enhanced_complex_scenarios" {
  // 测试增强的复杂场景
  // 多步骤计算：((a + b) * (c - d)) + (e * f)
  let a = 15
  let b = 25
  let c = 30
  let d = 10
  let e = 8
  let f = 12
  
  let step1 = add(a, b)  // 40
  let step2 = add(c, -d)  // 20
  let step3 = multiply(step1, step2)  // 800
  let step4 = multiply(e, f)  // 96
  let final_result = add(step3, step4)  // 896
  
  assert_eq(896, final_result)
  
  // 条件计算：如果 x > y，则计算 x * y，否则计算 x + y
  let x = 20
  let y = 15
  let conditional_result = if (x > y) { multiply(x, y) } else { add(x, y) }
  assert_eq(300, conditional_result)
  
  // 嵌套计算：a * (b + (c * (d + e)))
  let nested_result = multiply(a, add(b, multiply(c, add(d, e))))
  assert_eq(1275, nested_result)
}

test "enhanced_data_validation" {
  // 测试增强的数据验证
  let test_values = [0, 1, -1, 42, -42, 1000, -1000, 2147483647, -2147483648]
  
  // 验证加法单位元
  for value in test_values {
    assert_eq(value, add(value, 0))
    assert_eq(value, add(0, value))
  }
  
  // 验证乘法单位元
  for value in test_values {
    assert_eq(value, multiply(value, 1))
    assert_eq(value, multiply(1, value))
  }
  
  // 验证乘法零元
  for value in test_values {
    assert_eq(0, multiply(value, 0))
    assert_eq(0, multiply(0, value))
  }
  
  // 验证字符串处理一致性
  let test_strings = ["", "a", "test", "123", "测试", "🌟", "mix123测试"]
  for str in test_strings {
    let expected = "Hello, " + str + "!"
    assert_eq_string(expected, greet(str))
  }
}