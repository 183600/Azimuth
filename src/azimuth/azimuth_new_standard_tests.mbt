// 新增的 MoonBit 标准测试用例
// 测试项目核心功能的各种场景

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "divide_with_ceil_exact_division" {
  // 测试能够整除的情况
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(7, divide_with_ceil(21, 3))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "divide_with_ceil_with_zero" {
  // 测试包含0的除法
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(-3, divide_with_ceil(-10, 4))
}

test "complex_calculation_parentheses" {
  // 测试复杂计算的优先级和括号使用
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c = (5 + 3) * 2 = 16
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a + (b * c) = 5 + (3 * 2) = 11
  let result2 = add(a, multiply(b, c))
  assert_eq(11, result2)
}

test "greet_multilingual_support" {
  // 测试多语言支持的问候功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌎!", greet("🌎"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, サンリオ!", greet("サンリオ"))
}

test "mathematical_sequence_fibonacci_style" {
  // 测试类似斐波那契数列的计算模式
  let n1 = 1
  let n2 = 1
  let n3 = 2
  let n4 = 3
  let n5 = 5
  
  // 验证数列关系：n3 = n1 + n2, n4 = n2 + n3, n5 = n3 + n4
  assert_eq(n3, add(n1, n2))
  assert_eq(n4, add(n2, n3))
  assert_eq(n5, add(n3, n4))
}

test "area_perimeter_calculation" {
  // 测试几何计算：矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  let email = "user@example.com"
  let phone = "123-456-7890"
  let id = "USER_2023"
  
  // 验证这些字符串能够被greet函数正确处理
  assert_eq_string("Hello, user@example.com!", greet(email))
  assert_eq_string("Hello, 123-456-7890!", greet(phone))
  assert_eq_string("Hello, USER_2023!", greet(id))
}

test "business_calculation_profit_margin" {
  // 测试商业计算：利润率计算
  let revenue = 1000
  let cost = 700
  let profit_margin_percentage = 30
  
  // 计算利润：收入 - 成本
  let profit = add(revenue, -cost)
  
  // 计算利润率百分比：利润 / 收入 * 100
  let calculated_margin = multiply(profit, 100) / revenue
  
  assert_eq(300, profit)
  assert_eq(profit_margin_percentage, calculated_margin)
}