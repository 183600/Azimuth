// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
  assert_eq(add(1000, 2000), 3000)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(10, 10), 100)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(3, 10), -7)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 50), 50)
}

test "division_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  // 计算 (5 + 3) * 2 - 4 / 2 (向上取整)
  let step1 = add(5, 3)  // 8
  let step2 = multiply(step1, 2)  // 16
  let step3 = divide_with_ceil(4, 2)  // 2
  let result = subtract(step2, step3)  // 14
  
  assert_eq(result, 14)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  // 大数运算
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 1000), 10000000)
  
  // 负数运算
  assert_eq(add(-5, -3), -8)
  assert_eq(multiply(-2, -3), 6)
  assert_eq(subtract(-5, -3), -2)
  
  // 混合符号运算
  assert_eq(add(-5, 3), -2)
  assert_eq(multiply(-2, 3), -6)
}

test "real_world_application_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 25
  let item2_price = 30
  let item3_price = 15
  let tax_rate = 10  // 10%
  let discount = 10
  
  // 计算小计
  let subtotal = add(add(item1_price, item2_price), item3_price)
  
  // 应用折扣
  let discounted_total = subtract(subtotal, discount)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  
  // 计算总价
  let total = add(discounted_total, tax)
  
  assert_eq(subtotal, 70)
  assert_eq(discounted_total, 60)
  assert_eq(tax, 6)
  assert_eq(total, 66)
}

test "algorithmic_pattern_simulation" {
  // 测试算法模式模拟：等差数列求和
  // 计算 1 + 2 + 3 + ... + 10
  let first_term = 1
  let last_term = 10
  let number_of_terms = 10
  
  // 等差数列求和公式：S = n(a1 + an) / 2
  let sum_formula = divide_with_ceil(multiply(number_of_terms, add(first_term, last_term)), 2)
  
  // 手动计算验证
  let manual_sum = add(add(add(add(add(add(add(add(add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  
  assert_eq(sum_formula, manual_sum)
  assert_eq(sum_formula, 55)
}