// 标准 MoonBit 附加测试用例
// 专注于边界条件、错误处理和实际应用场景

test "add_maximum_values" {
  // 测试大数加法
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(0, add(1000000, -1000000))
}

test "multiply_identity_properties" {
  // 测试乘法单位元性质
  assert_eq(7, multiply(7, 1))
  assert_eq(7, multiply(1, 7))
  assert_eq(-7, multiply(-7, 1))
  assert_eq(-7, multiply(1, -7))
}

test "divide_with_ceil_precision" {
  // 测试除法的精度处理
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(1, -2))
  assert_eq(0, divide_with_ceil(-1, -2))
}

test "greet_with_whitespace" {
  // 测试包含空白字符的问候
  assert_eq_string("Hello,   World   !", greet("   World   "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
}

test "complex_business_logic" {
  // 模拟实际业务逻辑：计算折扣价格
  let original_price = 100
  let discount_percentage = 20
  let discounted_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discounted_amount)
  assert_eq(80, final_price)
}

test "inventory_management" {
  // 库存管理：计算需要的货架数量
  let total_products = 47
  let capacity_per_shelf = 10
  let shelves_needed = divide_with_ceil(total_products, capacity_per_shelf)
  assert_eq(5, shelves_needed)
}

test "temperature_conversion" {
  // 温度转换模拟（简化的线性转换）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  assert_eq(77, fahrenheit)
}

test "string_concatenation_edge" {
  // 测试字符串连接的边界情况
  let long_name = "VeryLongNameThatExceedsNormalExpectations"
  let result = greet(long_name)
  assert_true(String.length(result) > String.length(long_name))
  assert_true(String.starts_with(result, "Hello, "))
  assert_true(String.ends_with(result, "!"))
}

test "divide_with_ceil_consistency" {
  // 测试除法函数的一致性
  let test_values = [(1, 1), (5, 2), (10, 3), (100, 25), (7, 3)]
  for (value, divisor) in test_values {
    let result = divide_with_ceil(value, divisor)
    assert_true(result >= 0)
    assert_true(result * divisor >= value)
  }
}

test "arithmetic_sequence" {
  // 等差数列计算
  let first_term = 3
  let common_difference = 4
  let n = 5
  let nth_term = add(first_term, multiply(n - 1, common_difference))
  assert_eq(19, nth_term)  // 3 + (5-1)*4 = 3 + 16 = 19
}