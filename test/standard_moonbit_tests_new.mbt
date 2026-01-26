// 标准 MoonBit 测试用例 - 新增10个测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "mathematical_commutativity" {
  // 测试加法和乘法的交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(multiply(4, 9), multiply(9, 4))
}

test "divide_with_ceil_boundary_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "string_greeting_variations" {
  // 测试不同类型的字符串问候
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, a+b!", greet("a+b"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  let x = 10
  let y = 15
  let z = 20
  
  // (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(499, result)  // (10 + 15) * 20 - ceil(10/15) = 25 * 20 - 1 = 500 - 1 = 499
}

test "negative_number_division_precision" {
  // 测试负数除法的精度
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "real_world_shopping_cart" {
  // 真实购物车计算场景
  let apple_price = 3
  let apple_qty = 4
  let banana_price = 2
  let banana_qty = 6
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let cart_total = add(apple_total, banana_total)
  
  assert_eq(12, apple_total)
  assert_eq(12, banana_total)
  assert_eq(24, cart_total)
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 4
  let total_tasks = 17
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let total_minutes = multiply(hours_needed, 60)
  
  assert_eq(5, hours_needed)
  assert_eq(300, total_minutes)
}

test "financial_interest_calculation" {
  // 简单利息计算
  let principal = 1000
  let rate_percent = 5
  let years = 2
  
  let interest_per_year = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_interest = multiply(interest_per_year, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(50, interest_per_year)
  assert_eq(100, total_interest)
  assert_eq(1100, final_amount)
}

test "unicode_emoji_greetings" {
  // 测试 Unicode emoji 问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "algorithm_efficiency_simulation" {
  // 算法效率模拟
  let base_operations = 100
  let optimization_factor = 3
  let overhead = 15
  
  let optimized_ops = multiply(base_operations, optimization_factor)
  let actual_performance = add(optimized_ops, -overhead)
  
  assert_eq(300, optimized_ops)
  assert_eq(285, actual_performance)
}