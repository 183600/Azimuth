// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_associative_law" {
  // 验证加法结合律
  let x = 5
  let y = 8
  let z = 12
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(0, divide_with_ceil(0, 100))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 150 - 1 = 149
}

test "real_world_shopping_cart" {
  // 模拟真实购物车计算
  let apple_price = 3
  let apple_qty = 2
  let banana_price = 5
  let banana_qty = 1
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let shopping_total = add(apple_total, banana_total)
  assert_eq(11, shopping_total)  // 3*2 + 5*1 = 6 + 5 = 11
}

test "time_calculation_scenario" {
  // 时间计算场景：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  assert_eq(6, days_needed)  // ceil(45/8) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "large_number_operations" {
  // 测试大数运算
  let big_num = 1000000
  assert_eq(2000000, add(big_num, big_num))
  assert_eq(1000000, multiply(1000, 1000))
}

test "distributive_property" {
  // 验证分配律 a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)  // 4 * (6 + 9) = 4 * 6 + 4 * 9 = 60
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算订单总价
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  assert_eq(348, final_total)  // 89*4 + 12 - 20 = 356 + 12 - 20 = 348
}