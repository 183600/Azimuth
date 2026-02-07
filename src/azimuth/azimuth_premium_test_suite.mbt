// Azimuth 高级测试套件
// 包含7个精心设计的标准 MoonBit 测试用例

test "core_arithmetic_functions" {
  // 测试核心算术函数
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(5, subtract(15, 10))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, Test!", greet("Test"))
  assert_eq_string("Hello, !", greet(""))
}

test "division_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(3, divide_with_ceil(6, 2))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 6
  let b = 9
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 单位元验证
  assert_eq(add(a, 0), a)
  assert_eq(multiply(a, 1), a)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 5000
  let rate = 8
  let years = 3
  
  // 计算简单利息
  let interest_per_year = divide_with_ceil(multiply(principal, rate), 100)
  let total_interest = multiply(interest_per_year, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(interest_per_year, 400)
  assert_eq(total_interest, 1200)
  assert_eq(final_amount, 6200)
}

test "inventory_management_simulation" {
  // 库存管理模拟测试
  let initial_stock = 120
  let daily_sales = 8
  let days = 7
  let new_shipment = 50
  
  // 计算一周后的库存
  let weekly_sales = multiply(daily_sales, days)
  let stock_after_sales = subtract(initial_stock, weekly_sales)
  let final_stock = add(stock_after_sales, new_shipment)
  
  assert_eq(weekly_sales, 56)
  assert_eq(stock_after_sales, 64)
  assert_eq(final_stock, 114)
}

test "algorithm_fibonacci_sequence" {
  // 斐波那契数列算法测试
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  let fib6 = add(fib4, fib5)  // 8
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  assert_eq(fib6, 8)
  
  // 验证连续斐波那契数的比例
  let ratio = divide_with_ceil(multiply(fib6, 100), fib5)
  assert_eq(ratio, 160)  // 8/5 = 1.6，接近黄金比例
}