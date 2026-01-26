// Azimuth 增强标准测试用例
// 包含 8 个高质量的 MoonBit 测试用例，使用标准测试语法

test "addition_boundary_cases" {
  // 测试加法边界情况
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(100000, add(99999, 1))
}

test "multiplication_edge_cases" {
  // 测试乘法边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(2147483647, multiply(46340, 46341))
  assert_eq(0, multiply(2147483647, 0))
}

test "divide_with_ceil_negative_cases" {
  // 测试负数向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(2, divide_with_ceil(-4, -2))
  assert_eq(1, divide_with_ceil(-3, -2))
}

test "greet_function_special_cases" {
  // 测试问候函数特殊情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, aBcDeF!", greet("aBcDeF"))
}

test "complex_nested_operations" {
  // 测试复杂嵌套运算
  let result = add(multiply(add(2, 3), multiply(4, 5)), divide_with_ceil(100, 3))
  assert_eq(134, result)  // (2+3)*(4*5) + ceil(100/3) = 5*20 + 34 = 100 + 34 = 134
}

test "financial_calculation_scenario" {
  // 金融计算场景：投资回报
  let principal = 10000
  let rate_percent = 8
  let years = 3
  let yearly_return = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_return = multiply(yearly_return, years)
  let final_amount = add(principal, total_return)
  assert_eq(800, yearly_return)
  assert_eq(2400, total_return)
  assert_eq(12400, final_amount)
}

test "inventory_optimization" {
  // 库存优化计算
  let demand = 157
  let batch_size = 25
  let safety_stock = 20
  let batches_needed = divide_with_ceil(demand, batch_size)
  let total_inventory = add(multiply(batches_needed, batch_size), safety_stock)
  assert_eq(7, batches_needed)
  assert_eq(195, total_inventory)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_operations = multiply(input_size, 2)
  let quadratic_operations = divide_with_ceil(multiply(input_size, input_size), 1000)
  let total_operations = add(linear_operations, quadratic_operations)
  assert_eq(2000, linear_operations)
  assert_eq(1000, quadratic_operations)
  assert_eq(3000, total_operations)
}