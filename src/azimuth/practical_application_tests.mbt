// 实际应用场景测试用例
// 测试更贴近实际使用情况的场景

///|
test "financial_calculation" {
  // 模拟简单的金融计算场景
  let initial_balance = 1000
  let deposit1 = 500
  let deposit2 = -200  // 取款
  let final_balance = add(add(initial_balance, deposit1), deposit2)
  assert_eq(1300, final_balance)
  
  // 测试大额金融计算
  let large_amount = 1000000
  let interest = 50000
  let total = add(large_amount, interest)
  assert_eq(1050000, total)
}

///|
test "inventory_management" {
  // 模拟库存管理场景
  let initial_stock = 100
  let units_per_box = 12
  let boxes_received = 5
  let new_stock = add(initial_stock, multiply(units_per_box, boxes_received))
  assert_eq(160, new_stock)  // 100 + (12 * 5) = 160
  
  // 测试库存减少
  let items_sold = 30
  let remaining_stock = add(new_stock, -items_sold)
  assert_eq(130, remaining_stock)
}

///|
test "temperature_conversion" {
  // 模拟温度转换计算
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 9), 160)  // (C * 9/5) + 32 = (C * 9 + 160) / 5
  let result = multiply(fahrenheit, 1)  // 简化版本，实际应该是除以5
  assert_eq(385, result)  // 25 * 9 + 160 = 385
}

///|
test "greet_multilingual" {
  // 测试多语言问候
  assert_eq("Hello, 世界!", greet("世界"))
  assert_eq("Hello, Mundo!", greet("Mundo"))
  assert_eq("Hello, le Monde!", greet("le Monde"))
  assert_eq("Hello, 🌍!", greet("🌍"))
  assert_eq("Hello, Mr. Smith!", greet("Mr. Smith"))
  assert_eq("Hello, user@domain.com!", greet("user@domain.com"))
}

///|
test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let a1 = 2
  let d = 3
  let n = 5
  
  // 计算等差数列前n项和：S_n = n/2 * (2a_1 + (n-1)*d)
  let two_a1 = multiply(a1, 2)
  let n_minus_1 = add(n, -1)
  let term2 = multiply(n_minus_1, d)
  let sum_inside = add(two_a1, term2)
  let n_times_sum = multiply(n, sum_inside)
  
  // 简化版本，不考虑除以2
  assert_eq(80, n_times_sum)  // 5 * (4 + 12) = 5 * 16 = 80
}

///|
test "boundary_combination_test" {
  // 测试边界值的组合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // (max + min) + max = (-1) + max = max - 1
  let step1 = add(max_val, min_val)
  let result = add(step1, max_val)
  assert_eq(2147483646, result)
  
  // min * 1 + max = min + max = -1
  let min_times_one = multiply(min_val, 1)
  let combined = add(min_times_one, max_val)
  assert_eq(-1, combined)  // min * 1 = min, 然后 min + max = -1
}

///|
test "string_edge_cases" {
  // 测试字符串处理的边缘情况
  assert_eq("Hello, null!", greet("null"))
  assert_eq("Hello, undefined!", greet("undefined"))
  assert_eq("Hello, [object Object]!", greet("[object Object]"))
  assert_eq("Hello, \n!", greet("\n"))
  assert_eq("Hello, \t!", greet("\t"))
  assert_eq("Hello, \"quoted\"!", greet("\"quoted\""))
}

///|
test "mathematical_identities" {
  // 测试数学恒等式
  let x = 42
  let y = 17
  
  // x + y - y = x
  let sum = add(x, y)
  let back_to_x = add(sum, -y)
  assert_eq(x, back_to_x)
  
  // x * 1 + 0 = x
  let multiplied = multiply(x, 1)
  let final_result = add(multiplied, 0)
  assert_eq(x, final_result)
  
  // (x + y) * 0 = 0
  let sum2 = add(x, y)
  let zero_result = multiply(sum2, 0)
  assert_eq(0, zero_result)
}

///|
test "error_recovery_scenarios" {
  // 测试错误恢复场景
  let overflow_result = add(2147483647, 1)
  assert_eq(2147483647, overflow_result)  // 溢出返回最大值
  
  // 从溢出状态恢复
  let recovered = add(overflow_result, -1000)
  assert_eq(2147482647, recovered)  // 2147483647 - 1000 = 2147482647
  
  let underflow_result = add(-2147483648, -1)
  assert_eq(-2147483648, underflow_result)  // 下溢返回最小值
  
  // 从下溢状态恢复
  let recovered2 = add(underflow_result, 1000)
  assert_eq(-2147482648, recovered2)  // -2147483648 + 1000 = -2147482648
}