// 综合测试套件 - 涵盖各种测试场景

// 1. 数学属性测试
test "addition_associative_property" {
  // 测试加法结合律 (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 测试负数情况
  let x = -10
  let y = 20
  let z = -5
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
}

test "multiplication_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 测试负数情况
  let x = -3
  let y = 8
  let z = -2
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

// 2. 边界值和特殊情况测试
test "extreme_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试最小值加法
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试最大值乘法
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(max_val, -2))
  
  // 测试最小值乘法
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "special_value_combinations" {
  let min_val = -2147483648
  
  // 测试最小值与1的乘法
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试最小值与-1的乘法（特殊情况）
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试最小值与0的加法
  assert_eq(min_val, add(min_val, 0))
  
  // 测试最小值与0的乘法
  assert_eq(0, multiply(min_val, 0))
}

// 3. 实际应用场景测试
test "financial_calculations" {
  // 模拟金融计算：计算总投资
  let price_per_share = 157
  let shares = 1000
  let commission = 25
  let total_cost = add(multiply(price_per_share, shares), commission)
  assert_eq(157025, total_cost)
  
  // 计算收益
  let sell_price = 165
  let proceeds = multiply(sell_price, shares)
  let profit = subtract(proceeds, total_cost)
  assert_eq(7975, profit)
}

test "inventory_management" {
  // 库存管理场景
  let initial_stock = 500
  let incoming = add(150, 75)  // 两批进货
  let outgoing = add(100, 200) // 两批出货
  let final_stock = subtract(add(initial_stock, incoming), outgoing)
  assert_eq(425, final_stock)
}

// 4. 字符串处理测试
test "greet_function_edge_cases" {
  // 测试特殊字符
  assert_eq("Hello, @#$%!", greet("@#$%"))
  assert_eq("Hello, 中文!", greet("中文"))
  assert_eq("Hello, 🌙!", greet("🌙"))
  
  // 测试长字符串
  let long_name = "This is a very long name that tests the greet function"
  let expected = "Hello, " + long_name + "!"
  assert_eq(expected, greet(long_name))
}

// 5. 性能相关测试
test "large_number_operations" {
  // 测试大数运算（但不溢出）
  let large_num = 1000000
  let result1 = add(large_num, large_num)
  assert_eq(2000000, result1)
  
  let result2 = multiply(large_num, 100)
  assert_eq(100000000, result2)
  
  // 测试连续运算
  let chain_result = add(add(add(1, 2), 3), 4)
  assert_eq(10, chain_result)
}

// 辅助函数：减法（用于测试）
fn subtract(a : Int, b : Int) -> Int {
  add(a, multiply(b, -1))
}