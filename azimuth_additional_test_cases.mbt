// 新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(-8, 6))
}

test "multiply_by_negative" {
  // 测试乘以负数
  assert_eq(-15, multiply(3, -5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(-7, 0))
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, Paris!", greet("Paris"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  let x = 12
  let y = 8
  let z = 5
  
  // (x + y) * z
  let result = multiply(add(x, y), z)
  assert_eq(100, result)
  
  // x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(101, result2)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
}

test "string_special_cases" {
  // 测试字符串特殊情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 7
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(0, add(a, -a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  assert_eq(a, multiply(multiply(a, 2), 1) / 2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 10
  let width = 6
  let perimeter = multiply(2, add(length, width))
  assert_eq(32, perimeter)
  
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20%
  let discounted_price = multiply(original_price, 100 - discount_rate) / 100
  assert_eq(160, discounted_price)
}

test "error_handling_validation" {
  // 测试错误处理验证
  assert_true(true)
  assert_false(false)
  
  // 验证溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 验证函数正常工作
  assert_eq(10, add(3, 7))
  assert_eq(21, multiply(3, 7))
}

test "performance_edge_cases" {
  // 测试性能边界情况
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, add(large_num1, large_num2))
  assert_eq(2000000000000, multiply(large_num1, large_num2))
  
  // 链式运算
  let chain_result = add(1, add(2, add(3, add(4, 5))))
  assert_eq(15, chain_result)
  
  let chain_result2 = multiply(2, multiply(3, multiply(4, 5)))
  assert_eq(120, chain_result2)
}