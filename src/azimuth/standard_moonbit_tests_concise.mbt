// 标准 MoonBit 测试用例 - 简洁版本
// 包含8个核心测试用例，覆盖主要功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  assert_eq(subtract(10, 3), 7)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(add(-5, 3), -2)
  assert_eq(multiply(-4, 3), -12)
  assert_eq(multiply(-4, -3), 12)
  assert_eq(subtract(5, -3), 8)
  assert_eq(divide_with_ceil(-10, 3), -3)
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(-1, 2), -1)
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 单位元性质
  assert_eq(add(x, 0), x)
  assert_eq(multiply(x, 1), x)
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 17)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负单位测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
}

test "practical_application_scenario" {
  // 实际应用场景测试 - 购物车计算
  let item_price = 25
  let quantity = 3
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(subtotal, tax)
  
  assert_eq(subtotal, 75)
  assert_eq(tax, 8)
  assert_eq(total, 83)
}