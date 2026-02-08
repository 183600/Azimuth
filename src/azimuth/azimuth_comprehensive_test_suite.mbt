// Azimuth 项目综合测试套件
// 包含 8 个标准 MoonBit 测试用例，覆盖核心功能

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(11, subtract(20, 9))
  assert_eq(5, divide_with_ceil(22, 5))
  assert_eq(7, divide_with_ceil(21, 3))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greet_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, multiply(-3, 5))
  assert_eq(8, multiply(3, -5))
  assert_eq(-12, subtract(-7, 5))
  assert_eq(2, subtract(5, -3))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c - a / b (向上取整)
  let step1 = add(a, b)
  let step2 = multiply(step1, c)
  let step3 = divide_with_ceil(a, b)
  let result = subtract(step2, step3)
  
  assert_eq(step1, 13)
  assert_eq(step2, 26)
  assert_eq(step3, 4)
  assert_eq(result, 22)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负单位值测试
  assert_eq(4, add(5, -1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, divide_with_ceil(-5, 1))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景测试 - 购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 18
  let item2_qty = 2
  let tax_rate = 10
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let total_subtotal = add(subtotal1, subtotal2)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(total_subtotal, tax_rate), 100)
  
  // 计算总价
  let grand_total = add(total_subtotal, tax_amount)
  
  // 验证计算结果
  assert_eq(subtotal1, 75)
  assert_eq(subtotal2, 36)
  assert_eq(total_subtotal, 111)
  assert_eq(tax_amount, 12)
  assert_eq(grand_total, 123)
}