// 标准MoonBit测试用例 - 10个高质量测试用例
// 测试azimuth库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(7, 8), 56)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 8), -3)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 50), 50)
}

test "division_with_ceil_basic" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "division_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)  // 除数为0的情况
  assert_eq(divide_with_ceil(0, 0), 0)   // 两个0的情况
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 30)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 52)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 3)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "boundary_value_testing" {
  // 测试边界值
  // 测试零值
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 测试单位值
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 测试负一
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "practical_application_simulation" {
  // 模拟实际应用场景：购物车计算
  let item1_price = 25
  let item2_price = 30
  let item3_price = 45
  let discount = 10
  let tax_rate = 5  // 5%
  
  // 计算小计
  let subtotal = add(add(item1_price, item2_price), item3_price)
  
  // 应用折扣
  let discounted_total = subtract(subtotal, discount)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  
  // 计算最终总额
  let final_total = add(discounted_total, tax)
  
  assert_eq(subtotal, 100)
  assert_eq(discounted_total, 90)
  assert_eq(tax, 5)
  assert_eq(final_total, 95)
}