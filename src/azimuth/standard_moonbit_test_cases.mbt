// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的核心测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(5, add(2, 3))
  assert_eq(6, multiply(2, 3))
  assert_eq(-1, subtract(2, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(1, divide_with_ceil(3, 3))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "division_edge_cases" {
  // 测试除法边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(3, divide_with_ceil(-10, 4))
}

test "negative_number_calculations" {
  // 测试负数计算
  assert_eq(-1, add(-2, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-5, subtract(-2, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let result = multiply(add(5, 3), subtract(10, 2))
  assert_eq(64, result)
  
  let result2 = divide_with_ceil(add(multiply(3, 4), 5), 2)
  assert_eq(9, result2)
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

test "boundary_value_tests" {
  // 测试边界值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "large_number_operations" {
  // 测试大数运算
  let big_num = 10000
  assert_eq(20000, add(big_num, big_num))
  assert_eq(100000000, multiply(big_num, big_num))
  assert_eq(0, subtract(big_num, big_num))
  assert_eq(3334, divide_with_ceil(big_num, 3))
}

test "precision_validation" {
  // 测试精度验证
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(50, divide_with_ceil(100, 2))
  assert_eq(34, divide_with_ceil(100, 3))
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算
  let item_price = 25
  let quantity = 3
  let tax_rate = 10
  let subtotal = multiply(item_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(75, subtotal)
  assert_eq(8, tax)
  assert_eq(83, total)
}