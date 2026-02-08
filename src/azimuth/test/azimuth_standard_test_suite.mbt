// Azimuth 项目标准 MoonBit 测试套件
// 使用标准 MoonBit 测试语法，包含 10 个高质量测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_operations_and_greetings" {
  // 测试字符串操作和问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "negative_number_calculations" {
  // 测试负数计算
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-13, subtract(-10, 3))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let result1 = multiply(add(2, 3), 4)  // (2+3)*4 = 20
  assert_eq(20, result1)
  
  let result2 = add(multiply(5, 6), 7)  // 5*6+7 = 37
  assert_eq(37, result2)
  
  let result3 = multiply(add(10, -3), add(2, 1))  // (10-3)*(2+1) = 21
  assert_eq(21, result3)
}

test "division_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(2, divide_with_ceil(100, 99))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_number_operations" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(500000, subtract(1000000, 500000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 17
  let y = 23
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
  
  // 减法的逆运算
  assert_eq(add(subtract(x, y), y), x)
}

test "real_world_shopping_cart_scenario" {
  // 真实世界购物车场景测试
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let item1_subtotal = multiply(item1_price, item1_quantity)  // 75
  let item2_subtotal = multiply(item2_price, item2_quantity)  // 30
  
  // 计算总计
  let items_total = add(item1_subtotal, item2_subtotal)  // 105
  
  // 计算税费（10%，向上取整）
  let tax_amount = divide_with_ceil(multiply(items_total, 10), 100)  // 11
  
  // 计算总价
  let grand_total = add(items_total, tax_amount)  // 116
  
  assert_eq(75, item1_subtotal)
  assert_eq(30, item2_subtotal)
  assert_eq(105, items_total)
  assert_eq(11, tax_amount)
  assert_eq(116, grand_total)
}

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  let f0 = 0
  let f1 = 1
  
  // 计算斐波那契数列
  let f2 = add(f1, f0)  // 1
  let f3 = add(f2, f1)  // 2
  let f4 = add(f3, f2)  // 3
  let f5 = add(f4, f3)  // 5
  let f6 = add(f5, f4)  // 8
  let f7 = add(f6, f5)  // 13
  let f8 = add(f7, f6)  // 21
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
}