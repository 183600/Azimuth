// 高质量 MoonBit 测试用例
// 包含10个专业级测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
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

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 测试单位值
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 测试负一
  assert_eq(0, add(-1, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(1, subtract(0, -1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(-24, multiply(4, -6))
  assert_eq(24, multiply(-4, -6))
  assert_eq(-17, subtract(-10, 7))
  assert_eq(17, subtract(10, -7))
  assert_eq(-3, subtract(-10, -7))
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_operations" {
  // 字符串操作测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculations" {
  // 复杂计算测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(1, result3)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(500000, subtract(1000000, 500000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
}

test "precision_operations" {
  // 精度运算测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(2, divide_with_ceil(100, 99))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "real_world_calculation" {
  // 真实世界计算测试 - 购物车计算
  // 商品价格和数量
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算各商品小计
  let item1_subtotal = multiply(item1_price, item1_quantity)
  let item2_subtotal = multiply(item2_price, item2_quantity)
  
  // 计算商品总计
  let items_total = add(item1_subtotal, item2_subtotal)
  
  // 计算税费（假设税率10%，向上取整）
  let tax_rate = 10
  let tax_amount = divide_with_ceil(multiply(items_total, tax_rate), 100)
  
  // 计算总价
  let grand_total = add(items_total, tax_amount)
  
  // 验证计算结果
  assert_eq(75, item1_subtotal)
  assert_eq(30, item2_subtotal)
  assert_eq(105, items_total)
  assert_eq(11, tax_amount)
  assert_eq(116, grand_total)
}