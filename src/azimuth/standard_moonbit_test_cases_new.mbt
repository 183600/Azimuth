// 新增的标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(-1, azimuth::add(-3, 2))
  azimuth::assert_eq(100, azimuth::add(50, 50))
  azimuth::assert_eq(-100, azimuth::add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 1))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(0, subtract(5, 5))
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法函数的综合情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法函数的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(45, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(2, result3)
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

test "boundary_value_analysis" {
  // 测试边界值分析
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负一测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "real_world_shopping_cart" {
  // 测试真实世界购物车计算
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