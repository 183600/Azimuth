// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(5, add(2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 2))
}

test "negative_number_calculations" {
  // 负数计算测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(24, multiply(-4, -6))
  assert_eq(-17, subtract(-10, 7))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
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
  // 边界值分析测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 负一测试
  assert_eq(0, add(-1, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(1, subtract(0, -1))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  let a = 8
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(22, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(26, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(3, result3)
}

test "precision_and_rounding" {
  // 精度和舍入测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(2, divide_with_ceil(100, 99))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 负数舍入
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(500000, subtract(1000000, 500000))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  
  // 边界大数测试
  let big_num = 1000000
  assert_eq(1000001, add(big_num, 1))
  assert_eq(999999, subtract(big_num, 1))
}

test "real_world_application_simulation" {
  // 真实世界应用模拟测试
  // 购物车计算场景
  let item_price = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(item_price, quantity)
  assert_eq(100, subtotal)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  assert_eq(10, tax)
  
  // 计算总价
  let total = add(subtotal, tax)
  assert_eq(110, total)
  
  // 计算每箱包装数量（每箱最多12个）
  let boxes_needed = divide_with_ceil(quantity, 12)
  assert_eq(1, boxes_needed)
}