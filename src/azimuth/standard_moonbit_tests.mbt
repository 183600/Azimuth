// 标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(10, 8))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_edge_cases" {
  // 除法边界情况测试
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
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
  
  // 减法逆运算
  assert_eq(add(subtract(x, y), y), x)
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

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 25)        // 325
  let step3 = subtract(step2, 50)   // 275
  let step4 = divide_with_ceil(step3, 4)  // 69
  
  assert_eq(300, step1)
  assert_eq(325, step2)
  assert_eq(275, step3)
  assert_eq(69, step4)
}

test "large_number_operations" {
  // 大数运算测试
  let large1 = 100000
  let large2 = 50000
  
  assert_eq(150000, add(large1, large2))
  assert_eq(50000, subtract(large1, large2))
  assert_eq(5000000000, multiply(large1, large2))
  assert_eq(2, divide_with_ceil(large1, large2))
  assert_eq(33334, divide_with_ceil(100000, 3))
}

test "precision_and_rounding" {
  // 精度和舍入测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(1, divide_with_ceil(3, 4))
  assert_eq(2, divide_with_ceil(100, 99))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(34, divide_with_ceil(101, 3))
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算测试
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  let tax_rate = 10
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 30
  let total = add(subtotal1, subtotal2)             // 105
  
  // 计算税费
  let tax = divide_with_ceil(multiply(total, tax_rate), 100)  // 11
  let grand_total = add(total, tax)                          // 116
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}