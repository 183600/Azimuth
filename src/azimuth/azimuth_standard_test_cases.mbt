// Azimuth 项目标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(7, subtract(10, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "string_operations_basic" {
  // 测试基本字符串操作
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "division_edge_cases" {
  // 测试除法边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(-8, add(-5, -3))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(24, multiply(-4, -6))
  assert_eq(-17, subtract(-10, 7))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial = 100
  let step1 = multiply(initial, 3)  // 300
  let step2 = add(step1, 15)        // 315
  let step3 = subtract(step2, 2)    // 313
  let step4 = divide_with_ceil(step3, 4)  // 79
  
  assert_eq(300, step1)
  assert_eq(315, step2)
  assert_eq(313, step3)
  assert_eq(79, step4)
}

test "mathematical_properties" {
  // 测试数学性质
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
  // 测试边界值分析
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

test "practical_application_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 25
  let item1_qty = 3
  let item2_price = 15
  let item2_qty = 2
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_qty)  // 75
  let subtotal2 = multiply(item2_price, item2_qty)  // 30
  
  // 计算总价
  let total = add(subtotal1, subtotal2)  // 105
  
  // 计算税费（10%，向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)  // 11
  
  // 计算最终金额
  let grand_total = add(total, tax)  // 116
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, grand_total)
}