// Azimuth 核心功能测试
// 标准 MoonBit 测试语法，包含 8 个高质量测试用例

test "add_function_basic_cases" {
  // 加法函数基础测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 乘法函数综合测试
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1, multiply(1, 1))
  assert_eq(10000, multiply(100, 100))
}

test "subtract_function_edge_cases" {
  // 减法函数边界测试
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
  assert_eq(-10, subtract(5, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(0, subtract(0, 0))
}

test "divide_with_ceil_function_various_scenarios" {
  // 向上取整除法函数各种场景测试
  // 正常情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_different_inputs" {
  // 问候函数不同输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, !@#$!", greet("!@#$"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 模拟购物车计算
  let item1_price = 25
  let item1_quantity = 3
  let item2_price = 15
  let item2_quantity = 2
  
  // 计算小计
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total = add(subtotal1, subtotal2)
  
  // 计算税费（10%向上取整）
  let tax = divide_with_ceil(multiply(total, 10), 100)
  let final_total = add(total, tax)
  
  assert_eq(75, subtotal1)
  assert_eq(30, subtotal2)
  assert_eq(105, total)
  assert_eq(11, tax)
  assert_eq(116, final_total)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 减法性质
  assert_eq(add(subtract(a, b), b), a)
  
  // 乘法分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "boundary_value_analysis_comprehensive" {
  // 边界值分析综合测试
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
  assert_eq(-5, add(-5, 0))
  assert_eq(-5, multiply(-5, 1))
  assert_eq(-5, subtract(0, 5))
  assert_eq(-5, divide_with_ceil(-5, 1))
}