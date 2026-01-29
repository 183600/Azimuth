// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含对所有导出函数的基本测试

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(3, subtract(5, 2))
  assert_eq(0, subtract(5, 5))
  assert_eq(-3, subtract(2, 5))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  // 测试基本向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result = add(multiply(2, 3), subtract(10, 4))
  assert_eq(12, result)  // 2*3 + (10-4) = 6 + 6 = 12
}

test "practical_business_calculation" {
  // 实际业务计算场景
  let unit_price = 25
  let quantity = 4
  let discount = 15
  
  let subtotal = multiply(unit_price, quantity)
  let final_total = subtract(subtotal, discount)
  
  assert_eq(100, subtotal)
  assert_eq(85, final_total)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 13
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 减法非交换性
  assert_neq(subtract(x, y), subtract(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
}

test "edge_cases_and_boundary_values" {
  // 边界值和特殊情况测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq_string("Hello, !", greet(""))
}