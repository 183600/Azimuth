// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
  assert_eq(10, add(-5, 15))
}

test "add_function_overflow_handling" {
  // 测试 add 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-8, multiply(-2, 4))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_function_overflow_handling" {
  // 测试 multiply 函数的溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负溢出
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算矩形的面积和周长
  let length = 5
  let width = 3
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(15, area)
  assert_eq(16, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 特殊值运算
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(597, subtotal)
  assert_eq(612, total)
  
  // 温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)
}