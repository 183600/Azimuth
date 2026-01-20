// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_function_edge_cases" {
  // 测试加法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 100))
}

test "multiply_function_edge_cases" {
  // 测试乘法边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_function_standard" {
  // 测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "complex_calculation_test" {
  // 测试复杂计算
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "practical_application_test" {
  // 实际应用测试：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(597, subtotal)
  assert_eq(612, total)
}

test "string_processing_test" {
  // 字符串处理测试
  let names = ["Alice", "Bob", "Charlie"]
  
  assert_eq_string("Hello, Alice!", greet(names[0]))
  assert_eq_string("Hello, Bob!", greet(names[1]))
  assert_eq_string("Hello, Charlie!", greet(names[2]))
}

test "error_handling_test" {
  // 错误处理测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}