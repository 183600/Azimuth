// 标准 MoonBit 测试套件
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试 add 函数的基本功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(0, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, emoji😊!", greet("emoji😊"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：矩形面积和周长
  let length = 10
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：折扣计算
  let original_price = 1000
  let discount_percentage = 20
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(800, final_price)
}

test "sequential_operations" {
  // 测试连续运算
  // 1 + 2 + 3 + 4 + 5 = 15
  let result1 = add(add(1, 2), add(3, 4))
  let final_sum = add(result1, 5)
  assert_eq(15, final_sum)
  
  // 2 * 3 * 4 = 24
  let result2 = multiply(multiply(2, 3), 4)
  assert_eq(24, result2)
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}