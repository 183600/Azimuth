// 标准 MoonBit 测试用例
// 包含10个测试用例，涵盖基本功能测试

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-3, add(-8, 5))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(0, 100))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 3))
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

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 零值运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  
  // 单位元运算
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  
  // 边界值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算矩形的周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算三角形的面积：底 × 高 ÷ 2
  let base = 12
  let height = 8
  let area = multiply(base, height) / 2
  assert_eq(48, area)
}

test "string_processing" {
  // 测试字符串处理
  // 测试包含特殊字符的字符串
  let special_name = "test@example.com"
  assert_eq_string("Hello, test@example.com!", greet(special_name))
  
  // 测试包含Unicode字符的字符串
  let unicode_name = "测试用户"
  assert_eq_string("Hello, 测试用户!", greet(unicode_name))
  
  // 测试包含数字的字符串
  let numeric_name = "user2023"
  assert_eq_string("Hello, user2023!", greet(numeric_name))
}

test "assert_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}