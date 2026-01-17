// Azimuth 项目聚焦测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "divide_with_ceil_functionality" {
  // 测试 divide_with_ceil 函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "combined_operations_complex" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(900, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
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

test "real_world_scenarios" {
  // 测试实际应用场景
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(40, area)
  assert_eq(26, perimeter)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let special_chars = "!@#$%^&*()"
  let unicode_text = "Hello 世界 🌍"
  let numbers_text = "1234567890"
  
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  assert_eq_string("Hello, Hello 世界 🌍!", greet(unicode_text))
  assert_eq_string("Hello, 1234567890!", greet(numbers_text))
}

test "assert_functions_verification" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  assert_true(add(x, y) == 30)
  assert_false(multiply(x, y) == 150)
}