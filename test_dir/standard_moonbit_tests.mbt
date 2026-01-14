// 标准 MoonBit 测试用例
// 这些测试用例使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "basic_addition_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "addition_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 测试与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, add(0, 0))
}

test "basic_multiplication_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(100, multiply(10, 10))
}

test "multiplication_boundary_values" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
  
  // (a + b + c) * 2
  let result3 = multiply(add(add(a, b), c), 2)
  assert_eq(120, result3)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = multiply(2, add(length, width))
  assert_eq(50, perimeter)
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(160, discounted_price)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  assert_eq_string("Hello, Hello World!", greet(spaced_string))
  
  // 测试包含数字的字符串
  let numeric_string = "User2023"
  assert_eq_string("Hello, User2023!", greet(numeric_string))
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
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界条件的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, -1) == min_val)
}