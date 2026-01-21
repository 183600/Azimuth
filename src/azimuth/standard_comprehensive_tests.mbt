// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, multiply(0, 5))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
}

test "greet_function_standard_cases" {
  // 测试 greet 函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, a_b-c!", greet("a_b-c"))
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(45, result)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(53, result2)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 7
  let y = 4
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 2)), add(multiply(x, y), multiply(x, 2)))
}

test "real_world_calculation_scenarios" {
  // 测试实际应用计算场景
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(48, area)
  assert_eq(28, perimeter)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let normal_val = 1000
  
  // 混合边界值和正常值的运算
  assert_eq(max_val, add(max_val, normal_val))
  assert_eq(min_val, add(min_val, -normal_val))
  assert_eq(max_val, multiply(normal_val, max_val))
  assert_eq(0, multiply(normal_val, 0))
}