// Azimuth 标准测试用例集
// 覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 基本加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  // 零值处理
  assert_eq(15, add(0, 15))
  assert_eq(-20, add(-20, 0))
  assert_eq(0, add(0, 0))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(4, -3))
  assert_eq(16, multiply(-4, -4))
  
  // 单位元测试
  assert_eq(8, multiply(1, 8))
  assert_eq(9, multiply(9, 1))
  assert_eq(-7, multiply(-1, 7))
  assert_eq(0, multiply(0, 123))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 基本问候测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  
  // 特殊字符串测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // 包含空格的名称
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, MoonBit Language!", greet("MoonBit Language"))
}

test "divide_with_ceil_function_various_cases" {
  // 基本向上取整测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  
  // 边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
}

test "assert_functions_basic" {
  // 基本断言测试
  assert_true(true)
  assert_false(false)
  assert_eq(100, 100)
  assert_eq_string("hello", "hello")
  
  // 数值比较
  assert_eq(0, add(5, -5))
  assert_eq(25, multiply(5, 5))
  
  // 布尔表达式
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(5 == 5)
  assert_false(5 == 3)
}

test "combined_operations" {
  // 复合运算测试
  let x = 12
  let y = 8
  let z = -4
  
  // (x + y) * z
  let sum = add(x, y)
  let result = multiply(sum, z)
  assert_eq(-80, result)
  
  // x * y + z
  let product = multiply(x, y)
  let result2 = add(product, z)
  assert_eq(92, result2)
  
  // 链式运算
  let result3 = add(multiply(5, 3), multiply(2, 4))
  assert_eq(23, result3)
}

test "edge_cases_and_special_values" {
  // 特殊值和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  assert_eq(max_val, add(max_val - 1000, 1000))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值运算
  assert_eq(min_val, add(min_val + 1000, -1000))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 混合符号运算
  assert_eq(0, add(max_val, min_val + 1))
  assert_eq(-1, add(max_val, min_val))
}

test "string_operations_and_greetings" {
  // 字符串相关操作测试
  let name1 = "MoonBit"
  let name2 = "Developer"
  let name3 = ""
  
  // 不同长度的名称
  assert_eq_string("Hello, MoonBit!", greet(name1))
  assert_eq_string("Hello, Developer!", greet(name2))
  assert_eq_string("Hello, !", greet(name3))
  
  // 包含特殊字符的名称
  assert_eq_string("Hello, John-Doe!", greet("John-Doe"))
  assert_eq_string("Hello, O'Reilly!", greet("O'Reilly"))
  assert_eq_string("Hello, C++!", greet("C++"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元
  assert_eq(a, add(a, 0))
  assert_eq(b, add(0, b))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
  assert_eq(b, multiply(1, b))
  
  // 乘法零元
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, b))
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  
  // 极端数值测试
  let large_positive = 1000000
  let large_negative = -1000000
  let small_positive = 1
  let small_negative = -1
  
  // 大数运算
  assert_eq(2000000, add(large_positive, large_positive))
  assert_eq(-2000000, add(large_negative, large_negative))
  assert_eq(0, add(large_positive, large_negative))
  
  // 小数与大数混合
  assert_eq(1000001, add(large_positive, small_positive))
  assert_eq(-999999, add(large_negative, small_positive))
  
  // 乘法测试
  assert_eq(1000000, multiply(large_positive, small_positive))
  assert_eq(-1000000, multiply(large_positive, small_negative))
  assert_eq(1000000, multiply(large_negative, small_negative))
  
  // 向上取整除法的特殊情况
  assert_eq(1000000, divide_with_ceil(large_positive, 1))
  assert_eq(-1000000, divide_with_ceil(large_negative, 1))
  assert_eq(1, divide_with_ceil(large_positive, large_positive))
}