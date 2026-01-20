// Azimuth 标准测试用例集
// 覆盖核心功能和边界情况

test "add_function_comprehensive" {
  // 基本加法测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 零值处理
  @azimuth.assert_eq(15, @azimuth.add(0, 15))
  @azimuth.assert_eq(-20, @azimuth.add(-20, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_function_comprehensive" {
  // 基本乘法测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(4, -3))
  @azimuth.assert_eq(16, @azimuth.multiply(-4, -4))
  
  // 单位元测试
  @azimuth.assert_eq(8, @azimuth.multiply(1, 8))
  @azimuth.assert_eq(9, @azimuth.multiply(9, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 特殊字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  
  // 包含空格的名称
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, MoonBit Language!", @azimuth.greet("MoonBit Language"))
}

test "divide_with_ceil_function_various_cases" {
  // 基本向上取整测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  
  // 边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
}

test "assert_functions_basic" {
  // 基本断言测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("hello", "hello")
  
  // 数值比较
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  
  // 布尔表达式
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(3 > 5)
  @azimuth.assert_true(5 == 5)
  @azimuth.assert_false(5 == 3)
}

test "combined_operations" {
  // 复合运算测试
  let x = 12
  let y = 8
  let z = -4
  
  // (x + y) * z
  let sum = @azimuth.add(x, y)
  let result = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-80, result)
  
  // x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(92, result2)
  
  // 链式运算
  let result3 = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(2, 4))
  @azimuth.assert_eq(23, result3)
}

test "edge_cases_and_special_values" {
  // 特殊值和边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 混合符号运算
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_operations_and_greetings" {
  // 字符串相关操作测试
  let name1 = "MoonBit"
  let name2 = "Developer"
  let name3 = ""
  
  // 不同长度的名称
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(name1))
  @azimuth.assert_eq_string("Hello, Developer!", @azimuth.greet(name2))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(name3))
  
  // 包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, John-Doe!", @azimuth.greet("John-Doe"))
  @azimuth.assert_eq_string("Hello, O'Reilly!", @azimuth.greet("O'Reilly"))
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
}

test "mathematical_properties" {
  // 数学性质验证测试
  
  // 加法交换律
  let a = 15
  let b = 27
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法单位元
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(b, @azimuth.add(0, b))
  
  // 乘法单位元
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  
  // 极端数值测试
  let large_positive = 1000000
  let large_negative = -1000000
  let small_positive = 1
  let small_negative = -1
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(large_positive, large_positive))
  @azimuth.assert_eq(-2000000, @azimuth.add(large_negative, large_negative))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, large_negative))
  
  // 小数与大数混合
  @azimuth.assert_eq(1000001, @azimuth.add(large_positive, small_positive))
  @azimuth.assert_eq(-999999, @azimuth.add(large_negative, small_positive))
  
  // 乘法测试
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_positive, small_positive))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(large_positive, small_negative))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_negative, small_negative))
  
  // 向上取整除法的特殊情况
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(large_positive, 1))
  @azimuth.assert_eq(-1000000, @azimuth.divide_with_ceil(large_negative, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_positive, large_positive))
}