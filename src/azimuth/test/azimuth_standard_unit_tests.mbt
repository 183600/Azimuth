// Azimuth 项目标准单元测试
// 针对 lib.mbt 中函数的测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试 add 函数的基本运算
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(10, azimuth::add(10, 0))
  azimuth::assert_eq(15, azimuth::add(0, 15))
  azimuth::assert_eq(-5, azimuth::add(-2, -3))
  azimuth::assert_eq(0, azimuth::add(5, -5))
}

test "add_function_boundary_values" {
  // 测试 add 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  azimuth::assert_eq(max_val, azimuth::add(2147483640, 7))
  azimuth::assert_eq(max_val, azimuth::add(1000000000, 1147483647))
  
  // 测试最小值的特殊情况
  azimuth::assert_eq(min_val, azimuth::add(min_val, 0))
  azimuth::assert_eq(min_val, azimuth::add(0, min_val))
  azimuth::assert_eq(min_val, azimuth::add(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::add(-1, min_val))
  
  // 测试负数相加的边界
  azimuth::assert_eq(min_val, azimuth::add(-2147483640, -8))
}

test "multiply_function_basic_operations" {
  // 测试 multiply 函数的基本运算
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(0, azimuth::multiply(0, 5))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(5, azimuth::multiply(5, 1))
  azimuth::assert_eq(5, azimuth::multiply(1, 5))
  azimuth::assert_eq(-5, azimuth::multiply(5, -1))
  azimuth::assert_eq(-5, azimuth::multiply(-1, 5))
  azimuth::assert_eq(25, azimuth::multiply(-5, -5))
}

test "multiply_function_boundary_values" {
  // 测试 multiply 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的特殊情况
  azimuth::assert_eq(100, azimuth::multiply(100, 1))
  azimuth::assert_eq(100, azimuth::multiply(1, 100))
  azimuth::assert_eq(-100, azimuth::multiply(100, -1))
  azimuth::assert_eq(-100, azimuth::multiply(-1, 100))
  
  // 测试最小值的特殊情况
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
  azimuth::assert_eq(min_val, azimuth::multiply(1, min_val))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::multiply(-1, min_val))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 2))
  azimuth::assert_eq(min_val, azimuth::multiply(2, min_val))
  
  // 测试溢出情况
  azimuth::assert_eq(max_val, azimuth::multiply(46340, 46340))
  azimuth::assert_eq(min_val, azimuth::multiply(-46340, 46340))
}

test "greet_function_basic_operations" {
  // 测试 greet 函数的基本功能
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, Alice!", azimuth::greet("Alice"))
  azimuth::assert_eq_string("Hello, Bob!", azimuth::greet("Bob"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("  "))  // 空格
  azimuth::assert_eq_string("Hello, 中文!", azimuth::greet("中文"))  // 中文字符
  azimuth::assert_eq_string("Hello, !@#$!", azimuth::greet("!@#$"))  // 特殊字符
}

test "divide_with_ceil_basic_operations" {
  // 测试 divide_with_ceil 函数的基本运算
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))  // 10/3 = 3.33... 向上取整为 4
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.66... 向上取整为 4
  azimuth::assert_eq(4, azimuth::divide_with_ceil(12, 3))  // 12/3 = 4 正好整除
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))   // 0/5 = 0
  azimuth::assert_eq(5, azimuth::divide_with_ceil(25, 5))  // 25/5 = 5 正好整除
}

test "divide_with_ceil_negative_numbers" {
  // 测试 divide_with_ceil 函数的负数情况
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))   // -10/3 = -3.33... 向上取整为 -3
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-11, 3))   // -11/3 = -3.66... 向上取整为 -3
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(-10, -3))  // -10/-3 = 3.33... 向上取整为 4
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(-11, -3))  // -11/-3 = 3.66... 向上取整为 4
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(10, -3))    // 10/-3 = -3.33... 向上取整为 -3
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(11, -3))    // 11/-3 = -3.66... 向上取整为 -3
}

test "comprehensive_function_integration" {
  // 综合测试：多个函数的组合使用
  let a = 10
  let b = 3
  
  // 使用 add 和 multiply 组合
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(a, b)
  let combined = azimuth::add(sum, product)
  azimuth::assert_eq(43, combined)  // (10+3) + (10*3) = 13 + 30 = 43
  
  // 使用 divide_with_ceil 和其他函数组合
  let ceil_result = azimuth::divide_with_ceil(product, a)
  let final_result = azimuth::multiply(ceil_result, sum)
  azimuth::assert_eq(39, final_result)  // ceil(30/10) * 13 = 3 * 13 = 39
  
  // 字符串和数值组合测试
  let greeting = azimuth::greet("Number")
  let number_result = azimuth::add(5, 3)
  azimuth::assert_eq_string("Hello, Number!", greeting)
  azimuth::assert_eq(8, number_result)
}