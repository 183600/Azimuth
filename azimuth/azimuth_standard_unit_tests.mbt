// Azimuth 项目标准单元测试
// 针对 lib.mbt 中函数的测试用例
// 使用标准 MoonBit 测试语法

test "add_function_basic_operations" {
  // 测试 add 函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(10, add(10, 0))
  assert_eq(15, add(0, 15))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_function_boundary_values" {
  // 测试 add 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  assert_eq(max_val, add(2147483640, 7))
  assert_eq(max_val, add(1000000000, 1147483647))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(0, min_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(-1, min_val))
  
  // 测试负数相加的边界
  assert_eq(min_val, add(-2147483640, -8))
}

test "multiply_function_basic_operations" {
  // 测试 multiply 函数的基本运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(25, multiply(-5, -5))
}

test "multiply_function_boundary_values" {
  // 测试 multiply 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试1和-1的特殊情况
  assert_eq(100, multiply(100, 1))
  assert_eq(100, multiply(1, 100))
  assert_eq(-100, multiply(100, -1))
  assert_eq(-100, multiply(-1, 100))
  
  // 测试最小值的特殊情况
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(2, min_val))
  
  // 测试溢出情况
  assert_eq(max_val, multiply(46340, 46340))
  assert_eq(min_val, multiply(-46340, 46340))
}

test "greet_function_basic_operations" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, 中文!", greet("中文"))  // 中文字符
  assert_eq_string("Hello, !@#$!", greet("!@#$"))  // 特殊字符
}

test "divide_with_ceil_basic_operations" {
  // 测试 divide_with_ceil 函数的基本运算
  assert_eq(3, divide_with_ceil(10, 3))  // 10/3 = 3.33... 向上取整为 4
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.66... 向上取整为 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 正好整除
  assert_eq(0, divide_with_ceil(0, 5))   // 0/5 = 0
  assert_eq(5, divide_with_ceil(25, 5))  // 25/5 = 5 正好整除
}

test "divide_with_ceil_negative_numbers" {
  // 测试 divide_with_ceil 函数的负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))   // -10/3 = -3.33... 向上取整为 -3
  assert_eq(-3, divide_with_ceil(-11, 3))   // -11/3 = -3.66... 向上取整为 -3
  assert_eq(-4, divide_with_ceil(-10, -3))  // -10/-3 = 3.33... 向上取整为 4
  assert_eq(-4, divide_with_ceil(-11, -3))  // -11/-3 = 3.66... 向上取整为 4
  assert_eq(3, divide_with_ceil(10, -3))    // 10/-3 = -3.33... 向上取整为 -3
  assert_eq(3, divide_with_ceil(11, -3))    // 11/-3 = -3.66... 向上取整为 -3
}

test "divide_with_ceil_boundary_values" {
  // 测试 divide_with_ceil 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试大数除法
  assert_eq(2147483647, divide_with_ceil(max_val, 1))
  assert_eq(1073741824, divide_with_ceil(max_val, 2))
  assert_eq(1, divide_with_ceil(max_val, max_val))
  assert_eq(2, divide_with_ceil(max_val, max_val - 1))
  
  // 测试最小值除法
  assert_eq(min_val, divide_with_ceil(min_val, 1))
  assert_eq(-1073741824, divide_with_ceil(min_val, 2))
  assert_eq(1, divide_with_ceil(min_val, min_val))
  assert_eq(0, divide_with_ceil(min_val, max_val))
}

test "comprehensive_function_integration" {
  // 综合测试：多个函数的组合使用
  let a = 10
  let b = 3
  
  // 使用 add 和 multiply 组合
  let sum = add(a, b)
  let product = multiply(a, b)
  let combined = add(sum, product)
  assert_eq(43, combined)  // (10+3) + (10*3) = 13 + 30 = 43
  
  // 使用 divide_with_ceil 和其他函数组合
  let ceil_result = divide_with_ceil(product, a)
  let final_result = multiply(ceil_result, sum)
  assert_eq(12, final_result)  // ceil(30/10) * 13 = 3 * 13 = 39
  
  // 字符串和数值组合测试
  let greeting = greet("Number")
  let number_result = add(5, 3)
  assert_eq_string("Hello, Number!", greeting)
  assert_eq(8, number_result)
}