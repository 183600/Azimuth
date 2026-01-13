// Azimuth 项目综合测试文件
// 使用标准 MoonBit 测试语法

// 测试用例 1: add 函数基本功能测试
test "add_basic_functionality" {
  // 测试正常加法
  let result1 = @azimuth.add(2, 3)
  let result2 = @azimuth.add(-2, 3)
  let result3 = @azimuth.add(2, -3)
  let result4 = @azimuth.add(-2, -3)
  let result5 = @azimuth.add(0, 5)
  let result6 = @azimuth.add(5, 0)
  
  // 使用项目提供的断言函数
  @azimuth.assert_eq(5, result1)
  @azimuth.assert_eq(1, result2)
  @azimuth.assert_eq(-1, result3)
  @azimuth.assert_eq(-5, result4)
  @azimuth.assert_eq(5, result5)
  @azimuth.assert_eq(5, result6)
}

// 测试用例 2: add 函数边界值测试
test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  let result1 = @azimuth.add(max_val - 1, 1)
  let result2 = @azimuth.add(min_val + 1, -1)
  
  // 测试溢出情况
  let result3 = @azimuth.add(max_val, 1)
  let result4 = @azimuth.add(max_val, max_val)
  let result5 = @azimuth.add(min_val, -1)
  let result6 = @azimuth.add(min_val, min_val)
  
  @azimuth.assert_eq(max_val, result1)
  @azimuth.assert_eq(min_val, result2)
  @azimuth.assert_eq(max_val, result3)  // 溢出应返回最大值
  @azimuth.assert_eq(max_val, result4)  // 溢出应返回最大值
  @azimuth.assert_eq(min_val, result5)  // 溢出应返回最小值
  @azimuth.assert_eq(min_val, result6)  // 溢出应返回最小值
}

// 测试用例 3: multiply 函数基本功能测试
test "multiply_basic_functionality" {
  // 测试正常乘法
  let result1 = @azimuth.multiply(2, 3)
  let result2 = @azimuth.multiply(-2, 3)
  let result3 = @azimuth.multiply(2, -3)
  let result4 = @azimuth.multiply(-2, -3)
  let result5 = @azimuth.multiply(0, 5)
  let result6 = @azimuth.multiply(5, 0)
  let result7 = @azimuth.multiply(1, 5)
  let result8 = @azimuth.multiply(5, 1)
  
  @azimuth.assert_eq(6, result1)
  @azimuth.assert_eq(-6, result2)
  @azimuth.assert_eq(-6, result3)
  @azimuth.assert_eq(6, result4)
  @azimuth.assert_eq(0, result5)
  @azimuth.assert_eq(0, result6)
  @azimuth.assert_eq(5, result7)
  @azimuth.assert_eq(5, result8)
}

// 测试用例 4: multiply 函数特殊值测试
test "multiply_special_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试 -1 的情况
  let result1 = @azimuth.multiply(-1, 5)
  let result2 = @azimuth.multiply(-1, -5)
  let result3 = @azimuth.multiply(-1, min_val)
  
  // 测试最小值的情况
  let result4 = @azimuth.multiply(min_val, 1)
  let result5 = @azimuth.multiply(min_val, -1)
  let result6 = @azimuth.multiply(min_val, 2)
  
  @azimuth.assert_eq(-5, result1)
  @azimuth.assert_eq(5, result2)
  @azimuth.assert_eq(min_val, result3)  // 特殊情况：-1 * min_val = min_val
  @azimuth.assert_eq(min_val, result4)
  @azimuth.assert_eq(min_val, result5)  // 特殊情况：min_val * -1 = min_val
  @azimuth.assert_eq(min_val, result6)  // 溢出
}

// 测试用例 5: multiply 函数边界值测试
test "multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  let result1 = @azimuth.multiply(46340, 46340)  // sqrt(max_val)
  let result2 = @azimuth.multiply(-46340, 46340)
  
  // 测试溢出情况
  let result3 = @azimuth.multiply(max_val, 2)
  let result4 = @azimuth.multiply(min_val, 2)
  let result5 = @azimuth.multiply(65536, 65536)
  
  @azimuth.assert_eq(2147395600, result1)
  @azimuth.assert_eq(-2147395600, result2)
  @azimuth.assert_eq(max_val, result3)  // 溢出应返回最大值
  @azimuth.assert_eq(min_val, result4)  // 溢出应返回最小值
  @azimuth.assert_eq(max_val, result5)  // 溢出应返回最大值
}

// 测试用例 6: greet 函数基本功能测试
test "greet_basic_functionality" {
  // 测试基本字符串拼接
  let result1 = @azimuth.greet("World")
  let result2 = @azimuth.greet("MoonBit")
  let result3 = @azimuth.greet("")
  let result4 = @azimuth.greet("中文")
  
  @azimuth.assert_eq_string("Hello, World!", result1)
  @azimuth.assert_eq_string("Hello, MoonBit!", result2)
  @azimuth.assert_eq_string("Hello, !", result3)
  @azimuth.assert_eq_string("Hello, 中文!", result4)
}

// 测试用例 7: 断言函数测试
test "assertion_functions" {
  // 测试断言函数正常工作
  @azimuth.assert_eq(5, 5)
  @azimuth.assert_eq_string("hello", "hello")
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试比较操作
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(3 < 5)
  @azimuth.assert_true(5 >= 5)
  @azimuth.assert_true(5 <= 5)
  @azimuth.assert_true(5 == 5)
  @azimuth.assert_true(5 != 3)
  
  @azimuth.assert_false(5 > 10)
  @azimuth.assert_false(3 < 1)
  @azimuth.assert_false(5 >= 10)
  @azimuth.assert_false(5 <= 1)
  @azimuth.assert_false(5 == 3)
  @azimuth.assert_false(5 != 5)
}

// 测试用例 8: 复合运算测试
test "complex_operations" {
  // 测试多个函数的组合使用
  let add_result = @azimuth.add(10, 20)
  let multiply_result = @azimuth.multiply(add_result, 2)
  let greet_result = @azimuth.greet("Result")
  
  @azimuth.assert_eq(30, add_result)
  @azimuth.assert_eq(60, multiply_result)
  @azimuth.assert_eq_string("Hello, Result!", greet_result)
  
  // 测试链式计算
  let chain_result = @azimuth.multiply(
    @azimuth.add(5, 3),
    @azimuth.add(2, 4)
  )
  @azimuth.assert_eq(64, chain_result)
}

// 测试用例 9: 边界条件组合测试
test "boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值的组合运算
  let result1 = @azimuth.add(1, @azimuth.multiply(2, 3))
  let result2 = @azimuth.multiply(2, @azimuth.add(3, 4))
  let result3 = @azimuth.add(max_val - 10, 5)
  let result4 = @azimuth.multiply(100, 100)
  
  @azimuth.assert_eq(7, result1)
  @azimuth.assert_eq(14, result2)
  @azimuth.assert_eq(max_val - 5, result3)
  @azimuth.assert_eq(10000, result4)
}

// 测试用例 10: 错误处理和特殊情况
test "error_handling_and_special_cases" {
  // 测试零值处理
  let add_zero1 = @azimuth.add(0, 0)
  let add_zero2 = @azimuth.add(0, -5)
  let add_zero3 = @azimuth.add(0, 5)
  
  let multiply_zero1 = @azimuth.multiply(0, 0)
  let multiply_zero2 = @azimuth.multiply(0, -5)
  let multiply_zero3 = @azimuth.multiply(0, 5)
  
  @azimuth.assert_eq(0, add_zero1)
  @azimuth.assert_eq(-5, add_zero2)
  @azimuth.assert_eq(5, add_zero3)
  
  @azimuth.assert_eq(0, multiply_zero1)
  @azimuth.assert_eq(0, multiply_zero2)
  @azimuth.assert_eq(0, multiply_zero3)
  
  // 测试字符串边界情况
  let long_name = "这是一个非常长的名字用来测试greet函数是否能正确处理长字符串"
  let greet_long = @azimuth.greet(long_name)
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, greet_long)
}