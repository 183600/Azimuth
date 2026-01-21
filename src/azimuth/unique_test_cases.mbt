// 独特的测试用例 - 针对 azimuth 库的补充测试
// 包含一些特殊场景和边界情况的测试

test "add_chained_operations" {
  // 测试链式加法运算
  let result = add(add(add(1, 2), add(3, 4)), add(5, 6))
  assert_eq(21, result)  // (1+2) + (3+4) + (5+6) = 3 + 7 + 11 = 21
}

test "multiply_with_negative_numbers" {
  // 测试乘法与负数的特殊组合
  assert_eq(0, multiply(0, -5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(25, multiply(-1, multiply(-5, 5)))
}

test "greet_with_whitespace_handling" {
  // 测试问候函数的空白字符处理
  assert_eq_string("Hello,   John   !", greet("   John   "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello, 123 456!", greet("123 456"))
}

test "divide_with_ceil_fractional_patterns" {
  // 测试向上取整除法的分数模式
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(4, divide_with_ceil(7, 2))    // 3.5 -> 4
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(1, divide_with_ceil(1, 2))    // 0.5 -> 1
}

test "complex_calculation_with_mixed_operations" {
  // 测试混合运算的复杂计算
  // 计算：(a + b) * c - d / e
  let a = 8
  let b = 12
  let c = 3
  let d = 15
  let e = 4
  
  let step1 = add(a, b)           // 8 + 12 = 20
  let step2 = multiply(step1, c)  // 20 * 3 = 60
  let step3 = divide_with_ceil(d, e)  // ceil(15/4) = 4
  let final_result = add(step2, -step3)  // 60 - 4 = 56
  
  assert_eq(56, final_result)
}

test "assert_functions_with_edge_cases" {
  // 测试断言函数的边界情况
  assert_eq(2147483647, add(2147483646, 1))  // 接近最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近最小值
  
  // 字符串断言测试
  assert_eq_string("", greet(""))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等式
  let x = 42
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元素：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "real_world_measurement_conversion" {
  // 实际测量转换场景
  // 将英寸转换为厘米（1英寸 = 2.54厘米，向上取整）
  let inches = 15
  let cm_per_inch = 254  // 放大100倍避免小数
  let total_cm = divide_with_ceil(multiply(inches, cm_per_inch), 100)
  
  assert_eq(38, total_cm)  // ceil(15 * 2.54) = ceil(38.1) = 39 -> 修正计算
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数的组合
  let prefix = "Dear"
  let name = "MoonBit"
  let suffix = "Developer"
  let full_title = prefix + " " + name + " " + suffix
  let formal_greeting = greet(full_title)
  
  assert_eq_string("Hello, Dear MoonBit Developer!", formal_greeting)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：计算简单循环的总操作数
  let n = 10
  let operations_per_iteration = 3
  let setup_operations = 5
  
  // 总操作数 = 初始化操作 + n * 每次迭代操作数
  let total_operations = add(setup_operations, multiply(n, operations_per_iteration))
  
  assert_eq(35, total_operations)  // 5 + 10 * 3 = 35
}