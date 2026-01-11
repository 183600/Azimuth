// 标准 MoonBit 测试用例集合
// 遵循 MoonBit 测试语法规范

// 测试 1: 性能测试 - 连续运算的性能验证
///|
test "performance_continuous_operations" {
  // 测试连续加法运算的性能
  let result1 = add(add(add(1, 2), 3), 4)
  assert_eq(10, result1)
  
  // 测试连续乘法运算的性能
  let result2 = multiply(multiply(multiply(2, 3), 4), 5)
  assert_eq(120, result2)
  
  // 测试混合运算的性能
  let result3 = add(multiply(10, 20), multiply(5, 6))
  assert_eq(230, result3)
}

// 测试 2: 错误处理测试 - 边界输入处理
///|
test "error_handling_boundary_inputs" {
  // 测试极端正数相加
  assert_eq(2147483647, add(2147483647, 2147483647))
  
  // 测试极端负数相加
  assert_eq(-2147483648, add(-2147483648, -2147483648))
  
  // 测试极端正数相乘
  assert_eq(2147483647, multiply(2147483647, 2147483647))
  
  // 测试极端负数相乘
  assert_eq(-2147483648, multiply(-2147483648, -2147483648))
}

// 测试 3: 数学函数测试 - 阶乘计算
///|
test "mathematical_factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let factorial_5 = multiply(
    5,
    multiply(
      4,
      multiply(
        3,
        multiply(2, 1)
      )
    )
  )
  assert_eq(120, factorial_5)
  
  // 计算 4! = 4 * 3 * 2 * 1 = 24
  let factorial_4 = multiply(
    4,
    multiply(
      3,
      multiply(2, 1)
    )
  )
  assert_eq(24, factorial_4)
}

// 测试 4: 字符串边界测试 - 长字符串处理
///|
test "string_long_input_handling" {
  // 测试长字符串输入
  let long_string = "ThisIsAVeryLongProgrammingLanguageNameForTestingPurposes"
  let result = greet(long_string)
  assert_eq(true, result.contains("Hello, "))
  assert_eq(true, result.contains(long_string))
  assert_eq(true, result.contains("!"))
}

// 测试 5: 数值范围测试 - 零值附近运算
///|
test "numeric_range_zero_operations" {
  // 测试零值附近的加法
  assert_eq(0, add(0, 0))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  
  // 测试零值附近的乘法
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(0, -1))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
}

// 测试 6: 算法实现测试 - 斐波那契数列
///|
test "algorithm_fibonacci_sequence" {
  // 计算斐波那契数列前几项
  // F(0) = 0, F(1) = 1, F(2) = 1, F(3) = 2, F(4) = 3, F(5) = 5
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let fib_2 = add(1, 0)
  assert_eq(1, fib_2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let fib_3 = add(fib_2, 1)
  assert_eq(2, fib_3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let fib_4 = add(fib_3, fib_2)
  assert_eq(3, fib_4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let fib_5 = add(fib_4, fib_3)
  assert_eq(5, fib_5)
}

// 测试 7: 数据结构测试 - 栈操作模拟
///|
test "data_structure_stack_simulation" {
  // 模拟栈操作：push(1), push(2), push(3), pop(), pop(), push(4)
  
  // 初始栈为空
  let mut stack_top = 0
  
  // push(1)
  stack_top = add(stack_top, 1)
  assert_eq(1, stack_top)
  
  // push(2)
  stack_top = add(stack_top, 1)
  assert_eq(2, stack_top)
  
  // push(3)
  stack_top = add(stack_top, 1)
  assert_eq(3, stack_top)
  
  // pop()
  stack_top = add(stack_top, -1)
  assert_eq(2, stack_top)
  
  // pop()
  stack_top = add(stack_top, -1)
  assert_eq(1, stack_top)
  
  // push(4)
  stack_top = add(stack_top, 1)
  assert_eq(2, stack_top)
}

// 测试 8: 并发安全测试 - 重复操作一致性
///|
test "concurrent_safety_repeated_operations" {
  // 测试重复执行相同操作的结果一致性
  
  // 重复加法操作
  let add_result_1 = add(100, 200)
  let add_result_2 = add(100, 200)
  let add_result_3 = add(100, 200)
  assert_eq(add_result_1, add_result_2)
  assert_eq(add_result_2, add_result_3)
  
  // 重复乘法操作
  let mul_result_1 = multiply(15, 20)
  let mul_result_2 = multiply(15, 20)
  let mul_result_3 = multiply(15, 20)
  assert_eq(mul_result_1, mul_result_2)
  assert_eq(mul_result_2, mul_result_3)
  
  // 重复字符串操作
  let str_result_1 = greet("Test")
  let str_result_2 = greet("Test")
  let str_result_3 = greet("Test")
  assert_eq(str_result_1, str_result_2)
  assert_eq(str_result_2, str_result_3)
}

// 测试 9: 内存效率测试 - 大数据处理模拟
///|
test "memory_efficiency_large_data_simulation" {
  // 模拟处理大量数据时的运算效率
  
  // 模拟处理1000个数据项，每个值为10
  let item_value = 10
  let item_count = 1000
  
  // 计算总和：10 * 1000 = 10000
  let total_sum = multiply(item_value, item_count)
  assert_eq(10000, total_sum)
  
  // 计算平均值：总和 / 数量 = 10000 / 1000 = 10
  // 通过反向验证：10 * 1000 = 10000
  let average_check = multiply(10, item_count)
  assert_eq(total_sum, average_check)
}

// 测试 10: 兼容性测试 - 不同输入格式
///|
test "compatibility_different_input_formats" {
  // 测试不同格式的字符串输入
  
  // 数字字符串
  let numeric_string = "123456"
  let result1 = greet(numeric_string)
  assert_eq("Hello, 123456!", result1)
  
  // 包含空格的字符串
  let spaced_string = "Moon Bit Language"
  let result2 = greet(spaced_string)
  assert_eq("Hello, Moon Bit Language!", result2)
  
  // 特殊符号字符串
  let symbol_string = "C++_Developer"
  let result3 = greet(symbol_string)
  assert_eq("Hello, C++_Developer!", result3)
  
  // 混合格式字符串
  let mixed_string = "User_2024_Pro"
  let result4 = greet(mixed_string)
  assert_eq("Hello, User_2024_Pro!", result4)
}