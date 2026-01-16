// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

test "basic_arithmetic_addition_edge_cases" {
  // 测试加法运算的边界情况
  
  // 零值加法
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(42, 0), 42)
  
  // 正数加法
  assert_eq(azimuth::add(1, 1), 2)
  assert_eq(azimuth::add(100, 200), 300)
  
  // 大数加法
  assert_eq(azimuth::add(10000, 20000), 30000)
}

test "basic_arithmetic_multiplication_edge_cases" {
  // 测试乘法运算的边界情况
  
  // 零值乘法
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  
  // 单位乘法
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(1, 42), 42)
  assert_eq(azimuth::multiply(42, 1), 42)
  
  // 负数乘法
  assert_eq(azimuth::multiply(-1, 1), -1)
  assert_eq(azimuth::multiply(-1, -1), 1)
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  
  // 空字符串
  assert_eq(azimuth::greet(""), "Hello, !")
  
  // 单字符
  assert_eq(azimuth::greet("A"), "Hello, A!")
  
  // 普通名字
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  
  // 带空格的名字
  assert_eq(azimuth::greet("John Doe"), "Hello, John Doe!")
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 2
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(result, 16)
  
  // a * b + c * d
  let a2 = 4
  let b2 = 3
  let c2 = 2
  let d2 = 5
  let product1 = azimuth::multiply(a2, b2)
  let product2 = azimuth::multiply(c2, d2)
  let final_result = azimuth::add(product1, product2)
  assert_eq(final_result, 22)
}

test "mathematical_sequences" {
  // 测试数学序列计算
  
  // 斐波那契数列前几项: 1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = azimuth::add(fib1, fib2)  // 2
  let fib4 = azimuth::add(fib2, fib3)  // 3
  let fib5 = azimuth::add(fib3, fib4)  // 5
  let fib6 = azimuth::add(fib4, fib5)  // 8
  
  assert_eq(fib3, 2)
  assert_eq(fib4, 3)
  assert_eq(fib5, 5)
  assert_eq(fib6, 8)
}

test "calculation_with_negative_numbers" {
  // 测试负数计算
  
  // 负数加法
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(-5, 3), -2)
  assert_eq(azimuth::add(5, -3), 2)
  
  // 负数乘法
  assert_eq(azimuth::multiply(-5, -3), 15)
  assert_eq(azimuth::multiply(-5, 3), -15)
  assert_eq(azimuth::multiply(5, -3), -15)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  
  // 计算矩形面积: 长 * 宽
  let length = 10
  let width = 5
  let area = azimuth::multiply(length, width)
  assert_eq(area, 50)
  
  // 计算周长: 2 * (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(perimeter, 30)
  
  // 计算总价: 单价 * 数量
  let unit_price = 25
  let quantity = 4
  let total_price = azimuth::multiply(unit_price, quantity)
  assert_eq(total_price, 100)
}

test "string_and_number_combinations" {
  // 测试字符串和数字的组合使用
  
  // 数字转字符串的间接测试（通过问候函数）
  let year = 2024
  let year_str = "2024"
  let greeting1 = azimuth::greet("Year " + year_str)
  assert_eq(greeting1, "Hello, Year 2024!")
  
  // 版本号处理
  let major_version = 1
  let minor_version = 0
  let version_suffix = "." + "0"
  let version_greeting = azimuth::greet("v" + "1" + version_suffix)
  assert_eq(version_greeting, "Hello, v1.0!")
}

test "performance_related_calculations" {
  // 测试性能相关的计算
  
  // 计算算法复杂度 O(n^2) 的操作数
  let n = 10
  let operations = azimuth::multiply(n, n)
  assert_eq(operations, 100)
  
  // 计算内存使用情况
  let element_size = 4  // 每个元素4字节
  let array_length = 25
  let total_memory = azimuth::multiply(element_size, array_length)
  assert_eq(total_memory, 100)
  
  // 计算时间复杂度改进
  let old_complexity = 1000
  let improvement_factor = 10
  let new_complexity = old_complexity / improvement_factor
  assert_eq(new_complexity, 100)
}