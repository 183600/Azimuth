// Azimuth 新标准测试用例
// 使用标准的 MoonBit 测试语法，包含高质量的测试场景

test "add_function_basic_arithmetic" {
  // 测试基本算术加法运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-4, -6))
  @azimuth.assert_eq(1000, @azimuth.add(999, 1))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  @azimuth.assert_eq(@azimuth.multiply(-3, 5), @azimuth.multiply(5, -3))
  
  // 乘法单位元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -9876))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的综合情况
  // 正数除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))
  
  // 零除数处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 场景：计算 (a * b) + (c * d) 的结果
  let a = 12
  let b = 8
  let c = 15
  let d = 4
  
  let product1 = @azimuth.multiply(a, b)
  let product2 = @azimuth.multiply(c, d)
  let result = @azimuth.add(product1, product2)
  
  @azimuth.assert_eq(156, result)
}

test "boolean_logic_assertions" {
  // 测试布尔逻辑断言
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(-3 < 0)
  @azimuth.assert_true(42 == 42)
  @azimuth.assert_true(-100 == -100)
  
  @azimuth.assert_false(10 < 5)
  @azimuth.assert_false(-3 > 0)
  @azimuth.assert_false(42 == 24)
  @azimuth.assert_false(-100 == 100)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算
  // 计算斐波那契数列前几项的和
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), @azimuth.add(fib3, fib4)), fib5)
  @azimuth.assert_eq(13, sum)
}

test "divide_with_ceil_negative_cases" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-6, -3))
}

test "string_processing_with_greet" {
  // 测试使用greet函数进行字符串处理
  let names = ["Alice", "Bob", "Charlie"]
  
  let greeting1 = @azimuth.greet(names[0])
  let greeting2 = @azimuth.greet(names[1])
  let greeting3 = @azimuth.greet(names[2])
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
}

test "arithmetic_overflow_boundary" {
  // 测试算术运算的溢出边界
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  
  // 测试接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
  
  // 测试边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}