// 专业级 MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  let result1 = @azimuth.add(1, 2)
  if result1 != 3 { @builtin.abort("add(1, 2) should be 3") }
  
  let result2 = @azimuth.add(0, 0)
  if result2 != 0 { @builtin.abort("add(0, 0) should be 0") }
  
  let result3 = @azimuth.add(-1, 1)
  if result3 != 0 { @builtin.abort("add(-1, 1) should be 0") }
  
  let result4 = @azimuth.add(100, 200)
  if result4 != 300 { @builtin.abort("add(100, 200) should be 300") }
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  let result1 = @azimuth.multiply(2, 3)
  if result1 != 6 { @builtin.abort("multiply(2, 3) should be 6") }
  
  let result2 = @azimuth.multiply(0, 5)
  if result2 != 0 { @builtin.abort("multiply(0, 5) should be 0") }
  
  let result3 = @azimuth.multiply(-1, 1)
  if result3 != -1 { @builtin.abort("multiply(-1, 1) should be -1") }
  
  let result4 = @azimuth.multiply(10, 10)
  if result4 != 100 { @builtin.abort("multiply(10, 10) should be 100") }
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  let result1 = @azimuth.subtract(5, 3)
  if result1 != 2 { @builtin.abort("subtract(5, 3) should be 2") }
  
  let result2 = @azimuth.subtract(0, 0)
  if result2 != 0 { @builtin.abort("subtract(0, 0) should be 0") }
  
  let result3 = @azimuth.subtract(1, 1)
  if result3 != 0 { @builtin.abort("subtract(1, 1) should be 0") }
  
  let result4 = @azimuth.subtract(100, 50)
  if result4 != 50 { @builtin.abort("subtract(100, 50) should be 50") }
}

test "division_with_ceil_basic" {
  // 测试向上取整除法
  let result1 = @azimuth.divide_with_ceil(10, 3)
  if result1 != 4 { @builtin.abort("divide_with_ceil(10, 3) should be 4") }
  
  let result2 = @azimuth.divide_with_ceil(9, 3)
  if result2 != 3 { @builtin.abort("divide_with_ceil(9, 3) should be 3") }
  
  let result3 = @azimuth.divide_with_ceil(1, 2)
  if result3 != 1 { @builtin.abort("divide_with_ceil(1, 2) should be 1") }
  
  let result4 = @azimuth.divide_with_ceil(0, 5)
  if result4 != 0 { @builtin.abort("divide_with_ceil(0, 5) should be 0") }
}

test "division_edge_cases" {
  // 测试除法边界情况
  let result1 = @azimuth.divide_with_ceil(10, 0)
  if result1 != 0 { @builtin.abort("divide_with_ceil(10, 0) should be 0") }
  
  let result2 = @azimuth.divide_with_ceil(0, 0)
  if result2 != 0 { @builtin.abort("divide_with_ceil(0, 0) should be 0") }
  
  let result3 = @azimuth.divide_with_ceil(-10, 3)
  if result3 != -3 { @builtin.abort("divide_with_ceil(-10, 3) should be -3") }
  
  let result4 = @azimuth.divide_with_ceil(10, -3)
  if result4 != 3 { @builtin.abort("divide_with_ceil(10, -3) should be 3") }
}

test "greet_function_basic" {
  // 测试问候函数
  let result1 = @azimuth.greet("MoonBit")
  if result1 != "Hello, MoonBit!" { @builtin.abort("greet(\"MoonBit\") should be \"Hello, MoonBit!\"") }
  
  let result2 = @azimuth.greet("World")
  if result2 != "Hello, World!" { @builtin.abort("greet(\"World\") should be \"Hello, World!\"") }
  
  let result3 = @azimuth.greet("")
  if result3 != "Hello, !" { @builtin.abort("greet(\"\") should be \"Hello, !\"") }
}

test "complex_calculation_sequence" {
  // 测试复合计算序列
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let add_result = @azimuth.add(a, b)
  let result1 = @azimuth.multiply(add_result, c)
  if result1 != 30 { @builtin.abort("(10 + 5) * 2 should be 30") }
  
  // a * b + c
  let multiply_result = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(multiply_result, c)
  if result2 != 52 { @builtin.abort("10 * 5 + 2 should be 52") }
  
  // (a - b) / c (向上取整)
  let subtract_result = @azimuth.subtract(a, b)
  let result3 = @azimuth.divide_with_ceil(subtract_result, c)
  if result3 != 3 { @builtin.abort("divide_with_ceil(10 - 5, 2) should be 3") }
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 3
  
  // 加法交换律
  let result1 = @azimuth.add(x, y)
  let result2 = @azimuth.add(y, x)
  if result1 != result2 { @builtin.abort("addition should be commutative") }
  
  // 乘法交换律
  let result3 = @azimuth.multiply(x, y)
  let result4 = @azimuth.multiply(y, x)
  if result3 != result4 { @builtin.abort("multiplication should be commutative") }
  
  // 加法单位元
  let result5 = @azimuth.add(x, 0)
  if result5 != x { @builtin.abort("addition with zero should return the original number") }
  
  // 乘法单位元
  let result6 = @azimuth.multiply(x, 1)
  if result6 != x { @builtin.abort("multiplication with one should return the original number") }
}

test "boundary_value_testing" {
  // 测试边界值
  // 测试零值
  let result1 = @azimuth.add(0, 0)
  if result1 != 0 { @builtin.abort("add(0, 0) should be 0") }
  
  let result2 = @azimuth.multiply(0, 5)
  if result2 != 0 { @builtin.abort("multiply(0, 5) should be 0") }
  
  let result3 = @azimuth.subtract(0, 0)
  if result3 != 0 { @builtin.abort("subtract(0, 0) should be 0") }
  
  // 测试单位值
  let result4 = @azimuth.add(1, 0)
  if result4 != 1 { @builtin.abort("add(1, 0) should be 1") }
  
  let result5 = @azimuth.multiply(1, 5)
  if result5 != 5 { @builtin.abort("multiply(1, 5) should be 5") }
  
  let result6 = @azimuth.subtract(1, 0)
  if result6 != 1 { @builtin.abort("subtract(1, 0) should be 1") }
  
  let result7 = @azimuth.divide_with_ceil(5, 1)
  if result7 != 5 { @builtin.abort("divide_with_ceil(5, 1) should be 5") }
  
  // 测试负一
  let result8 = @azimuth.add(-1, 1)
  if result8 != 0 { @builtin.abort("add(-1, 1) should be 0") }
  
  let result9 = @azimuth.multiply(-1, 5)
  if result9 != -5 { @builtin.abort("multiply(-1, 5) should be -5") }
  
  let result10 = @azimuth.subtract(0, -1)
  if result10 != 1 { @builtin.abort("subtract(0, -1) should be 1") }
}