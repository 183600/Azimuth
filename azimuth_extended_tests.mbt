// Azimuth 项目扩展测试用例
// 使用标准 MoonBit 测试语法，补充核心功能的测试覆盖

// 导入主模块中的函数
use azimuth

test "add_function_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -10
  let y = 20
  let z = -5
  
  let result3 = add(add(x, y), z)
  let result4 = add(x, add(y, z))
  assert_eq(result3, result4)
}

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 123
  let b = 456
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -100
  let y = 250
  
  let result3 = add(x, y)
  let result4 = add(y, x)
  assert_eq(result3, result4)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = multiply(a, add(b, c))
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -3
  let y = 8
  let z = -2
  
  let result3 = multiply(x, add(y, z))
  let result4 = add(multiply(x, y), multiply(x, z))
  assert_eq(result3, result4)
}

test "multiply_function_associative_property" {
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let a = 2
  let b = 3
  let c = 4
  
  let result1 = multiply(multiply(a, b), c)
  let result2 = multiply(a, multiply(b, c))
  assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -2
  let y = 5
  let z = -3
  
  let result3 = multiply(multiply(x, y), z)
  let result4 = multiply(x, multiply(y, z))
  assert_eq(result3, result4)
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律: a * b = b * a
  let a = 7
  let b = 11
  
  let result1 = multiply(a, b)
  let result2 = multiply(b, a)
  assert_eq(result1, result2)
  
  // 测试负数情况
  let x = -6
  let y = 9
  
  let result3 = multiply(x, y)
  let result4 = multiply(y, x)
  assert_eq(result3, result4)
}

test "identity_elements" {
  // 测试单位元
  // 加法单位元: a + 0 = a
  let a = 42
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法单位元: a * 1 = a
  let b = 37
  assert_eq(b, multiply(b, 1))
  assert_eq(b, multiply(1, b))
}

test "zero_multiplication_property" {
  // 测试零乘法性质: a * 0 = 0
  let a = 12345
  assert_eq(0, multiply(a, 0))
  assert_eq(0, multiply(0, a))
  
  // 测试负数乘以零
  let b = -6789
  assert_eq(0, multiply(b, 0))
  assert_eq(0, multiply(0, b))
}

test "negative_number_properties" {
  // 测试负数的性质
  let a = 10
  let b = 5
  
  // 负数加法: a + (-b) = a - b
  assert_eq(5, add(a, -b))
  
  // 负数乘法: a * (-b) = -(a * b)
  assert_eq(-50, multiply(a, -b))
  
  // 负负得正: (-a) * (-b) = a * b
  assert_eq(50, multiply(-a, -b))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  
  // 圆形面积计算: π * r² (使用近似值)
  let radius = 5
  let pi_approx = 3
  let area = multiply(pi_approx, multiply(radius, radius))
  assert_eq(75, area)
  
  // 平均值计算: (a + b + c) / 3
  let scores = [80, 90, 100]
  let sum = add(add(scores[0], scores[1]), scores[2])
  let average = sum / 3
  assert_eq(90, average)
  
  // 温度转换: 摄氏度转华氏度 F = C * 9/5 + 32
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 9) / 5, 32)
  assert_eq(68, fahrenheit)
}

test "edge_case_large_numbers" {
  // 测试大数边界情况
  let large_num = 1000000
  let another_large = 2000000
  
  // 大数加法
  let sum_result = add(large_num, another_large)
  assert_eq(3000000, sum_result)
  
  // 大数乘法（小心溢出）
  let multiply_result = multiply(large_num, 2)
  assert_eq(2000000, multiply_result)
  
  // 大数与0的运算
  assert_eq(large_num, add(large_num, 0))
  assert_eq(0, multiply(large_num, 0))
}