// Azimuth 项目新增单元测试用例
// 使用标准 MoonBit 测试语法，覆盖更多边界条件和实际应用场景

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的情况
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(17, @azimuth.multiply(-17, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -1))
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(10, @azimuth.multiply(5, 2))
  @azimuth.assert_eq(28, @azimuth.multiply(7, 4))
  @azimuth.assert_eq(24, @azimuth.multiply(3, 8))
  @azimuth.assert_eq(176, @azimuth.multiply(11, 16))
}

test "greet_multiline_names" {
  // 测试多行名称问候
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
  @azimuth.assert_eq_string("Hello, First\nSecond\nThird!", @azimuth.greet("First\nSecond\nThird"))
}

test "greet_with_quotes" {
  // 测试包含引号的名称
  @azimuth.assert_eq_string("Hello, John \"The Rock\" Doe!", @azimuth.greet("John \"The Rock\" Doe"))
  @azimuth.assert_eq_string("Hello, 'Single Quote'!", @azimuth.greet("'Single Quote'"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列
  // 斐波那契数列前几项: 1, 1, 2, 3, 5, 8, 13, 21
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "calculate_perimeter" {
  // 测试计算矩形周长
  let length = 10
  let width = 6
  // 周长 = 2 * (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(32, perimeter)
  
  let length2 = 15
  let width2 = 8
  let perimeter2 = @azimuth.multiply(2, @azimuth.add(length2, width2))
  @azimuth.assert_eq(46, perimeter2)
}

test "calculate_average" {
  // 测试计算平均值
  let num1 = 80
  let num2 = 90
  let num3 = 100
  // 平均值 = (num1 + num2 + num3) / 3
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let _average = sum / 3  // 简化版本，实际项目中可能需要除法函数
  @azimuth.assert_eq(270, sum)
}

test "nested_calculations" {
  // 测试嵌套计算
  // 计算: (a * b) + (c * d) - (e * f)
  let a = 3; let b = 4
  let c = 5; let d = 6
  let e = 2; let f = 7
  
  let term1 = @azimuth.multiply(a, b)  // 12
  let term2 = @azimuth.multiply(c, d)  // 30
  let term3 = @azimuth.multiply(e, f)  // 14
  
  let result = @azimuth.add(@azimuth.add(term1, term2), @azimuth.add(0, -term3))
  @azimuth.assert_eq(28, result)
}