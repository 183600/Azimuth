// 额外的 MoonBit 测试用例
// 这些测试用例扩展了对 azimuth 库功能的验证

test "add_edge_case_max_min_values" {
  // 测试最大值和最小值相加的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "add_consecutive_operations" {
  // 测试连续加法操作
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result)
  
  let result2 = @azimuth.add(@azimuth.add(-5, 10), @azimuth.add(-3, 8))
  @azimuth.assert_eq(10, result2)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(-1024, @azimuth.multiply(-32, 32))
}

test "multiply_fractional_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算百分比）
  // 25% of 400 = 400 * 25 / 100
  let value = 400
  let percentage = 25
  let result = @azimuth.multiply(value, percentage) / 100
  @azimuth.assert_eq(100, result)
  
  // 75% of 200 = 200 * 75 / 100
  let value2 = 200
  let percentage2 = 75
  let result2 = @azimuth.multiply(value2, percentage2) / 100
  @azimuth.assert_eq(150, result2)
}

test "greet_multilingual" {
  // 测试多语言问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
}

test "greet_with_newlines_and_tabs" {
  // 测试包含换行符和制表符的字符串
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
  @azimuth.assert_eq_string("Hello, Tab\tSeparated!", @azimuth.greet("Tab\tSeparated"))
  @azimuth.assert_eq_string("Hello, Mixed\n\tContent!", @azimuth.greet("Mixed\n\tContent"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  // 计算：1 + 2*3 + 4*5*6
  let term1 = 1
  let term2 = @azimuth.multiply(2, 3)
  let term3 = @azimuth.multiply(@azimuth.multiply(4, 5), 6)
  let result = @azimuth.add(@azimuth.add(term1, term2), term3)
  @azimuth.assert_eq(127, result)  // 1 + 6 + 120 = 127
}

test "geometric_calculations" {
  // 测试几何计算：正方形面积和周长
  let side = 5
  let area = @azimuth.multiply(side, side)
  let perimeter = @azimuth.multiply(4, side)
  @azimuth.assert_eq(25, area)
  @azimuth.assert_eq(20, perimeter)
  
  // 测试长方形面积和周长
  let length = 6
  let width = 4
  let rect_area = @azimuth.multiply(length, width)
  let rect_perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(24, rect_area)
  @azimuth.assert_eq(20, rect_perimeter)
}

test "factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let fact1 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, fact1)
  
  // 计算 4! = 4 * 3 * 2 * 1 = 24
  let fact2 = @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))
  @azimuth.assert_eq(24, fact2)
  
  // 计算 3! = 3 * 2 * 1 = 6
  let fact3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  @azimuth.assert_eq(6, fact3)
}

test "fibonacci_sequence" {
  // 斐波那契数列：F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  let f9 = @azimuth.add(f8, f7)  // 34
  let f10 = @azimuth.add(f9, f8) // 55
  
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  @azimuth.assert_eq(34, f9)
  @azimuth.assert_eq(55, f10)
}