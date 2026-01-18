// 额外的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  
  // 测试两个负数
  let e = -12345
  let f = -67890
  
  @azimuth.assert_eq(@azimuth.add(e, f), @azimuth.add(f, e))
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -150
  let e = 250
  let f = -100
  
  let result3 = @azimuth.add(@azimuth.add(d, e), f)
  let result4 = @azimuth.add(d, @azimuth.add(e, f))
  
  @azimuth.assert_eq(result3, result4)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let d = -3
  let e = 8
  let f = -2
  
  let left_side2 = @azimuth.multiply(d, @azimuth.add(e, f))
  let right_side2 = @azimuth.add(@azimuth.multiply(d, e), @azimuth.multiply(d, f))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "multiply_identity_property" {
  // 测试乘法单位元：a × 1 = a
  let a = 123456
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  
  // 测试负数情况
  let b = -123456
  @azimuth.assert_eq(b, @azimuth.multiply(b, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  
  // 测试零的情况
  let c = 0
  @azimuth.assert_eq(c, @azimuth.multiply(c, 1))
  @azimuth.assert_eq(c, @azimuth.multiply(1, c))
}

test "multiply_zero_property" {
  // 测试乘法零元：a × 0 = 0
  let a = 123456
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
  
  // 测试负数情况
  let b = -123456
  @azimuth.assert_eq(0, @azimuth.multiply(b, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
  
  // 测试最大值和最小值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "greet_unicode_handling" {
  // 测试 greet 函数的 Unicode 处理能力
  // 测试中文字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  
  // 测试日文字符
  @azimuth.assert_eq_string("Hello, こんにちは!", @azimuth.greet("こんにちは"))
  
  // 测试韩文字符
  @azimuth.assert_eq_string("Hello, 안녕하세요!", @azimuth.greet("안녕하세요"))
  
  // 测试阿拉伯文字符
  @azimuth.assert_eq_string("Hello, مرحبا!", @azimuth.greet("مرحبا"))
  
  // 测试表情符号
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
  @azimuth.assert_eq_string("Hello, 👍!", @azimuth.greet("👍"))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  // 计算贷款月供：P × [r(1+r)^n] / [(1+r)^n-1]
  // 简化为整数计算
  
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  
  // 简化计算：月供 ≈ 本金 / 月数 + 利息
  let months = @azimuth.multiply(years, 12)
  let monthly_principal = principal / months
  
  // 简化的利息计算
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  let monthly_interest = total_interest / months
  
  let monthly_payment = @azimuth.add(monthly_principal, monthly_interest)
  
  // 验证月供在合理范围内
  @azimuth.assert_true(monthly_payment > 500)
  @azimuth.assert_true(monthly_payment < 1000)
}

test "scientific_calculation" {
  // 测试科学计算场景
  // 计算自由落体距离：s = 0.5 × g × t²
  // 简化为整数计算：s ≈ 5 × t² (g ≈ 10 m/s²)
  
  let time1 = 1  // 1秒
  let distance1 = @azimuth.multiply(5, @azimuth.multiply(time1, time1))
  @azimuth.assert_eq(5, distance1)
  
  let time2 = 2  // 2秒
  let distance2 = @azimuth.multiply(5, @azimuth.multiply(time2, time2))
  @azimuth.assert_eq(20, distance2)
  
  let time3 = 3  // 3秒
  let distance3 = @azimuth.multiply(5, @azimuth.multiply(time3, time3))
  @azimuth.assert_eq(45, distance3)
  
  // 计算速度：v = g × t
  let velocity1 = @azimuth.multiply(10, time1)  // 10 m/s
  let velocity2 = @azimuth.multiply(10, time2)  // 20 m/s
  let velocity3 = @azimuth.multiply(10, time3)  // 30 m/s
  
  @azimuth.assert_eq(10, velocity1)
  @azimuth.assert_eq(20, velocity2)
  @azimuth.assert_eq(30, velocity3)
}

test "string_length_validation" {
  // 测试字符串长度验证
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试短字符串
  @azimuth.assert_eq_string("Hello, Hi!", @azimuth.greet("Hi"))
  
  // 测试中等长度字符串
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  Space Test !", @azimuth.greet(" Space Test "))
}

test "extreme_boundary_combinations" {
  // 测试极端边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试连续边界运算
  let chain1 = @azimuth.add(@azimuth.multiply(max_val, 0), max_val)
  @azimuth.assert_eq(max_val, chain1)
  
  let chain2 = @azimuth.add(@azimuth.multiply(min_val, 0), min_val)
  @azimuth.assert_eq(min_val, chain2)
  
  // 测试复杂边界表达式
  let complex1 = @azimuth.add(@azimuth.multiply(@azimuth.add(max_val, min_val), 1), max_val)
  @azimuth.assert_eq(max_val, complex1)
  
  let complex2 = @azimuth.multiply(@azimuth.add(max_val, 1), 0)
  @azimuth.assert_eq(0, complex2)
}