// 综合测试用例 - 补充现有测试
// 测试 azimuth 包中的核心功能的更多场景

// 测试加法的交换律
test "add_commutative_property" {
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a)(b), @azimuth.add(b)(a))
  
  let neg_a = -12345
  let neg_b = -67890
  @azimuth.assert_eq(@azimuth.add(neg_a)(neg_b), @azimuth.add(neg_b)(neg_a))
}

// 测试加法的结合律
test "add_associative_property" {
  let a = 100
  let b = 200
  let c = 300
  let result1 = @azimuth.add(@azimuth.add(a)(b))(c)
  let result2 = @azimuth.add(a)(@azimuth.add(b)(c))
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法的交换律
test "multiply_commutative_property" {
  let a = 12
  let b = 34
  @azimuth.assert_eq(@azimuth.multiply(a)(b), @azimuth.multiply(b)(a))
  
  let neg_a = -12
  let neg_b = -34
  @azimuth.assert_eq(@azimuth.multiply(neg_a)(neg_b), @azimuth.multiply(neg_b)(neg_a))
}

// 测试乘法的结合律
test "multiply_associative_property" {
  let a = 2
  let b = 3
  let c = 4
  let result1 = @azimuth.multiply(@azimuth.multiply(a)(b))(c)
  let result2 = @azimuth.multiply(a)(@azimuth.multiply(b)(c))
  @azimuth.assert_eq(result1, result2)
}

// 测试乘法对加法的分配律
test "multiply_distributive_property" {
  let a = 5
  let b = 7
  let c = 9
  let left = @azimuth.multiply(a)(@azimuth.add(b)(c))
  let right = @azimuth.add(@azimuth.multiply(a)(b))(@azimuth.multiply(a)(c))
  @azimuth.assert_eq(left, right)
}

// 测试大数运算的稳定性
test "large_number_operations" {
  let large1 = 1000000
  let large2 = 2000000
  let large3 = 3000000
  
  // 大数加法
  let sum1 = @azimuth.add(large1)(large2)
  let sum2 = @azimuth.add(sum1)(large3)
  @azimuth.assert_eq(6000000, sum2)
  
  // 大数乘法
  let product1 = @azimuth.multiply(large1)(2)
  let product2 = @azimuth.multiply(large2)(3)
  @azimuth.assert_eq(2000000, product1)
  @azimuth.assert_eq(6000000, product2)
}

// 测试混合运算场景
test "mixed_operations" {
  let base = 10
  
  // (base + 5) * 2
  let step1 = @azimuth.add(base)(5)
  let result1 = @azimuth.multiply(step1)(2)
  @azimuth.assert_eq(30, result1)
  
  // (base * 3) + 7
  let step2 = @azimuth.multiply(base)(3)
  let result2 = @azimuth.add(step2)(7)
  @azimuth.assert_eq(37, result2)
}

// 测试边界值的组合运算
test "boundary_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // max_val + (-max_val) 应该等于 0
  let result1 = @azimuth.add(max_val)(-max_val)
  @azimuth.assert_eq(0, result1)
  
  // min_val + 1 应该触发溢出保护
  let result2 = @azimuth.add(min_val)(1)
  @azimuth.assert_eq(min_val, result2)
  
  // max_val * 1 应该等于 max_val
  let result3 = @azimuth.multiply(max_val)(1)
  @azimuth.assert_eq(max_val, result3)
  
  // min_val * 1 应该等于 min_val
  let result4 = @azimuth.multiply(min_val)(1)
  @azimuth.assert_eq(min_val, result4)
}

// 测试字符串处理的更多场景
test "greet_extended_scenarios" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, Hello & World!", @azimuth.greet("Hello & World"))
  
  // 测试长字符串
  let long_name = "This is a very long name that should still work properly with the greet function"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
}

// 测试断言函数的功能
test "assertion_functions" {
  // 测试 assert_true 和 assert_false
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试 assert_eq 和 assert_eq_string
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 5
  let y = 10
  @azimuth.assert_true(@azimuth.add(x)(y) == 15)
  @azimuth.assert_false(@azimuth.multiply(x)(y) == 40)
}