// 新增的 MoonBit 测试用例
// 覆盖 add、multiply 和 greet 函数的各种场景

// 测试 add 函数的边界情况
test "add_minimum_value" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, 0)
  @azimuth.assert_eq(min_val, result)
}

// 测试 add 函数的负数溢出保护
test "add_negative_overflow" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 测试 add 函数的正数溢出保护
test "add_positive_overflow" {
  let max_val = 2147483647
  let result = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result)
}

// 测试 multiply 函数的最小值处理
test "multiply_minimum_value" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result)
}

// 测试 multiply 函数的溢出保护
test "multiply_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, max_val)
  @azimuth.assert_eq(max_val, result)
}

// 测试 multiply 函数的最小值乘以-1的情况
test "multiply_minimum_by_negative_one" {
  let min_val = -2147483648
  let result = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

// 测试 greet 函数的特殊字符处理
test "greet_special_characters" {
  let result = @azimuth.greet("张三")
  @azimuth.assert_eq_string("Hello, 张三!", result)
}

// 测试 greet 函数的长字符串处理
test "greet_long_string" {
  let long_name = "这是一个非常长的名字用来测试greet函数处理长字符串的能力"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
}

// 测试断言函数的布尔值处理
test "assert_functions_with_booleans" {
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(1 == 2)
}

// 测试 add 和 multiply 函数的组合使用
test "combined_operations" {
  let a = 10
  let b = 20
  let c = 30
  
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(30, sum)
  
  let product = @azimuth.multiply(sum, 1)
  @azimuth.assert_eq(30, product)
  
  let final_result = @azimuth.add(product, c)
  @azimuth.assert_eq(60, final_result)
}