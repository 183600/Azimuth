test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 2: 边界值测试
///|
test "boundary_value_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = @clean_test.add(max_val, 0)
  let result2 = @clean_test.add(min_val, 0)
  
  @clean_test.assert_eq(max_val, result1)
  @clean_test.assert_eq(min_val, result2)
}

// 测试用例 3: 负数运算
///|
test "negative_operations" {
  let a = -10
  let b = -20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(-30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 4: 零值测试
///|
test "zero_operations" {
  let a = 100
  let b = 0
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(100, sum)
  @clean_test.assert_eq(0, product)
}

// 测试用例 5: 字符串处理
///|
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @clean_test.greet(name1)
  let greeting2 = @clean_test.greet(name2)
  
  @clean_test.assert_eq_string("Hello, Alice!", greeting1)
  @clean_test.assert_eq_string("Hello, Bob!", greeting2)
}
