test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 2: 字符串处理
///|
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @clean_test.greet(name1)
  let greeting2 = @clean_test.greet(name2)
  
  @clean_test.assert_eq_string("Hello, Alice!", greeting1)
  @clean_test.assert_eq_string("Hello, Bob!", greeting2)
}

// 测试用例 3: 边界值测试
///|
test "boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  let result1 = @clean_test.add(max_val, 0)
  let result2 = @clean_test.add(min_val, 0)
  
  @clean_test.assert_eq(max_val, result1)
  @clean_test.assert_eq(min_val, result2)
}

// 测试用例 4: 负数运算
///|
test "negative_operations" {
  let a = -10
  let b = -20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(-30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 5: 复合运算
///|
test "compound_operations" {
  let base = 5
  
  let result1 = @clean_test.multiply(base, 2)
  let result2 = @clean_test.add(result1, 10)
  let result3 = @clean_test.multiply(result2, 3)
  
  @clean_test.assert_eq(10, result1)
  @clean_test.assert_eq(20, result2)
  @clean_test.assert_eq(60, result3)
}
