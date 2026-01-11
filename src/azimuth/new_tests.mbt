// 新增测试用例 - 补充尚未覆盖的测试场景

///|
test "associative_property_add" {
  // 测试加法结合律
  let a = 100
  let b = 200
  let c = 300
  
  // (a + b) + c = a + (b + c)
  let left_assoc = add(add(a, b), c)
  let right_assoc = add(a, add(b, c))
  assert_eq(left_assoc, right_assoc)
}

///|
test "associative_property_multiply" {
  // 测试乘法结合律
  let a = 10
  let b = 20
  let c = 30
  
  // (a * b) * c = a * (b * c)
  let left_assoc = multiply(multiply(a, b), c)
  let right_assoc = multiply(a, multiply(b, c))
  assert_eq(left_assoc, right_assoc)
}

///|
test "distributive_property" {
  // 测试分配律
  let a = 5
  let b = 10
  let c = 20
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

///|
test "power_of_two_operations" {
  // 测试2的幂次运算
  let two_to_10 = 1024
  let two_to_15 = 32768
  let two_to_20 = 1048576
  
  // 测试2的幂次相加
  assert_eq(2048, add(two_to_10, two_to_10))
  assert_eq(65536, add(two_to_15, two_to_15))
  
  // 测试2的幂次相乘
  assert_eq(33554432, multiply(two_to_15, two_to_10))
  assert_eq(1073741824, multiply(two_to_20, two_to_10)) // 1048576 * 1024 = 1073741824
}

///|
test "sequential_overflow_handling" {
  // 测试连续溢出处理
  let mut value = 1000000
  
  // 连续加法直到溢出
  value = add(value, 1000000)  // 2000000
  value = add(value, 1000000)  // 3000000
  value = add(value, 1000000)  // 4000000
  value = add(value, 1800000000)  // 1804000000
  value = add(value, 343483647)  // 2147483647 (达到最大值)
  value = add(value, 1)  // 应该保持最大值
  assert_eq(2147483647, value)
}

///|
test "string_unicode_handling" {
  // 测试Unicode字符串处理
  assert_eq("Hello, 🌙!", greet("🌙"))
  assert_eq("Hello, 🚀!", greet("🚀"))
  assert_eq("Hello, émojis!", greet("émojis"))
  assert_eq("Hello, 👨‍👩‍👧‍👦!", greet("👨‍👩‍👧‍👦"))
  assert_eq("Hello, 𝔘𝔫𝔦𝔠𝔬𝔡𝔢!", greet("𝔘𝔫𝔦𝔠𝔬𝔡𝔢"))
}

///|
test "negative_zero_handling" {
  // 测试负零处理（虽然Int没有负零，但测试相关边界）
  let zero = 0
  let neg_zero = -0
  
  assert_eq(zero, neg_zero)
  assert_eq(5, add(zero, 5))
  assert_eq(5, add(neg_zero, 5))
  assert_eq(0, multiply(zero, 5))
  assert_eq(0, multiply(neg_zero, 5))
}

///|
test "alternating_operations" {
  // 测试交替加减乘除运算
  let mut result = 100
  
  // 交替运算：+ * - *
  result = add(result, 50)     // 150
  result = multiply(result, 2) // 300
  result = add(result, -100)   // 200
  result = multiply(result, 3) // 600
  
  assert_eq(600, result)
}

///|
test "nested_operations" {
  // 测试嵌套运算
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  // 复杂嵌套：(a + b) * (c + d)
  let result1 = multiply(add(a, b), add(c, d))
  
  // 另一种嵌套：a * c + b * d
  let result2 = add(multiply(a, c), multiply(b, d))
  
  // 测试特定嵌套模式
  assert_eq(2100, result1)  // (10+20)*(30+40) = 30*70 = 2100
  assert_eq(1100, result2)  // 10*30 + 20*40 = 300 + 800 = 1100
}

///|
test "boundary_sequence_tests" {
  // 测试边界值序列
  let max_val = 2147483647
  
  // 从最大值递减到最小值的序列测试
  let mut value = max_val
  value = add(value, -1)  // 2147483646
  value = add(value, -1)  // 2147483645
  value = add(value, -100)  // 2147483545
  value = add(value, -1000000)  // 2147482545
  value = add(value, -2000000000)  // 147482545
  value = add(value, -147482546)  // -1
  value = add(value, -1)  // -2
  value = add(value, -2147483646)  // 应该达到最小值
  assert_eq(-2147483648, value)
}