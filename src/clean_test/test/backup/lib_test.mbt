// 测试文件 - 使用clean_test包中的函数
test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = a + b
  clean_test::assert_eq(30, sum)
  
  let product = a * b
  clean_test::assert_eq(200, product)
}

test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = "Hello, " + name1 + "!"
  clean_test::assert_eq_string("Hello, Alice!", greeting1)
  
  let greeting2 = "Hello, " + name2 + "!"
  clean_test::assert_eq_string("Hello, Bob!", greeting2)
}

test "edge_cases" {
  // 测试零值
  let zero = 0
  let positive = 5
  let negative = -5
  
  clean_test::assert_eq(positive, clean_test::add(zero, positive))
  clean_test::assert_eq(positive, clean_test::add(positive, zero))
  clean_test::assert_eq(zero, clean_test::multiply(zero, positive))
  clean_test::assert_eq(zero, clean_test::multiply(positive, zero))
  
  // 使用 negative 变量以避免未使用警告
  clean_test::assert_eq(zero, clean_test::add(negative, positive))
  clean_test::assert_eq(-25, clean_test::multiply(negative, positive))
}

test "boundary_values" {
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试边界值加法
  let result1 = clean_test::add(max_int, 0)
  clean_test::assert_eq(max_int, result1)
  
  let result2 = clean_test::add(min_int, 0)
  clean_test::assert_eq(min_int, result2)
  
  // 测试边界值乘法
  let result3 = clean_test::multiply(max_int, 1)
  clean_test::assert_eq(max_int, result3)
  
  let result4 = clean_test::multiply(min_int, 1)
  clean_test::assert_eq(min_int, result4)
  
  // 使用greet函数以避免未使用警告
  let greet_result = clean_test::greet("Boundary")
  clean_test::assert_eq_string("Hello, Boundary!", greet_result)
}
