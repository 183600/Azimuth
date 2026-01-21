// 新的综合测试用例
// 使用标准 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律
  inspect(add(5, 3), "8")?
  inspect(add(3, 5), "8")?
  inspect(add(-2, 7), "5")?
  inspect(add(7, -2), "5")?
  inspect(add(-4, -6), "-10")?
  inspect(add(-6, -4), "-10")?
}

test "add_function_associative_property" {
  // 测试加法结合律
  inspect(add(add(2, 3), 4), "9")?
  inspect(add(2, add(3, 4)), "9")?
  inspect(add(add(-2, 3), 5), "6")?
  inspect(add(-2, add(3, 5)), "6")?
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律
  inspect(multiply(4, 5), "20")?
  inspect(multiply(5, 4), "20")?
  inspect(multiply(-3, 6), "-18")?
  inspect(multiply(6, -3), "-18")?
  inspect(multiply(-2, -7), "14")?
  inspect(multiply(-7, -2), "14")?
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  inspect(multiply(3, add(2, 4)), "18")?
  inspect(add(multiply(3, 2), multiply(3, 4)), "18")?
  inspect(multiply(-2, add(5, 3)), "-16")?
  inspect(add(multiply(-2, 5), multiply(-2, 3)), "-16")?
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  inspect(divide_with_ceil(10, 3), "4")?
  inspect(divide_with_ceil(11, 3), "4")?
  inspect(divide_with_ceil(12, 3), "4")?
  inspect(divide_with_ceil(13, 3), "5")?
  inspect(divide_with_ceil(-10, 3), "-3")?
  inspect(divide_with_ceil(-11, 3), "-3")?
  inspect(divide_with_ceil(-12, 3), "-4")?
}

test "greet_function_unicode_and_special_chars" {
  // 测试 greet 函数的 Unicode 和特殊字符处理
  inspect(greet("世界"), "Hello, 世界!")?
  inspect(greet("🚀"), "Hello, 🚀!")?
  inspect(greet("naïve"), "Hello, naïve!")?
  inspect(greet("café"), "Hello, café!")?
  inspect(greet("123!@#"), "Hello, 123!@#!")?
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  inspect(add(multiply(2, 3), multiply(4, 5)), "26")?
  inspect(multiply(add(2, 3), add(4, 5)), "45")?
  inspect(divide_with_ceil(multiply(6, 7), 4), "11")?
  inspect(add(divide_with_ceil(15, 4), multiply(3, 2)), "11")?
}

test "boundary_value_comprehensive_tests" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  inspect(add(max_val, 0), "2147483647")?
  inspect(add(min_val, 0), "-2147483648")?
  inspect(add(0, max_val), "2147483647")?
  inspect(add(0, min_val), "-2147483648")?
  
  // 边界值乘法
  inspect(multiply(0, max_val), "0")?
  inspect(multiply(0, min_val), "0")?
  inspect(multiply(1, max_val), "2147483647")?
  inspect(multiply(1, min_val), "-2147483648")?
}

test "mathematical_identities" {
  // 测试数学恒等式
  // 加法单位元
  inspect(add(5, 0), "5")?
  inspect(add(-3, 0), "-3")?
  inspect(add(0, 7), "7")?
  inspect(add(0, -2), "-2")?
  
  // 乘法单位元
  inspect(multiply(5, 1), "5")?
  inspect(multiply(-3, 1), "-3")?
  inspect(multiply(1, 7), "7")?
  inspect(multiply(1, -2), "-2")?
  
  // 乘法零元
  inspect(multiply(5, 0), "0")?
  inspect(multiply(-3, 0), "0")?
  inspect(multiply(0, 7), "0")?
  inspect(multiply(0, -2), "0")?
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算商品总价（含税）
  let price1 = 199
  let price2 = 299
  let tax_rate = 2
  let subtotal = add(price1, price2)
  let total = multiply(subtotal, tax_rate)
  inspect(total, "996")?
  
  // 场景2：计算平均分（向上取整）
  let score1 = 88
  let score2 = 92
  let score3 = 85
  let total_score = add(add(score1, score2), score3)
  let average = divide_with_ceil(total_score, 3)
  inspect(average, "89")?
  
  // 场景3：生成用户欢迎消息
  let username = "用户"
  let welcome = greet(username)
  inspect(welcome, "Hello, 用户!")?
}