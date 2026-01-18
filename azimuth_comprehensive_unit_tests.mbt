// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，覆盖主要功能模块

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
  assert_eq(100, add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  assert_eq(1, add(1, 0))
  assert_eq(-5, add(-10, 5))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(0, multiply(0, -100))
}

test "greet_function_basic_cases" {
  // 测试 greet 函数的基本情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(" "))
  assert_eq_string("Hello, A!", greet("A"))
}

test "combined_operations" {
  // 测试组合操作
  let a = 10
  let b = 20
  let c = 5
  
  // 复合计算
  let result1 = add(multiply(a, b), c)
  let result2 = multiply(add(a, c), b)
  let result3 = add(multiply(a, c), multiply(b, c))
  
  assert_eq(205, result1)
  assert_eq(300, result2)
  assert_eq(150, result3)
}

test "business_logic_scenarios" {
  // 测试商业逻辑场景
  let unit_price = 100
  let quantity = 3
  let discount = 50
  let tax_rate = 10
  
  // 计算总价
  let subtotal = multiply(unit_price, quantity)
  let discounted_total = add(subtotal, -discount)
  let tax = multiply(discounted_total, tax_rate) / 100
  let final_total = add(discounted_total, tax)
  
  assert_eq(300, subtotal)
  assert_eq(250, discounted_total)
  assert_eq(25, tax)
  assert_eq(275, final_total)
}

test "mathematical_series" {
  // 测试数学级数
  let n = 5
  
  // 计算前n个自然数的和
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  
  // 计算前n个偶数的和
  let sum2 = add(add(add(add(2, 4), 6), 8), 10)
  
  // 计算前n个奇数的和
  let sum3 = add(add(add(add(1, 3), 5), 7), 9)
  
  assert_eq(15, sum1)
  assert_eq(30, sum2)
  assert_eq(25, sum3)
}

test "string_and_number_combinations" {
  // 测试字符串和数字的组合操作
  let name = "Azimuth"
  let age = 25
  let score = 95
  
  // 生成问候语并进行数值计算
  let greeting = greet(name)
  let age_times_score = multiply(age, score)
  let age_plus_score = add(age, score)
  
  assert_eq_string("Hello, Azimuth!", greeting)
  assert_eq(2375, age_times_score)
  assert_eq(120, age_plus_score)
}