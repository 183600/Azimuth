// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，覆盖主要功能模块

test "add_function_basic_cases" {
  // 测试 add 函数的基本情况
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_basic_cases" {
  // 测试 multiply 函数的基本情况
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
}

test "greet_function_basic_cases" {
  // 测试 greet 函数的基本情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "combined_operations" {
  // 测试组合操作
  let a = 10
  let b = 20
  let c = 5
  
  // 复合计算
  let result1 = @azimuth.add(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(@azimuth.add(a, c), b)
  let result3 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  
  @azimuth.assert_eq(205, result1)
  @azimuth.assert_eq(300, result2)
  @azimuth.assert_eq(150, result3)
}

test "business_logic_scenarios" {
  // 测试商业逻辑场景
  let unit_price = 100
  let quantity = 3
  let discount = 50
  let tax_rate = 10
  
  // 计算总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let discounted_total = @azimuth.add(subtotal, -discount)
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  let final_total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(300, subtotal)
  @azimuth.assert_eq(250, discounted_total)
  @azimuth.assert_eq(25, tax)
  @azimuth.assert_eq(275, final_total)
}

test "mathematical_series" {
  // 测试数学级数
  let n = 5
  
  // 计算前n个自然数的和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  
  // 计算前n个偶数的和
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8), 10)
  
  // 计算前n个奇数的和
  let sum3 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  
  @azimuth.assert_eq(15, sum1)
  @azimuth.assert_eq(30, sum2)
  @azimuth.assert_eq(25, sum3)
}

test "string_and_number_combinations" {
  // 测试字符串和数字的组合操作
  let name = "Azimuth"
  let age = 25
  let score = 95
  
  // 生成问候语并进行数值计算
  let greeting = @azimuth.greet(name)
  let age_times_score = @azimuth.multiply(age, score)
  let age_plus_score = @azimuth.add(age, score)
  
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting)
  @azimuth.assert_eq(2375, age_times_score)
  @azimuth.assert_eq(120, age_plus_score)
}