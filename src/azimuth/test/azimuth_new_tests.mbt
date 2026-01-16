// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，覆盖多样化的测试场景

test "add_zero_identity" {
  // 测试加法零元特性
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(5, 0), 5)
  assert_eq(azimuth::add(0, -3), -3)
  assert_eq(azimuth::add(-7, 0), -7)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-1, -1), -2)
  assert_eq(azimuth::add(-5, 3), -2)
  assert_eq(azimuth::add(7, -10), -3)
  assert_eq(azimuth::add(-8, -2), -10)
}

test "multiply_zero_property" {
  // 测试乘法零元特性
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(5, 0), 0)
  assert_eq(azimuth::multiply(0, -3), 0)
  assert_eq(azimuth::multiply(-7, 0), 0)
}

test "multiply_identity_property" {
  // 测试乘法单位元特性
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(5, 1), 5)
  assert_eq(azimuth::multiply(1, -3), -3)
  assert_eq(azimuth::multiply(-7, 1), -7)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-1, -1), 1)
  assert_eq(azimuth::multiply(-5, 3), -15)
  assert_eq(azimuth::multiply(7, -10), -70)
  assert_eq(azimuth::multiply(-8, -2), 16)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  assert_eq(azimuth::greet("\n"), "Hello, \n!")
  assert_eq(azimuth::greet("@#$"), "Hello, @#$!")
  assert_eq(azimuth::greet("[]{}"), "Hello, []{}!")
  assert_eq(azimuth::greet("()"), "Hello, ()!")
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq(azimuth::greet("中文"), "Hello, 中文!")
  assert_eq(azimuth::greet("العربية"), "Hello, العربية!")
  assert_eq(azimuth::greet("Русский"), "Hello, Русский!")
  assert_eq(azimuth::greet("Español"), "Hello, Español!")
}

test "arithmetic_combinations" {
  // 测试算术组合运算
  
  // 计算 (a + b) * (c + d)
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  let result = azimuth::multiply(azimuth::add(a, b), azimuth::add(c, d))
  assert_eq(45, result)
  
  // 计算 a * b + c * d + e * f
  let e = 6
  let f = 7
  let result2 = azimuth::add(azimuth::add(azimuth::multiply(a, b), azimuth::multiply(c, d)), azimuth::multiply(e, f))
  assert_eq(68, result2)
}

test "number_theory_examples" {
  // 测试数论例子
  
  // 计算最小公倍数的简单情况: lcm(a,b) = a * b / gcd(a,b)
  // 对于互质的数 3 和 4, gcd = 1, 所以 lcm = 3 * 4 = 12
  let lcm_3_4 = azimuth::multiply(3, 4)
  assert_eq(12, lcm_3_4)
  
  // 对于 5 和 7, gcd = 1, 所以 lcm = 5 * 7 = 35
  let lcm_5_7 = azimuth::multiply(5, 7)
  assert_eq(35, lcm_5_7)
}

test "practical_word_problems" {
  // 测试实际应用问题
  
  // 问题: 一个农民有 12 只鸡，每只鸡每天下 2 个蛋，一周能下多少个蛋？
  let chickens = 12
  let eggs_per_day = azimuth::multiply(chickens, 2)
  let days_in_week = 7
  let total_eggs = azimuth::multiply(eggs_per_day, days_in_week)
  assert_eq(168, total_eggs)
  
  // 问题: 买了 3 本书，每本 25 元，又买了 5 支笔，每支 8 元，总共花了多少钱？
  let books = azimuth::multiply(3, 25)
  let pens = azimuth::multiply(5, 8)
  let total_cost = azimuth::add(books, pens)
  assert_eq(115, total_cost)
}