// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，涵盖不同测试场景

test "addition_with_various_numbers" {
  // 测试各种数字的加法运算
  assert_eq(azimuth::add(5, 3), 8)
  assert_eq(azimuth::add(-2, 7), 5)
  assert_eq(azimuth::add(0, 10), 10)
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(100, 200), 300)
}

test "multiplication_with_various_numbers" {
  // 测试各种数字的乘法运算
  assert_eq(azimuth::multiply(4, 5), 20)
  assert_eq(azimuth::multiply(-3, 6), -18)
  assert_eq(azimuth::multiply(0, 15), 0)
  assert_eq(azimuth::multiply(-2, -4), 8)
  assert_eq(azimuth::multiply(10, 10), 100)
}

test "greet_function_with_different_inputs" {
  // 测试问候函数的不同输入
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("测试"), "Hello, 测试!")
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 5
  let c = 3
  
  // (a + b) * c
  let result1 = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(result1, 45)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(result2, 53)
  
  // (a - b) * (b + c)
  let result3 = azimuth::multiply(azimuth::add(a, -b), azimuth::add(b, c))
  assert_eq(result3, 40)
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法结合律: (a + b) + c = a + (b + c)
  let a = 7
  let b = 12
  let c = 5
  
  let left = azimuth::add(azimuth::add(a, b), c)
  let right = azimuth::add(a, azimuth::add(b, c))
  assert_eq(left, right)
  
  // 乘法交换律: a * b = b * a
  let x = 8
  let y = 6
  assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
}

test "boundary_value_tests" {
  // 测试边界值
  // 测试大数
  assert_eq(azimuth::add(1000000, 1000000), 2000000)
  assert_eq(azimuth::multiply(10000, 100), 1000000)
  
  // 测试零
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
  
  // 测试负数
  assert_eq(azimuth::add(-100, -200), -300)
  assert_eq(azimuth::multiply(-10, -10), 100)
}

test "sequential_calculations" {
  // 测试连续计算
  let result = 0
  
  // 累加计算: 1 + 2 + 3 + 4 + 5
  result = azimuth::add(result, 1)
  result = azimuth::add(result, 2)
  result = azimuth::add(result, 3)
  result = azimuth::add(result, 4)
  result = azimuth::add(result, 5)
  assert_eq(result, 15)
  
  // 累乘计算: 1 * 2 * 3 * 4
  let product = 1
  product = azimuth::multiply(product, 1)
  product = azimuth::multiply(product, 2)
  product = azimuth::multiply(product, 3)
  product = azimuth::multiply(product, 4)
  assert_eq(product, 24)
}

test "string_operations_with_greet" {
  // 测试使用 greet 函数的字符串操作
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = azimuth::greet(name1)
  let greeting2 = azimuth::greet(name2)
  
  // 测试问候结果
  assert_eq(greeting1, "Hello, Alice!")
  assert_eq(greeting2, "Hello, Bob!")
  
  // 测试长度相同的名字生成的问候长度相同
  assert_eq(length(greeting1), length(greeting2))
}

test "mixed_operations_test" {
  // 测试混合运算
  let base = 10
  
  // 复合运算: ((base * 2) + 5) * 3
  let step1 = azimuth::multiply(base, 2)  // 20
  let step2 = azimuth::add(step1, 5)      // 25
  let result = azimuth::multiply(step2, 3) // 75
  
  assert_eq(result, 75)
  
  // 另一种计算方式: base * 6 + 15
  let alternative = azimuth::add(azimuth::multiply(base, 6), 15)
  assert_eq(result, alternative)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试空字符串问候
  let empty_greeting = azimuth::greet("")
  assert_eq(empty_greeting, "Hello, !")
  
  // 测试单个字符问候
  let single_char_greeting = azimuth::greet("X")
  assert_eq(single_char_greeting, "Hello, X!")
  
  // 测试带空格的名字
  let spaced_name_greeting = azimuth::greet("John Doe")
  assert_eq(spaced_name_greeting, "Hello, John Doe!")
}