// 简单的新测试用例 - 演示标准的 MoonBit 测试语法

// 测试用例 1: 斐波那契数列计算
///|
test "fibonacci_calculation" {
  let fib0 = 0
  let fib1 = 1
  let fib2 = add(fib0, fib1)  // 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  assert_eq(1, fib2)
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
}

// 测试用例 2: 阶乘计算
///|
test "factorial_calculation" {
  let fact1 = multiply(1, 1)  // 1! = 1
  let fact2 = multiply(2, fact1)  // 2! = 2
  let fact3 = multiply(3, fact2)  // 3! = 6
  let fact4 = multiply(4, fact3)  // 4! = 24
  
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
}

// 测试用例 3: 幂运算
///|
test "power_calculation" {
  // 2^3 = 8
  let power2_3 = multiply(multiply(2, 2), 2)
  assert_eq(8, power2_3)
  
  // 3^2 = 9
  let power3_2 = multiply(3, 3)
  assert_eq(9, power3_2)
}

// 测试用例 4: 线性方程求解
///|
test "linear_equation" {
  // 验证 3x + 7 = 22，解得 x = 5
  let a = 3
  let b = 7
  let c = 22
  let x = 5
  
  let ax = multiply(a, x)
  let ax_plus_b = add(ax, b)
  
  assert_eq(c, ax_plus_b)
}

// 测试用例 5: 字符串处理
///|
test "string_processing" {
  let greeting1 = greet("MoonBit")
  let greeting2 = greet("测试")
  let greeting3 = greet("")
  
  assert_eq("Hello, MoonBit!", greeting1)
  assert_eq("Hello, 测试!", greeting2)
  assert_eq("Hello, !", greeting3)
}

// 测试用例 6: 数组平均值计算（模拟）
///|
test "array_average" {
  // 模拟数组：[10, 20, 30, 40, 50]
  let sum = add(add(add(add(10, 20), 30), 40), 50)
  let count = 5
  let expected_average = 30
  
  // 验证：平均值 * 数量 = 总和
  let average_times_count = multiply(expected_average, count)
  
  assert_eq(sum, average_times_count)  // 150 = 30 * 5
}

// 测试用例 7: 二进制转换
///|
test "binary_conversion" {
  // 二进制 1011 = 1*2^3 + 0*2^2 + 1*2^1 + 1*2^0 = 11
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1
  
  let value3 = multiply(bit3, multiply(multiply(2, 2), 2))  // 1 * 8
  let value2 = multiply(bit2, multiply(2, 2))  // 0 * 4
  let value1 = multiply(bit1, 2)  // 1 * 2
  let value0 = multiply(bit0, 1)  // 1 * 1
  
  let decimal_value = add(add(add(value3, value2), value1), value0)
  
  assert_eq(11, decimal_value)
}

// 测试用例 8: 百分比计算
///|
test "percentage_calculation" {
  let whole = 200
  let part = 50
  let expected_percent = 25  // 50/200 * 100 = 25
  
  // 验证：部分 * 100 = 百分比 * 整体
  let part_times_100 = multiply(part, 100)
  let verification = multiply(expected_percent, whole)
  
  assert_eq(part_times_100, verification)  // 5000 = 25 * 200
}

// 测试用例 9: 温度转换
///|
test "temperature_conversion" {
  // 摄氏度到华氏度：F = C * 9/5 + 32
  // 测试0°C = 32°F
  let celsius = 0
  let fahrenheit_times_5 = add(multiply(celsius, 9), 160)  // (0 * 9 + 160) = 160, 160/5 = 32
  
  assert_eq(160, fahrenheit_times_5)
}

// 测试用例 10: 最大公约数（模拟）
///|
test "gcd_simulation" {
  // gcd(48, 18) = 6
  let a = 48
  let b = 18
  
  // 48 mod 18 = 12 (通过减法模拟)
  let remainder1 = add(a, multiply(-2, b))  // 48 - 2*18 = 12
  assert_eq(12, remainder1)
  
  // 18 mod 12 = 6
  let remainder2 = add(b, multiply(-1, remainder1))  // 18 - 1*12 = 6
  assert_eq(6, remainder2)
  
  // gcd是最后一个非零余数
  let gcd = remainder2
  assert_eq(6, gcd)
}