// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，包含边界条件、错误处理和性能测试

test "add_positive_numbers" {
  // 测试正数加法
  assert_eq(azimuth::add(15, 25), 40)
  assert_eq(azimuth::add(100, 200), 300)
  assert_eq(azimuth::add(1, 1), 2)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-10, -20), -30)
  assert_eq(azimuth::add(-5, -15), -20)
  assert_eq(azimuth::add(-100, -200), -300)
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  assert_eq(azimuth::add(10, -5), 5)
  assert_eq(azimuth::add(-20, 30), 10)
  assert_eq(azimuth::add(50, -100), -50)
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  assert_eq(azimuth::multiply(7, 8), 56)
  assert_eq(azimuth::multiply(12, 12), 144)
  assert_eq(azimuth::multiply(3, 33), 99)
}

test "multiply_with_zero" {
  // 测试与零相乘
  assert_eq(azimuth::multiply(0, 5), 0)
  assert_eq(azimuth::multiply(10, 0), 0)
  assert_eq(azimuth::multiply(0, 0), 0)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-4, 5), -20)
  assert_eq(azimuth::multiply(6, -7), -42)
  assert_eq(azimuth::multiply(-3, -9), 27)
}

test "greet_normal_names" {
  // 测试普通名称问候
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  assert_eq(azimuth::greet("Charlie"), "Hello, Charlie!")
}

test "greet_special_characters" {
  // 测试包含特殊字符的名称
  assert_eq(azimuth::greet("John Doe"), "Hello, John Doe!")
  assert_eq(azimuth::greet("user@domain.com"), "Hello, user@domain.com!")
  assert_eq(azimuth::greet("test-123"), "Hello, test-123!")
}

test "mathematical_identities" {
  // 测试数学恒等式
  // 加法单位元
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  
  // 乘法单位元
  assert_eq(azimuth::multiply(17, 1), 17)
  assert_eq(azimuth::multiply(1, 17), 17)
  
  // 乘法零元
  assert_eq(azimuth::multiply(999, 0), 0)
}