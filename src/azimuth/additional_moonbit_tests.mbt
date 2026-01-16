// 额外的 MoonBit 测试用例
// 本文件包含对 azimuth 模块的额外测试用例

// 导入主模块
use azimuth

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, azimuth.add(-3, -7))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(-100, azimuth.add(-50, -50))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, azimuth.multiply(-3, -5))
  assert_eq(-15, azimuth.multiply(3, -5))
  assert_eq(0, azimuth.multiply(-10, 0))
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, Mundo!", azimuth.greet("Mundo"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(1000000, azimuth.add(500000, 500000))
  assert_eq(2000000000, azimuth.add(1000000000, 1000000000))
}

test "multiply_by_large_numbers" {
  // 测试大数相乘
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  assert_eq(100000000, azimuth.multiply(10000, 10000))
}

test "add_with_extreme_values" {
  // 测试极值相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "multiply_with_extreme_values" {
  // 测试极值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(0, azimuth.multiply(min_val, 0))
  assert_eq(max_val, azimuth.multiply(max_val, 1))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算：(2 + 3) * (4 + 5) - 6 * 7
  let step1 = azimuth.add(2, 3)  // 5
  let step2 = azimuth.add(4, 5)  // 9
  let step3 = azimuth.multiply(step1, step2)  // 45
  let step4 = azimuth.multiply(6, 7)  // 42
  let result = azimuth.add(step3, -step4)  // 3
  
  assert_eq(3, result)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, A!", azimuth.greet("A"))
  assert_eq_string("Hello,   !", azimuth.greet("  "))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 42
  let b = 17
  
  // 加法恒等式
  assert_eq(azimuth.add(a, 0), a)
  assert_eq(azimuth.add(0, a), a)
  
  // 乘法恒等式
  assert_eq(azimuth.multiply(a, 1), a)
  assert_eq(azimuth.multiply(1, a), a)
  assert_eq(azimuth.multiply(a, 0), 0)
  
  // 分配律
  assert_eq(
    azimuth.multiply(a, azimuth.add(b, 3)),
    azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3))
  )
}