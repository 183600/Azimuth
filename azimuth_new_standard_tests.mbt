// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(8, azimuth.add(3, 5))?
  @assertion.assert_eq(15, azimuth.multiply(3, 5))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 5))?
}

test "edge_case_calculations" {
  // 边界情况计算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值和最小值的运算
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "string_processing_multilingual" {
  // 多语言字符串处理测试
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))?
  @assertion.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))?
  @assertion.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 验证交换律
  @assertion.assert_eq(azimuth.add(a, b), azimuth.add(b, a))?
  @assertion.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))?
  
  // 验证结合律
  let c = 5
  @assertion.assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))?
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 6
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(32, perimeter)?
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 200
  let discount_rate = 15  // 15%
  let discount_amount = azimuth.multiply(original_price, discount_rate) / 100
  let final_price = azimuth.add(original_price, -discount_amount)
  @assertion.assert_eq(170, final_price)?
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  // 计算：(a + b) × (c - d) + e
  let a = 15
  let b = 10
  let c = 20
  let d = 8
  let e = 5
  
  let sum_ab = azimuth.add(a, b)
  let diff_cd = azimuth.add(c, -d)
  let product = azimuth.multiply(sum_ab, diff_cd)
  let result = azimuth.add(product, e)
  
  @assertion.assert_eq(165, result)?
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与普通值的运算
  @assertion.assert_eq(2147483648, azimuth.add(max_val, 1))?
  @assertion.assert_eq(2147483647, azimuth.multiply(max_val, 1))?
  
  // 测试最小值与普通值的运算
  @assertion.assert_eq(-2147483647, azimuth.add(min_val, 1))?
  @assertion.assert_eq(-2147483648, azimuth.multiply(min_val, 1))?
}

test "performance_calculations" {
  // 性能相关计算测试
  // 计算等差数列和：1 + 2 + ... + n = n × (n + 1) / 2
  let n = 100
  let series_sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(5050, series_sum)?
  
  // 计算等比数列和：2^0 + 2^1 + ... + 2^6 = 2^7 - 1 = 127
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 2), 4), 8), 16), 32), 64)
  @assertion.assert_eq(127, geo_sum)?
}

test "error_handling_validation" {
  // 错误处理验证测试
  // 验证断言函数正常工作
  @assertion.assert_true(true)?
  @assertion.assert_false(false)?
  @assertion.assert_eq(42, 42)?
  @assertion.assert_eq_string("test", "test")?
  
  // 验证溢出保护
  let max_val = 2147483647
  @assertion.assert_true(azimuth.add(max_val, 100) == max_val)?
  @assertion.assert_true(azimuth.multiply(max_val, 2) == max_val)?
}

test "data_conversion_scenarios" {
  // 数据转换场景测试
  // 模拟数值到字符串的转换
  let year = 2024
  let year_string = "2024"
  @assertion.assert_eq_string("Hello, 2024!", azimuth.greet(year_string))?
  
  // 模拟布尔值到字符串的转换
  @assertion.assert_eq_string("Hello, true!", azimuth.greet("true"))?
  @assertion.assert_eq_string("Hello, false!", azimuth.greet("false"))?
  
  // 模拟特殊字符处理
  let special_chars = "@#$%^&*()"
  @assertion.assert_eq_string("Hello, @#$%^&*()!", azimuth.greet(special_chars))?
}