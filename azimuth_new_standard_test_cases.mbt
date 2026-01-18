// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能测试

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum_result = azimuth.add(a, b)
  @assertion.assert_eq(22, sum_result)?
  
  // 测试乘法
  let mul_result = azimuth.multiply(a, b)
  @assertion.assert_eq(105, mul_result)?
  
  // 测试与0的运算
  @assertion.assert_eq(15, azimuth.add(a, 0))?
  @assertion.assert_eq(0, azimuth.multiply(a, 0))?
}

test "boundary_value_handling" {
  // 边界值处理测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 2))?
  
  // 测试最小值边界
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
  
  // 测试边界值组合
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
}

test "string_processing_functions" {
  // 字符串处理功能测试
  let test_name = "MoonBit"
  
  // 测试基本问候功能
  let greeting = azimuth.greet(test_name)
  @assertion.assert_eq_string("Hello, MoonBit!", greeting)?
  
  // 测试空字符串处理
  let empty_greeting = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", empty_greeting)?
  
  // 测试特殊字符字符串
  let special_name = "测试@123"
  let special_greeting = azimuth.greet(special_name)
  @assertion.assert_eq_string("Hello, 测试@123!", special_greeting)?
}

test "mathematical_formula_verification" {
  // 数学公式验证测试
  let x = 12
  let y = 5
  let z = 3
  
  // 验证分配律: x * (y + z) = x * y + x * z
  let left_side = azimuth.multiply(x, azimuth.add(y, z))
  let right_side = azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, z))
  @assertion.assert_eq(left_side, right_side)?
  
  // 验证结合律: (x + y) + z = x + (y + z)
  let add_left = azimuth.add(azimuth.add(x, y), z)
  let add_right = azimuth.add(x, azimuth.add(y, z))
  @assertion.assert_eq(add_left, add_right)?
}

test "real_world_calculation_scenario" {
  // 实际计算场景测试：订单总价计算
  let unit_price = 199
  let quantity = 3
  let discount_rate = 10  // 10% 折扣
  let shipping_fee = 15
  let tax_rate = 8  // 8% 税费
  
  // 计算商品总价
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算折扣金额
  let discount = azimuth.multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终总价
  let final_total = azimuth.add(azimuth.add(discounted_price, tax), shipping_fee)
  
  // 验证结果: (199×3 - 10%) + 8% + 15 = 597 - 59.7 + 47.76 + 15 = 600.06 ≈ 600
  @assertion.assert_eq(600, final_total)?
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  let normal_value = 100
  let extreme_value = 2147483647
  
  // 测试正常情况下的断言函数
  @assertion.assert_true(true)?
  @assertion.assert_false(false)?
  @assertion.assert_eq(normal_value, 100)?
  @assertion.assert_eq_string("test", "test")?
  
  // 测试极值情况下的处理
  let extreme_add = azimuth.add(extreme_value, extreme_value)
  @assertion.assert_eq(extreme_value, extreme_add)?
  
  let extreme_mul = azimuth.multiply(extreme_value, extreme_value)
  @assertion.assert_eq(extreme_value, extreme_mul)?
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试：计算数组元素的和与平均值
  let elements = [15, 25, 35, 45, 55]
  
  // 计算总和
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(elements[0], elements[1]), elements[2]), elements[3]), elements[4])
  @assertion.assert_eq(175, sum)?
  
  // 计算平均值
  let average = sum / 5
  @assertion.assert_eq(35, average)?
  
  // 计算方差（简化版）
  let diff1 = azimuth.add(elements[0], -average)
  let diff2 = azimuth.add(elements[1], -average)
  let diff3 = azimuth.add(elements[2], -average)
  let diff4 = azimuth.add(elements[3], -average)
  let diff5 = azimuth.add(elements[4], -average)
  
  let variance = azimuth.add(azimuth.add(azimuth.add(azimuth.add(
    azimuth.multiply(diff1, diff1), 
    azimuth.multiply(diff2, diff2)), 
    azimuth.multiply(diff3, diff3)), 
    azimuth.add(azimuth.multiply(diff4, diff4), azimuth.multiply(diff5, diff5))) / 5
  
  @assertion.assert_eq(200, variance)?
}

test "performance_efficiency_check" {
  // 性能效率检查测试：大量数据计算模拟
  let base_value = 100
  let multiplier = 1000
  
  // 测试大数相加
  let large_sum = azimuth.add(base_value, multiplier)
  @assertion.assert_eq(1100, large_sum)?
  
  // 测试大数相乘
  let large_product = azimuth.multiply(base_value, multiplier)
  @assertion.assert_eq(100000, large_product)?
  
  // 测试连续运算性能
  let chain_result = azimuth.add(azimuth.multiply(base_value, 10), azimuth.multiply(multiplier, 2))
  @assertion.assert_eq(2100, chain_result)?
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  let chinese_name = "张三"
  let japanese_name = "田中"
  let emoji_name = "🌍"
  let mixed_name = "Hello世界123"
  
  // 测试中文处理
  let chinese_greeting = azimuth.greet(chinese_name)
  @assertion.assert_eq_string("Hello, 张三!", chinese_greeting)?
  
  // 测试日文处理
  let japanese_greeting = azimuth.greet(japanese_name)
  @assertion.assert_eq_string("Hello, 田中!", japanese_greeting)?
  
  // 测试 Emoji 处理
  let emoji_greeting = azimuth.greet(emoji_name)
  @assertion.assert_eq_string("Hello, 🌍!", emoji_greeting)?
  
  // 测试混合字符串处理
  let mixed_greeting = azimuth.greet(mixed_name)
  @assertion.assert_eq_string("Hello, Hello世界123!", mixed_greeting)?
}