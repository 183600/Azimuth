// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(-1, azimuth.add(2, -3))?
  @assertion.assert_eq(0, azimuth.add(5, -5))?
  
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(-6, azimuth.multiply(2, -3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
}

test "edge_case_calculations" {
  // 边界情况计算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(max_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_int, 0))?
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  // 测试 (a+b)*(c+d) 形式的表达式
  let a = 2
  let b = 3
  let c = 4
  let d = 5
  
  let sum1 = azimuth.add(a, b)
  let sum2 = azimuth.add(c, d)
  let result = azimuth.multiply(sum1, sum2)
  
  @assertion.assert_eq(35, result)?  // (2+3)*(4+5) = 5*9 = 45
}

test "business_scenario_calculations" {
  // 商业场景计算测试
  // 计算总价：单价 × 数量
  let unit_price = 50
  let quantity = 8
  let total_price = azimuth.multiply(unit_price, quantity)
  @assertion.assert_eq(400, total_price)?
  
  // 计算折扣：总价 × 折扣率
  let discount_rate = 10  // 10%
  let discount_amount = azimuth.multiply(total_price, discount_rate) / 100
  @assertion.assert_eq(40, discount_amount)?
  
  // 计算最终价格：总价 - 折扣
  let final_price = azimuth.add(total_price, -discount_amount)
  @assertion.assert_eq(360, final_price)?
}

test "geometric_calculations" {
  // 几何计算测试
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 计算立方体体积：长 × 宽 × 高
  let height = 5
  let volume = azimuth.multiply(area, height)
  @assertion.assert_eq(480, volume)?
}

test "series_and_pattern_calculations" {
  // 级数和模式计算测试
  // 计算连续整数和：1+2+3+4+5
  let sum1 = azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 2), 3), 4), 5)
  @assertion.assert_eq(15, sum1)?
  
  // 计算等差数列：2+4+6+8+10
  let sum2 = azimuth.add(azimuth.add(azimuth.add(azimuth.add(2, 4), 6), 8), 10)
  @assertion.assert_eq(30, sum2)?
  
  // 计算平方数列：1+4+9+16
  let sum3 = azimuth.add(azimuth.add(azimuth.add(1, 4), 9), 16)
  @assertion.assert_eq(30, sum3)?
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  let zero = 0
  let one = 1
  let negative_one = -1
  
  // 测试零值运算
  @assertion.assert_eq(0, azimuth.add(zero, zero))?
  @assertion.assert_eq(0, azimuth.multiply(zero, zero))?
  @assertion.assert_eq(0, azimuth.multiply(zero, one))?
  @assertion.assert_eq(0, azimuth.multiply(zero, negative_one))?
  
  // 测试单位元运算
  @assertion.assert_eq(1, azimuth.add(zero, one))?
  @assertion.assert_eq(-1, azimuth.add(zero, negative_one))?
  @assertion.assert_eq(5, azimuth.multiply(one, 5))?
  @assertion.assert_eq(-5, azimuth.multiply(negative_one, 5))?
}

test "performance_and_optimization_tests" {
  // 性能和优化测试
  // 测试重复运算的性能
  let base = 7
  let iterations = 5
  
  // 重复加法 vs 乘法
  let repeated_add = azimuth.add(azimuth.add(azimuth.add(azimuth.add(base, base), base), base), base)
  let multiplication_result = azimuth.multiply(base, iterations)
  
  @assertion.assert_eq(repeated_add, multiplication_result)?
  
  // 测试嵌套运算
  let nested1 = azimuth.multiply(azimuth.add(2, 3), azimuth.add(4, 5))
  let nested2 = azimuth.multiply(azimuth.add(10, 20), azimuth.add(30, 40))
  
  @assertion.assert_eq(45, nested1)?  // (2+3)*(4+5) = 5*9 = 45
  @assertion.assert_eq(2100, nested2)?  // (10+20)*(30+40) = 30*70 = 2100
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let item1_total = azimuth.multiply(item1_price, item1_quantity)
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  let cart_total = azimuth.add(item1_total, item2_total)
  
  @assertion.assert_eq(697, cart_total)?  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 场景2：计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  
  let total_score = azimuth.add(azimuth.add(azimuth.add(score1, score2), score3), score4)
  let average_score = total_score / 4  // 假设有4个科目
  
  @assertion.assert_eq(86, average_score)?  // (85+90+78+92)/4 = 345/4 = 86.25 → 86 (整数除法)
  
  // 场景3：用户问候功能集成
  let user_name = "李四"
  let greeting_message = azimuth.greet(user_name)
  @assertion.assert_eq_string("Hello, 李四!", greeting_message)?
}