// 用户自定义的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖 azimuth 模块的主要功能

// 导入 azimuth 模块
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 测试加法
  let sum = azimuth.add(a, b)
  @assertion.assert_eq(22, sum)?
  
  // 测试乘法
  let product = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product)?
  
  // 测试复合运算
  let complex = azimuth.add(azimuth.multiply(a, b), azimuth.add(a, b))
  @assertion.assert_eq(127, complex)?
}

test "string_processing_functionality" {
  // 测试字符串处理功能
  let name1 = "MoonBit"
  let name2 = "开发者"
  let name3 = "🚀"
  
  // 测试基本字符串拼接
  let result1 = azimuth.greet(name1)
  @assertion.assert_eq_string("Hello, MoonBit!", result1)?
  
  // 测试中文字符串
  let result2 = azimuth.greet(name2)
  @assertion.assert_eq_string("Hello, 开发者!", result2)?
  
  // 测试emoji字符串
  let result3 = azimuth.greet(name3)
  @assertion.assert_eq_string("Hello, 🚀!", result3)?
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的加法溢出保护
  let overflow_add = azimuth.add(max_val, 100)
  @assertion.assert_eq(max_val, overflow_add)?
  
  // 测试最小值的加法溢出保护
  let underflow_add = azimuth.add(min_val, -100)
  @assertion.assert_eq(min_val, underflow_add)?
  
  // 测试最大值的乘法溢出保护
  let overflow_mul = azimuth.multiply(max_val, 2)
  @assertion.assert_eq(max_val, overflow_mul)?
  
  // 测试最小值的特殊处理
  let min_mul = azimuth.multiply(min_val, -1)
  @assertion.assert_eq(min_val, min_mul)?
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let x = 12
  let y = 8
  let z = 5
  
  // 验证加法交换律
  let commutative_add1 = azimuth.add(x, y)
  let commutative_add2 = azimuth.add(y, x)
  @assertion.assert_eq(commutative_add1, commutative_add2)?
  
  // 验证乘法交换律
  let commutative_mul1 = azimuth.multiply(x, y)
  let commutative_mul2 = azimuth.multiply(y, x)
  @assertion.assert_eq(commutative_mul1, commutative_mul2)?
  
  // 验证分配律
  let distributive1 = azimuth.multiply(x, azimuth.add(y, z))
  let distributive2 = azimuth.add(azimuth.multiply(x, y), azimuth.multiply(x, z))
  @assertion.assert_eq(distributive1, distributive2)?
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5          // 利率（百分比）
  let years = 3         // 年数
  
  // 计算简单利息
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  @assertion.assert_eq(150000, interest)?
  
  // 计算本息和
  let total = azimuth.add(principal, interest)
  @assertion.assert_eq(160000, total)?
  
  // 测试贷款计算
  let monthly_payment = azimuth.divide_with_ceil(total, azimuth.multiply(years, 12))
  @assertion.assert_eq(4445, monthly_payment)?
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  let radius = 4
  
  // 计算矩形周长
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(32, perimeter)?
  
  // 计算矩形面积
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(60, area)?
  
  // 计算圆周长（简化：2 × π × r ≈ 2 × 3 × r）
  let circumference = azimuth.multiply(azimuth.multiply(2, 3), radius)
  @assertion.assert_eq(24, circumference)?
  
  // 计算圆面积（简化：π × r² ≈ 3 × r²）
  let circle_area = azimuth.multiply(3, azimuth.multiply(radius, radius))
  @assertion.assert_eq(48, circle_area)?
}

test "temperature_conversion" {
  // 测试温度转换
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  // 测试冰点
  let celsius1 = 0
  let fahrenheit1 = azimuth.add(azimuth.multiply(celsius1, 2), 30)
  @assertion.assert_eq(30, fahrenheit1)?
  
  // 测试室温
  let celsius2 = 20
  let fahrenheit2 = azimuth.add(azimuth.multiply(celsius2, 2), 30)
  @assertion.assert_eq(70, fahrenheit2)?
  
  // 测试沸点
  let celsius3 = 100
  let fahrenheit3 = azimuth.add(azimuth.multiply(celsius3, 2), 30)
  @assertion.assert_eq(230, fahrenheit3)?
}

test "shopping_cart_calculation" {
  // 测试购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 计算商品总价
  let subtotal1 = azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  // 验证总价：(299×2 + 199×1) + 税费 + 运费 = 797 + 63 + 15 = 875
  @assertion.assert_eq(875, total)?
}

test "statistical_analysis" {
  // 测试统计分析
  let data1 = 85
  let data2 = 90
  let data3 = 78
  let data4 = 92
  let data5 = 80
  
  // 计算总和
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(data1, data2), data3), data4), data5)
  @assertion.assert_eq(425, sum)?
  
  // 计算平均值
  let mean = sum / 5
  @assertion.assert_eq(85, mean)?
  
  // 计算极差
  let max_val = 92
  let min_val = 78
  let range = azimuth.add(max_val, azimuth.multiply(min_val, -1))
  @assertion.assert_eq(14, range)?
  
  // 计算方差（简化）
  let diff1 = azimuth.add(data1, azimuth.multiply(mean, -1))
  let diff2 = azimuth.add(data2, azimuth.multiply(mean, -1))
  let variance = azimuth.add(azimuth.multiply(diff1, diff1), azimuth.multiply(diff2, diff2)) / 5
  @assertion.assert_eq(26, variance)?
}

test "game_logic_simulation" {
  // 测试游戏逻辑模拟
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let bonus_points = 500
  let penalty = 200
  
  // 计算等级加成
  let level_bonus = azimuth.multiply(base_score, level)
  
  // 计算连击加成
  let combo_bonus = azimuth.multiply(level_bonus, combo_multiplier)
  
  // 计算最终得分
  let final_score = azimuth.add(azimuth.add(combo_bonus, bonus_points), azimuth.multiply(penalty, -1))
  
  // 验证得分：(1000×5)×3 + 500 - 200 = 15000 + 300 = 15300
  @assertion.assert_eq(15300, final_score)?
  
  // 测试排名计算（前10%额外加分）
  let top_percentage = 10
  let rank_bonus = azimuth.multiply(final_score, top_percentage) / 100
  let rank_total = azimuth.add(final_score, rank_bonus)
  
  @assertion.assert_eq(16830, rank_total)?
}