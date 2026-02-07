// Azimuth 项目标准综合测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

// 导入被测试的模块
use azimuth

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  azimuth.assert_eq(15, azimuth.add(7, 8))
  azimuth.assert_eq(-3, azimuth.add(-10, 7))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  
  azimuth.assert_eq(56, azimuth.multiply(7, 8))
  azimuth.assert_eq(-70, azimuth.multiply(-7, 10))
  azimuth.assert_eq(0, azimuth.multiply(0, 999))
  
  azimuth.assert_eq(5, azimuth.subtract(12, 7))
  azimuth.assert_eq(-5, azimuth.subtract(7, 12))
  azimuth.assert_eq(0, azimuth.subtract(100, 100))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 100))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 10))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(15, 0))
  
  // 负数测试
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(-10, -3))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 17
  let b = 23
  
  // 加法交换律和结合律
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  azimuth.assert_eq(azimuth.add(azimuth.add(a, b), 5), azimuth.add(a, azimuth.add(b, 5)))
  
  // 乘法交换律和结合律
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  azimuth.assert_eq(azimuth.multiply(azimuth.multiply(a, b), 2), azimuth.multiply(a, azimuth.multiply(b, 2)))
  
  // 分配律
  azimuth.assert_eq(azimuth.multiply(a, azimuth.add(b, 3)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3)))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
  azimuth.assert_eq_string("Hello, 中文测试!", azimuth.greet("中文测试"))
  azimuth.assert_eq_string("Hello, éspanol!", azimuth.greet("éspanol"))
  azimuth.assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
}

test "boundary_conditions_extreme" {
  // 边界条件和极值测试
  // 大数运算
  azimuth.assert_eq(2000000, azimuth.add(1000000, 1000000))
  azimuth.assert_eq(100000000, azimuth.multiply(10000, 10000))
  azimuth.assert_eq(1000001, azimuth.divide_with_ceil(1000000, 1))
  
  // 边界除法
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 1))
  
  // 极值减法
  azimuth.assert_eq(0, azimuth.subtract(2147483647, 2147483647))
  azimuth.assert_eq(-2147483648, azimuth.subtract(-2147483647, 1))
}

test "error_handling_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 零乘法测试
  azimuth.assert_eq(0, azimuth.multiply(2147483647, 0))
  azimuth.assert_eq(0, azimuth.multiply(-2147483648, 0))
  azimuth.assert_eq(0, azimuth.multiply(0, 0))
}

test "business_logistics_scenario" {
  // 物流业务场景测试
  let total_packages = 347
  let truck_capacity = 48
  let trucks_needed = azimuth.divide_with_ceil(total_packages, truck_capacity)
  
  azimuth.assert_eq(8, trucks_needed)
  
  // 验证总运力
  let total_capacity = azimuth.multiply(trucks_needed, truck_capacity)
  azimuth.assert_eq(384, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_packages)
  azimuth.assert_eq(37, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = azimuth.divide_with_ceil(azimuth.multiply(total_packages, 100), total_capacity)
  azimuth.assert_eq(91, utilization_percent)  // floor(34700/384) = 90%
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  let tax_rate = 20  // 20%
  
  // 计算总利息
  let total_interest = azimuth.multiply(principal, azimuth.multiply(interest_rate, years))
  let total_interest_amount = azimuth.divide_with_ceil(total_interest, 100)
  
  // 计算税后利息
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(total_interest_amount, tax_rate), 100)
  let net_interest = azimuth.subtract(total_interest_amount, tax_amount)
  
  // 计算最终金额
  let final_amount = azimuth.add(principal, net_interest)
  
  azimuth.assert_eq(1500, total_interest_amount)  // 10000 * 5% * 3 = 1500
  azimuth.assert_eq(300, tax_amount)  // 1500 * 20% = 300
  azimuth.assert_eq(1200, net_interest)  // 1500 - 300 = 1200
  azimuth.assert_eq(11200, final_amount)  // 10000 + 1200 = 11200
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：多阶段项目规划
  let phase1_days = 15
  let phase2_days = 20
  let phase3_days = 10
  let team_size = 6
  let hours_per_day = 8
  let cost_per_hour = 75
  
  // 计算总天数
  let total_days = azimuth.add(azimuth.add(phase1_days, phase2_days), phase3_days)
  
  // 计算总工时
  let total_hours = azimuth.multiply(total_days, azimuth.multiply(team_size, hours_per_day))
  
  // 计算总成本
  let total_cost = azimuth.multiply(total_hours, cost_per_hour)
  
  // 计算需要的冲刺数（每冲刺10天）
  let sprints_needed = azimuth.divide_with_ceil(total_days, 10)
  
  azimuth.assert_eq(45, total_days)  // 15 + 20 + 10 = 45
  azimuth.assert_eq(2160, total_hours)  // 45 * 6 * 8 = 2160
  azimuth.assert_eq(162000, total_cost)  // 2160 * 75 = 162000
  azimuth.assert_eq(5, sprints_needed)  // ceil(45/10) = 5
}

test "unicode_and_special_characters" {
  // Unicode 和特殊字符综合测试
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🌟✨🚀!", azimuth.greet("🌟✨🚀"))
  azimuth.assert_eq_string("Hello, café!", azimuth.greet("café"))
  azimuth.assert_eq_string("Hello, Москва!", azimuth.greet("Москва"))
  azimuth.assert_eq_string("Hello, ñoño!", azimuth.greet("ñoño"))
  azimuth.assert_eq_string("Hello, 🏳️‍🌈!", azimuth.greet("🏳️‍🌈"))
  azimuth.assert_eq_string("Hello, العربية!", azimuth.greet("العربية"))
  azimuth.assert_eq_string("Hello, 🎉🎊🥳!", azimuth.greet("🎉🎊🥳"))
  azimuth.assert_eq_string("Hello, special@#$%&*()!", azimuth.greet("special@#$%&*()"))
  azimuth.assert_eq_string("Hello, line\nbreak\tand\ttabs!", azimuth.greet("line\nbreak\tand\ttabs"))
}