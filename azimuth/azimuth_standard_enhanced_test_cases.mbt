// Azimuth 项目标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

// 导入被测试的模块
use azimuth

test "arithmetic_comprehensive_operations" {
  // 综合算术运算测试
  // 测试基本加法运算
  azimuth.assert_eq(42, azimuth.add(20, 22))
  azimuth.assert_eq(0, azimuth.add(0, 0))
  azimuth.assert_eq(-5, azimuth.add(-8, 3))
  azimuth.assert_eq(1000, azimuth.add(500, 500))
  
  // 测试基本乘法运算
  azimuth.assert_eq(25, azimuth.multiply(5, 5))
  azimuth.assert_eq(0, azimuth.multiply(100, 0))
  azimuth.assert_eq(-21, azimuth.multiply(-7, 3))
  azimuth.assert_eq(36, azimuth.multiply(-6, -6))
  
  // 测试减法运算
  azimuth.assert_eq(15, azimuth.subtract(25, 10))
  azimuth.assert_eq(-10, azimuth.subtract(5, 15))
  azimuth.assert_eq(0, azimuth.subtract(50, 50))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 基本正数除法
  azimuth.assert_eq(4, azimuth.divide_with_ceil(10, 3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(9, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 10))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
  
  // 除零情况处理
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 负数除法场景
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  azimuth.assert_eq(-4, azimuth.divide_with_ceil(10, -3))
  azimuth.assert_eq(4, azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  // 基本英文问候
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  
  // 中文字符测试
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 用户!", azimuth.greet("用户"))
  
  // 特殊字符和数字
  azimuth.assert_eq_string("Hello, 123!", azimuth.greet("123"))
  azimuth.assert_eq_string("Hello, @#$!", azimuth.greet("@#$"))
  
  // 空字符串和空格
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律: a + b = b + a
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  azimuth.assert_eq(azimuth.add(azimuth.add(a, b), c), azimuth.add(a, azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  azimuth.assert_eq(azimuth.multiply(azimuth.multiply(a, b), c), azimuth.multiply(a, azimuth.multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  azimuth.assert_eq(azimuth.multiply(a, azimuth.add(b, c)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c)))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库货物装箱
  let total_items = 158
  let box_capacity = 25
  
  // 计算需要的箱子数量
  let boxes_needed = azimuth.divide_with_ceil(total_items, box_capacity)
  azimuth.assert_eq(7, boxes_needed)  // ceil(158/25) = 7
  
  // 计算总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  azimuth.assert_eq(175, total_capacity)
  
  // 计算剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(17, remaining_space)
  
  // 验证空间利用率
  azimuth.assert_true(total_items <= total_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极小值测试
  azimuth.assert_eq(1, azimuth.add(1, 0))
  azimuth.assert_eq(0, azimuth.multiply(1, 0))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(-1, -1))
  
  // 边界除法测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(9999, 10000))
  azimuth.assert_eq(10000, azimuth.divide_with_ceil(10000, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(10000, 10000))
  
  // 零元素性质
  let test_value = 42
  azimuth.assert_eq(test_value, azimuth.add(test_value, 0))
  azimuth.assert_eq(0, azimuth.multiply(test_value, 0))
  azimuth.assert_eq(test_value, azimuth.multiply(test_value, 1))
  azimuth.assert_eq(0, azimuth.subtract(test_value, test_value))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划和预算分配
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let working_days = 15
  let budget_per_sprint = 10000
  
  // 计算总工时
  let total_hours = azimuth.multiply(team_members, azimuth.multiply(hours_per_day, working_days))
  azimuth.assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总成本
  let total_cost = azimuth.multiply(total_hours, hourly_rate)
  azimuth.assert_eq(72000, total_cost)  // 960 * 75 = 72000
  
  // 计算需要的冲刺数
  let sprints_needed = azimuth.divide_with_ceil(total_cost, budget_per_sprint)
  azimuth.assert_eq(8, sprints_needed)  // ceil(72000/10000) = 8
  
  // 计算实际预算
  let actual_budget = azimuth.multiply(sprints_needed, budget_per_sprint)
  azimuth.assert_eq(80000, actual_budget)
  
  // 计算剩余预算
  let remaining_budget = azimuth.subtract(actual_budget, total_cost)
  azimuth.assert_eq(8000, remaining_budget)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let large_positive = 100000
  let large_negative = -100000
  
  azimuth.assert_eq(200000, azimuth.add(large_positive, large_positive))
  azimuth.assert_eq(-200000, azimuth.add(large_negative, large_negative))
  azimuth.assert_eq(0, azimuth.add(large_positive, large_negative))
  
  azimuth.assert_eq(10000000000, azimuth.multiply(large_positive, large_positive))
  azimuth.assert_eq(10000000000, azimuth.multiply(large_negative, large_negative))
  azimuth.assert_eq(-10000000000, azimuth.multiply(large_positive, large_negative))
  
  // 特殊字符串处理测试
  azimuth.assert_eq_string("Hello, \t\n!", azimuth.greet("\t\n"))
  azimuth.assert_eq_string("Hello, 空格测试!", azimuth.greet("空格测试"))
  azimuth.assert_eq_string("Hello, mixed123中文!", azimuth.greet("mixed123中文"))
}