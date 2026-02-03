// 标准 MoonBit 测试用例 - Azimuth 项目增强版
// 包含8个高质量测试用例，覆盖核心功能和边界情况

// 导入被测试的模块
use azimuth

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  azimuth.assert_eq(10, azimuth.add(7, 3))
  azimuth.assert_eq(-4, azimuth.add(-7, 3))
  azimuth.assert_eq(21, azimuth.multiply(7, 3))
  azimuth.assert_eq(-21, azimuth.multiply(-7, 3))
  azimuth.assert_eq(4, azimuth.subtract(10, 6))
  azimuth.assert_eq(-4, azimuth.subtract(6, 10))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  azimuth.assert_eq(3, azimuth.divide_with_ceil(8, 3))
  azimuth.assert_eq(-2, azimuth.divide_with_ceil(-8, 3))
  azimuth.assert_eq(-3, azimuth.divide_with_ceil(8, -3))
  azimuth.assert_eq(3, azimuth.divide_with_ceil(-8, -3))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 5))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(5, 0))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  azimuth.assert_eq_string("Hello, 🚀!", azimuth.greet("🚀"))
  azimuth.assert_eq_string("Hello, São Paulo!", azimuth.greet("São Paulo"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  azimuth.assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  azimuth.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 加法恒等式
  azimuth.assert_eq(a, azimuth.add(a, 0))
  azimuth.assert_eq(a, azimuth.add(0, a))
  
  // 乘法恒等式
  azimuth.assert_eq(a, azimuth.multiply(a, 1))
  azimuth.assert_eq(a, azimuth.multiply(1, a))
}

test "business_packaging_optimization" {
  // 业务包装优化场景测试
  let total_items = 127
  let box_capacity = 15
  let boxes_needed = azimuth.divide_with_ceil(total_items, box_capacity)
  
  azimuth.assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  azimuth.assert_eq(135, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_items)
  azimuth.assert_eq(8, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = azimuth.divide_with_ceil(azimuth.multiply(total_items, 100), total_capacity)
  azimuth.assert_eq(94, utilization_percent)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 9
  let tasks_per_member = 7
  let total_tasks = azimuth.multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_sprint = 15
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_sprint)
  
  let total_weeks = azimuth.multiply(sprints_needed, sprint_duration)
  
  azimuth.assert_eq(63, total_tasks)  // 9 * 7 = 63
  azimuth.assert_eq(5, sprints_needed)  // ceil(63/15) = 5
  azimuth.assert_eq(10, total_weeks)  // 5 * 2 = 10
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误处理
  azimuth.assert_eq(0, azimuth.divide_with_ceil(10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-10, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  azimuth.assert_eq(0, azimuth.multiply(1000000, 0))
  azimuth.assert_eq(1000000, azimuth.add(1000000, 0))
  azimuth.assert_eq(0, azimuth.subtract(1000000, 1000000))
  
  // 空字符串处理
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
}

test "performance_large_numbers" {
  // 大数性能测试
  let large_num1 = 10000
  let large_num2 = 10000
  
  // 大数运算
  azimuth.assert_eq(20000, azimuth.add(large_num1, large_num2))
  azimuth.assert_eq(100000000, azimuth.multiply(large_num1, large_num2))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(large_num1, large_num2))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(large_num1, 20000))
  
  // 复合运算
  let result = azimuth.add(azimuth.multiply(large_num1, 2), azimuth.divide_with_ceil(large_num2, 3))
  azimuth.assert_eq(23334, result)  // 10000*2 + ceil(10000/3) = 20000 + 3334
}