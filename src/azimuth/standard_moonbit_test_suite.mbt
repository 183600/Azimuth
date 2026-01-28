// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 为 azimuth 库提供全面的测试覆盖

test "error_boundary_handling" {
  // 测试错误边界处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "mathematical_series_sum" {
  // 测试数学级数求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 测试等差数列：2 + 4 + 6 + 8 = 20
  let sum2 = add(add(add(2, 4), 6), 8)
  assert_eq(20, sum2)
}

test "string_greeting_edge_cases" {
  // 测试字符串问候边界情况
  assert_eq_string("Hello,   !", greet("  "))  // 空格
  assert_eq_string("Hello, \n!", greet("\n"))  // 换行符
  assert_eq_string("Hello, \t!", greet("\t"))  // 制表符
  assert_eq_string("Hello, A!", greet("A"))    // 单字符
}

test "complex_calculator_operations" {
  // 测试复杂计算器操作
  // 计算：(5 + 3) * 2 - 10 / 3
  let step1 = add(5, 3)
  let step2 = multiply(step1, 2)
  let step3 = divide_with_ceil(10, 3)
  let result = add(step2, -step3)
  assert_eq(13, result)  // (5 + 3) * 2 - ceil(10/3) = 8 * 2 - 4 = 16 - 4 = 12
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_resources = 100
  let project1_needs = 35
  let project2_needs = 28
  let project3_needs = 22
  
  let allocated = add(add(project1_needs, project2_needs), project3_needs)
  let remaining = add(total_resources, -allocated)
  let additional_projects = divide_with_ceil(remaining, 10)
  
  assert_eq(85, allocated)
  assert_eq(15, remaining)
  assert_eq(2, additional_projects)
}

test "time_zone_conversion_simulation" {
  // 测试时区转换模拟
  let local_time = 14  // 下午2点
  let timezone_offset = 8  // 东8区
  let target_offset = -5  // 西5区
  
  let time_diff = add(target_offset, -timezone_offset)
  let converted_time = add(local_time, time_diff)
  let normalized_time = if converted_time < 0 {
    add(converted_time, 24)
  } else if converted_time >= 24 {
    add(converted_time, -24)
  } else {
    converted_time
  }
  
  assert_eq(1, normalized_time)  // 14 + (-5 - 8) = 14 - 13 = 1 (凌晨1点)
}

test "geometry_area_calculation" {
  // 测试几何面积计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积
  let floor_area = multiply(length, width)
  
  // 计算四个墙面的总面积
  let wall_area = multiply(2, multiply(add(length, width), height))
  
  // 计算总表面积
  let total_area = add(floor_area, wall_area)
  
  assert_eq(96, floor_area)
  assert_eq(200, wall_area)
  assert_eq(296, total_area)
}

test "financial_compound_interest" {
  // 测试金融复利计算（简化版）
  let principal = 1000
  let rate_percent = 10
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1100, year1)
  assert_eq(1210, year2)
  assert_eq(1331, year3)
}

test "data_structure_simulation" {
  // 测试数据结构模拟（栈操作）
  let stack1 = 10
  let stack2 = add(stack1, 20)  // push 20
  let stack3 = add(stack2, 30)  // push 30
  let stack4 = add(stack3, -30) // pop 30
  let stack5 = add(stack4, -20) // pop 20
  
  assert_eq(30, stack2)
  assert_eq(60, stack3)
  assert_eq(30, stack4)
  assert_eq(10, stack5)
}

test "algorithm_efficiency_benchmark" {
  // 测试算法效率基准
  let base_operations = 100
  let algorithm1_factor = 3
  let algorithm2_factor = 5
  let optimization_factor = 2
  
  // 算法1的复杂度
  let algo1_ops = multiply(base_operations, algorithm1_factor)
  
  // 算法2的复杂度
  let algo2_ops = multiply(base_operations, algorithm2_factor)
  
  // 优化后的算法1
  let optimized_algo1 = divide_with_ceil(algo1_ops, optimization_factor)
  
  // 性能比较
  let performance_diff = add(algo2_ops, -optimized_algo1)
  
  assert_eq(300, algo1_ops)
  assert_eq(500, algo2_ops)
  assert_eq(150, optimized_algo1)
  assert_eq(350, performance_diff)
}