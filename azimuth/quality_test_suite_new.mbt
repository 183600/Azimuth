// 高质量 MoonBit 测试套件 - 新增10个标准测试用例
// 覆盖边界情况、实际应用场景和数学性质验证

test "arithmetics_identity_elements" {
  // 测试算术恒等元
  assert_eq(5, add(5, 0))  // 加法恒等元
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))  // 乘法零元
  assert_eq(5, multiply(5, 1))  // 乘法恒等元
  assert_eq(5, multiply(1, 5))
}

test "divide_with_ceil_precision_boundary" {
  // 测试向上取整除法的精度边界
  assert_eq(1, divide_with_ceil(1, 2))    // 最小真分数
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 向上取整
  assert_eq(1000, divide_with_ceil(1999, 2))  // 大数精度测试
  assert_eq(33334, divide_with_ceil(100000, 3))  // 循环小数向上取整
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "business_inventory_calculation" {
  // 业务场景：库存计算
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_products)
  assert_eq(11, unused_space)
}

test "mathematical_distributive_law" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "negative_number_comprehensive" {
  // 负数综合测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(-2, add(5, -7))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "project_resource_allocation" {
  // 项目资源分配场景
  let project_tasks = 89
  let team_members = 6
  let tasks_per_member = divide_with_ceil(project_tasks, team_members)
  
  assert_eq(15, tasks_per_member)  // ceil(89/6) = 15
  
  // 验证总处理能力
  let total_capacity = multiply(tasks_per_member, team_members)
  assert_eq(90, total_capacity)
  
  // 验证任务分配合理性
  assert_true(total_capacity >= project_tasks)
}

test "edge_case_zero_handling" {
  // 零值处理边缘情况
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(5, 0))  // 除零保护
  assert_eq(0, divide_with_ceil(0, 0))  // 0/0 保护
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 12
  
  // 计算步骤：((base * 3) + 10) / 5 + 2
  let step1 = multiply(base_value, 3)      // 36
  let step2 = add(step1, 10)               // 46
  let step3 = divide_with_ceil(step2, 5)   // 10
  let result = add(step3, 2)               // 12
  
  assert_eq(12, result)
}

test "performance_large_scale_computation" {
  // 大规模计算性能测试
  let large_number = 100000
  let iterations = 10
  
  // 批量加法
  let sum_result = multiply(large_number, iterations)
  assert_eq(1000000, sum_result)
  
  // 批量乘法
  let product_result = multiply(large_number, large_number)
  assert_eq(10000000000, product_result)
  
  // 大数除法
  let division_result = divide_with_ceil(product_result, large_number)
  assert_eq(100000, division_result)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}