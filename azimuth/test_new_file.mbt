// 新测试文件 - 包含10个标准 MoonBit 测试用例

test "mathematical_identities_test" {
  // 数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "large_number_operations_test" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "negative_number_comprehensive_test" {
  // 负数综合测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, add(-15, 25))
  
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  
  assert_eq(-10, subtract(-5, 5))
  assert_eq(10, subtract(5, -5))
}

test "business_resource_calculation_test" {
  // 业务资源计算测试
  let employees = 15
  let projects_per_employee = 4
  let total_projects = multiply(employees, projects_per_employee)
  
  let sprints_per_project = 3
  let total_sprints = multiply(total_projects, sprints_per_project)
  
  let sprints_per_quarter = 20
  let quarters_needed = divide_with_ceil(total_sprints, sprints_per_quarter)
  
  assert_eq(60, total_projects)     // 15 * 4 = 60
  assert_eq(180, total_sprints)     // 60 * 3 = 180
  assert_eq(9, quarters_needed)     // ceil(180/20) = 9
}

test "string_handling_edge_cases_test" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, ALLCAPS!", greet("ALLCAPS"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, space at end !", greet("space at end "))
}

test "complex_calculation_workflow_test" {
  // 复杂计算工作流测试
  let initial_value = 10
  
  // 第一步：加法运算
  let step1 = add(initial_value, 5)  // 15
  
  // 第二步：乘法运算
  let step2 = multiply(step1, 3)     // 45
  
  // 第三步：除法运算
  let step3 = divide_with_ceil(step2, 20)  // 3
  
  // 第四步：减法运算
  let final_result = subtract(step3, 1)    // 2
  
  assert_eq(2, final_result)
}

test "boundary_value_analysis_test" {
  // 边界值分析测试
  // 最小边界值
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 最大边界值
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 特殊边界值
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "error_handling_robustness_test" {
  // 错误处理健壮性测试
  // 除零情况的各种测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
}

test "real_world_inventory_management_test" {
  // 真实世界库存管理测试
  let product_types = 8
  let items_per_type = 125
  let total_items = multiply(product_types, items_per_type)
  
  let box_capacity = 50
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  let cost_per_box = 25
  let total_cost = multiply(boxes_needed, cost_per_box)
  
  let budget = 600
  let within_budget = total_cost <= budget
  
  assert_eq(1000, total_items)      // 8 * 125 = 1000
  assert_eq(20, boxes_needed)       // ceil(1000/50) = 20
  assert_eq(500, total_cost)        // 20 * 25 = 500
  assert_true(within_budget)        // 500 <= 600
}