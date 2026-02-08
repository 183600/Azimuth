// Azimuth 增强测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例
// 覆盖边界情况、特殊场景和复杂业务逻辑

test "enhanced_zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
}

test "enhanced_large_number_calculations" {
  // 大数计算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "enhanced_complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目资源分配
  let team_members = 12
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  
  assert_eq(96, total_tasks)  // 12 * 8 = 96
  assert_eq(5, sprints_needed)  // ceil(96/20) = 5
  assert_eq(100, total_capacity)  // 5 * 20 = 100
  assert_eq(4, unused_capacity)  // 100 - 96 = 4
}

test "enhanced_distributive_property" {
  // 分配律测试
  let a = 7
  let b = 5
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "enhanced_resource_allocation_scenario" {
  // 资源分配场景测试
  // 场景：计算存储需求
  let files_count = 150
  let file_size_mb = 8
  let total_size_mb = multiply(files_count, file_size_mb)
  
  let storage_capacity_gb = 2
  let storage_capacity_mb = multiply(storage_capacity_gb, 1024)
  let storage_units_needed = divide_with_ceil(total_size_mb, storage_capacity_mb)
  
  assert_eq(1200, total_size_mb)  // 150 * 8 = 1200
  assert_eq(2048, storage_capacity_mb)  // 2 * 1024 = 2048
  assert_eq(1, storage_units_needed)  // ceil(1200/2048) = 1
}

test "enhanced_extreme_boundary_values" {
  // 极端边界值测试
  // 测试大数和小数的组合
  assert_eq(1000001, add(1000000, 1))
  assert_eq(999999, subtract(1000000, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
}

test "enhanced_multi_step_calculation" {
  // 多步计算测试
  let initial_value = 10
  
  // 步骤1：加法
  let step1 = add(initial_value, 15)  // 25
  
  // 步骤2：乘法
  let step2 = multiply(step1, 3)  // 75
  
  // 步骤3：除法向上取整
  let step3 = divide_with_ceil(step2, 20)  // 4
  
  // 步骤4：减法
  let step4 = subtract(step3, 2)  // 2
  
  // 步骤5：再次加法
  let final_result = add(step4, 8)  // 10
  
  assert_eq(10, final_result)
}

test "enhanced_special_character_greetings" {
  // 特殊字符问候测试
  assert_eq_string("Hello, spaces   !", greet("spaces   "))
  assert_eq_string("Hello, tab\tcharacter!", greet("tab\tcharacter"))
  assert_eq_string("Hello, newline\ncharacter!", greet("newline\ncharacter"))
  assert_eq_string("Hello, quote\"character!", greet("quote\"character"))
  assert_eq_string("Hello, emoji🚀character!", greet("emoji🚀character"))
}