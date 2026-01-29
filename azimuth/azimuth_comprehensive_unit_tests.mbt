// Azimuth 综合单元测试 - 新增10个标准 MoonBit 测试用例
// 测试主要功能的各个方面，包括边界情况、错误处理和实际应用场景

test "subtract_function_basic" {
  // 基本减法函数测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
}

test "multiply_and_add_combination" {
  // 乘法和加法组合测试
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "divide_with_ceil_packaging" {
  // 向上取整除法包装计算测试
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "complex_calculation_sequence" {
  // 复杂计算序列测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)          // 5
  assert_eq(5, step4)
}

test "resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let teams = 3
  let resources_per_team = divide_with_ceil(total_resources, teams)
  assert_eq(34, resources_per_team)  // ceil(100/3) = 34
}

test "financial_calculation" {
  // 金融计算测试
  let principal = 1000
  let rate = 5
  let interest = divide_with_ceil(multiply(principal, rate), 100)
  let total = add(principal, interest)
  assert_eq(1050, total)  // 1000 + 50 = 1050
}

test "inventory_management" {
  // 库存管理测试
  let products = 47
  let capacity = 12
  let boxes = divide_with_ceil(products, capacity)
  assert_eq(4, boxes)  // ceil(47/12) = 4
}

test "project_planning" {
  // 项目规划测试
  let tasks = 89
  let people = 5
  let days = 12
  let total_person_days = multiply(people, days)
  let tasks_per_day = divide_with_ceil(tasks, total_person_days)
  assert_eq(2, tasks_per_day)  // ceil(89/60) = 2
}

test "batch_processing" {
  // 批处理测试
  let records = 1000
  let batch_size = 75
  let batches = divide_with_ceil(records, batch_size)
  assert_eq(14, batches)  // ceil(1000/75) = 14
}

test "logistics_optimization" {
  // 物流优化测试
  let items = 156
  let truck_capacity = 20
  let trucks = divide_with_ceil(items, truck_capacity)
  assert_eq(8, trucks)  // ceil(156/20) = 8
}