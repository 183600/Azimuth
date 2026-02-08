// Azimuth 项目新增测试套件
// 包含 8 个高质量 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖多种实际应用场景

test "basic_financial_calculation" {
  // 基本金融计算测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算简单利息
  let interest = divide_with_ceil(multiply(principal, rate), 100)
  let total = add(principal, interest)
  
  assert_eq(500, interest)
  assert_eq(10500, total)
}

test "inventory_management" {
  // 库存管理测试
  let items_per_box = 24
  let total_items = 100
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let empty_space = subtract(total_capacity, total_items)
  
  assert_eq(5, boxes_needed)
  assert_eq(120, total_capacity)
  assert_eq(20, empty_space)
}

test "array_operations" {
  // 数组操作模拟测试
  let array_length = 10
  let element_size = 4
  
  let total_size = multiply(array_length, element_size)
  let middle_index = divide_with_ceil(array_length, 2)
  let middle_offset = multiply(middle_index, element_size)
  
  assert_eq(40, total_size)
  assert_eq(5, middle_index)
  assert_eq(20, middle_offset)
}

test "time_calculation" {
  // 时间计算测试
  let hours = 8
  let minutes_per_hour = 60
  let total_minutes = multiply(hours, minutes_per_hour)
  
  let seconds_per_minute = 60
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  
  assert_eq(480, total_minutes)
  assert_eq(28800, total_seconds)
}

test "resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let projects = 3
  let base_allocation = divide_with_ceil(total_resources, projects)
  
  let remaining = subtract(total_resources, multiply(base_allocation, projects))
  let bonus_allocation = add(base_allocation, remaining)
  
  assert_eq(34, base_allocation)
  assert_eq(-2, remaining)
  assert_eq(32, bonus_allocation)
}

test "coordinate_geometry" {
  // 坐标几何测试
  let x1 = 2
  let y1 = 3
  let x2 = 8
  let y2 = 9
  
  let dx = subtract(x2, x1)
  let dy = subtract(y2, y1)
  let distance_squared = add(multiply(dx, dx), multiply(dy, dy))
  
  assert_eq(6, dx)
  assert_eq(6, dy)
  assert_eq(72, distance_squared)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 25
  let result = divide_with_ceil(multiply(total, percentage), 100)
  
  let increased_total = add(total, result)
  let decreased_total = subtract(total, result)
  
  assert_eq(50, result)
  assert_eq(250, increased_total)
  assert_eq(150, decreased_total)
}

test "batch_processing" {
  // 批处理测试
  let batch_size = 50
  let total_items = 225
  let full_batches = divide_with_ceil(total_items, batch_size)
  
  let processed_items = multiply(full_batches, batch_size)
  let overflow = subtract(processed_items, total_items)
  
  assert_eq(5, full_batches)
  assert_eq(250, processed_items)
  assert_eq(25, overflow)
}