// 简化的高级测试用例 - 5个高质量测试
// 使用标准 MoonBit 测试语法

test "mathematical_identities" {
  // 数学恒等式验证
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_calculations" {
  // 业务计算：项目资源分配
  let total_tasks = 47
  let team_capacity = 8
  let sprints_needed = divide_with_ceil(total_tasks, team_capacity)
  
  assert_eq(6, sprints_needed)
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, team_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  
  assert_eq(48, total_capacity)
  assert_eq(1, unused_capacity)
}

test "string_processing" {
  // 字符串处理测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 123456!", greet("123456"))
}

test "negative_arithmetic" {
  // 负数算术运算
  let a = -15
  let b = 8
  
  assert_eq(-7, add(a, b))
  assert_eq(-120, multiply(a, b))
  assert_eq(-23, subtract(a, b))
  assert_eq(-2, divide_with_ceil(-15, 8))
}

test "complex_workflow" {
  // 复杂工作流：电商订单包装
  let order_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总重量
  let weight_per_item = 2
  let total_weight = multiply(order_items, weight_per_item)
  let avg_weight_per_box = divide_with_ceil(total_weight, boxes_needed)
  
  assert_eq(46, total_weight)
  assert_eq(12, avg_weight_per_box)
}