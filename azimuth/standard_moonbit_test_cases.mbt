// 标准 MoonBit 测试用例
// 为 azimuth 库添加10个高质量的标准测试用例

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(1000, add(500, 500))
  assert_eq(1, add(0, 1))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零性质
  assert_eq(0, multiply(0, 123))
  assert_eq(0, multiply(456, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-100, 0))
  assert_eq(123, multiply(123, 1))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, Bonjour!", greet("Bonjour"))
  assert_eq_string("Hello, Hola!", greet("Hola"))
  assert_eq_string("Hello, こんにちは!", greet("こんにちは"))
  assert_eq_string("Hello, 안녕하세요!", greet("안녕하세요"))
  assert_eq_string("Hello, مرحبا!", greet("مرحبا"))
}

test "divide_with_ceil_practical_applications" {
  // 测试向上取整除法的实际应用
  // 场景：计算需要的房间数量
  let guests = 35
  let capacity_per_room = 8
  let rooms_needed = divide_with_ceil(guests, capacity_per_room)
  assert_eq(5, rooms_needed)
  
  // 验证总容量
  let total_capacity = multiply(rooms_needed, capacity_per_room)
  assert_eq(40, total_capacity)
}

test "subtract_function_basic_operations" {
  // 测试减法函数的基本操作
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(20, 5))
  assert_eq(-15, subtract(-10, 5))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目成本
  let base_cost = 1000
  let tax_rate = 20  // 20%
  let discount = 150
  
  // 计算税额
  let tax_amount = divide_with_ceil(multiply(base_cost, tax_rate), 100)
  assert_eq(200, tax_amount)
  
  // 计算小计
  let subtotal = add(base_cost, tax_amount)
  assert_eq(1200, subtotal)
  
  // 计算最终成本
  let final_cost = subtract(subtotal, discount)
  assert_eq(1050, final_cost)
}

test "mathematical_commutative_and_associative" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "resource_planning_scenario" {
  // 测试资源规划场景
  // 场景：计算项目资源需求
  let project_duration = 45  // 天
  let team_members = 6
  let work_hours_per_day = 8
  
  // 计算总工时
  let total_hours = multiply(project_duration, multiply(team_members, work_hours_per_day))
  assert_eq(2160, total_hours)
  
  // 计算需要的周数（每周工作5天）
  let weeks_needed = divide_with_ceil(project_duration, 5)
  assert_eq(9, weeks_needed)
  
  // 计算每周工时
  let weekly_hours = multiply(team_members, multiply(work_hours_per_day, 5))
  assert_eq(240, weekly_hours)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_business_calculation" {
  // 测试真实世界业务计算
  // 场景：计算订单配送
  let order_items = 127
  let items_per_box = 24
  let cost_per_box = 15
  let shipping_cost_per_box = 5
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(6, boxes_needed)
  
  // 计算箱子成本
  let box_cost = multiply(boxes_needed, cost_per_box)
  assert_eq(90, box_cost)
  
  // 计算运输成本
  let shipping_cost = multiply(boxes_needed, shipping_cost_per_box)
  assert_eq(30, shipping_cost)
  
  // 计算总成本
  let total_cost = add(box_cost, shipping_cost)
  assert_eq(120, total_cost)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(144, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, order_items)
  assert_eq(17, remaining_space)
}